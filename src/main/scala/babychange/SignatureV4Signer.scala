package babychange

import java.security.MessageDigest
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import akka.NotUsed
import akka.actor.{ActorLogging, ActorSystem, Props, Status}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpEntity.Strict
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{DateTime, HttpCharsets, HttpEntity, HttpRequest}
import akka.pattern.pipe
import akka.stream.actor.ActorPublisher
import akka.stream.actor.ActorPublisherMessage.{Cancel, Request}
import akka.stream.scaladsl.{Flow, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.util.{ByteString, Timeout}
import babychange.Ec2CredentialsRetriever.UpdateCredentials
import babychange.SignatureV4Signer.AwsCredentials
import spray.json._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object SignatureV4Signer {
  case class AwsCredentials(accessKeyId: String, secretAccessKey: String, token: Option[String])

  def basic(accessKeyId: String, secretAccessKey: String) = new SignatureV4Signer {
    protected val credentials = Source.repeat(AwsCredentials(accessKeyId, secretAccessKey, None))
  }

  def environmentVariables() =
    basic(sys.env("AWS_ACCESS_KEY_ID"), sys.env("AWS_SECRET_ACCESS_KEY"))

  def ec2Instance(roleName: String)(implicit system: ActorSystem) = new Ec2Signer(roleName, system)
}

class Ec2Signer(val roleName: String, val system: ActorSystem) extends SignatureV4Signer {
  implicit val timeout = Timeout(system.settings.config.getDuration("sigv4.ec2.readTimeout").toMillis.millis)
  protected val credentials = Source.actorPublisher[AwsCredentials](Props(classOf[Ec2CredentialsRetriever], roleName))
}

object Ec2CredentialsRetriever {
  private case object UpdateCredentials
}
class Ec2CredentialsRetriever(roleName: String) extends ActorPublisher[AwsCredentials] with ActorLogging {

  implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(context.system))

  val readTimeout = context.system.settings.config.getDuration("sigv4.ec2.readTimeout").toMillis.millis
  val updateInterval = context.system.settings.config.getDuration("sigv4.ec2.updateInterval").toMillis.millis

  var currentCredentials = Option.empty[AwsCredentials]

  context.system.scheduler.schedule(0.seconds, updateInterval, self, UpdateCredentials)

  implicit def ec: ExecutionContext = context.system.dispatcher

  override def receive: Receive = {
    case Request(_) =>
      publish()

    case UpdateCredentials =>
      requestCredentials()

    case HttpEntity.Strict(_, data) =>
      extractCredentials(data) match {
        case Success(creds) =>
          currentCredentials = Some(creds)
          publish()
        case Failure(e) =>
          updateCredentialsError(e)
      }

    case Status.Failure(e) =>
      updateCredentialsError(e)

    case Cancel =>
      context.stop(self)
  }

  def publish(): Unit = if(totalDemand > 0) {
    currentCredentials match {
      case Some(creds) =>
        (1l to totalDemand).foreach(i => onNext(creds))
      case _ =>
        () // We don't have the credentials yet... We'll send them when we get them.
    }
  }

  def updateCredentialsError(t: Throwable): Unit = {
    if(currentCredentials.isEmpty) {
      log.error(t, "Unable to update EC2 Credentials for role {}. Cache is empty, so retrying in 1 minute", roleName)
      context.system.scheduler.scheduleOnce(1.minute, self, UpdateCredentials)
    }
    else {
      log.error(t, "Unable to update EC2 Credentials for role {}", roleName)
    }
  }

  def requestCredentials(): Unit = {
    val request = HttpRequest(uri = "http://169.254.169.254/latest/meta-data/iam/security-credentials/" + roleName)
    val responseEntity = Http(context.system).singleRequest(request).flatMap(_.entity.toStrict(readTimeout))
    responseEntity pipeTo self
  }

  def extractCredentials(data: ByteString): Try[AwsCredentials] = Try {
    val json = data.decodeString("UTF-8").parseJson.asJsObject
    val JsString(accessKeyId) = json.fields("AccessKeyId")
    val JsString(secretAccessKey) = json.fields("SecretAccessKey")
    val token = json.fields.get("Token").collect { case JsString(t) => t }
    AwsCredentials(accessKeyId, secretAccessKey, token)
  }
}

trait SignatureV4Signer {

  protected def credentials: Source[AwsCredentials, Any]

  def signFlow: Flow[HttpRequest, HttpRequest, NotUsed] = Flow[HttpRequest].zip(credentials).map {
    case (request, creds) => sign(request, creds)
  }

  protected def sign(rBefore: HttpRequest, creds: AwsCredentials): HttpRequest = {

    val AwsCredentials(accessKeyId, secretAccessKey, token) = creds

    val r = rBefore.copy(headers = rBefore.headers :+ Host(rBefore.uri.authority.host.address()))

    val now = DateTime(currentTimeMillis)
    val auth = authHeader(r, now, accessKeyId, secretAccessKey)

    val maybeTokenHeader = token.map(t => RawHeader("X-Amz-Security-Token", t))
    val otherHeaders =
      auth +:
      r.headers :+
      RawHeader("X-Amz-Date", now.toIsoDateTimeString().replace("-", "").replace(":", "") + "Z") //TODO: remove replace

    r.copy(headers = otherHeaders ++ maybeTokenHeader)
  }

  private def authHeader(r: HttpRequest, date: DateTime, accessKeyId: String, secretAccessKey: String): Authorization = {
    val content = r.entity match {
      case Strict(ct, data) =>
        val charset = ct.charsetOption.getOrElse(HttpCharsets.`UTF-8`)
        data.utf8String.getBytes(charset.value)
      case _ => Array.emptyByteArray
    }

    val signedHeaders = r.headers.sortBy(_.name()).map(_.lowercaseName()).mkString(";")

    val canonicalRequest =
      r.method.value + "\n" +
      r.uri.path + "\n" +
      r.uri.query().sortBy(_._1).toString().replaceAll("&", "&\n") + "\n" + // TODO: Make better
      r.headers.sortBy(_.name()).map(h => h.lowercaseName() + ":" + h.value() + "\n").mkString + "\n" +
      signedHeaders + "\n" +
      hex(sha256(content))

    println("CR")
    println(canonicalRequest)

    val dateString = date.toIsoDateString().replace("-", "") // TODO: Fix replace
    val credentialScope = dateString + "/eu-west-1/es/aws4_request" // TODO: Fix hardcoded region+service

    val stringToSign =
      "AWS4-HMAC-SHA256\n" +
      date.toIsoDateTimeString().replace("-", "").replace(":", "") + "Z\n" + // TODO: Fix replace
      credentialScope + "\n" +
      hex(sha256(canonicalRequest.getBytes("UTF-8")))

    println("STS")
    println(stringToSign)

    val kDate = hmacSha256(dateString, ("AWS4" + secretAccessKey).getBytes("UTF-8"))
    val kRegion = hmacSha256("eu-west-1", kDate)
    val kService = hmacSha256("es", kRegion)
    val kSigning = hmacSha256("aws4_request", kService)

    val signature = hex(hmacSha256(stringToSign, kSigning))

    Authorization(GenericHttpCredentials("AWS4-HMAC-SHA256", s"Credential=$accessKeyId/$credentialScope, SignedHeaders=$signedHeaders, Signature=$signature"))
  }

  private def hex(bytes: Array[Byte]): String = {
    bytes.map("%02X" format _).mkString.toLowerCase
  }

  private def sha256(bytes: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-256")
    md.digest(bytes)
  }

  private def hmacSha256(str: String, secret: Array[Byte]): Array[Byte] = hmacSha256(str.getBytes("UTF-8"), secret)

  private def hmacSha256(bytes: Array[Byte], secret: Array[Byte]): Array[Byte] = {
    val signingKey = new SecretKeySpec(secret, "HmacSHA256")
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(signingKey)
    mac.doFinal(bytes)
  }

  def currentTimeMillis = System.currentTimeMillis

}
