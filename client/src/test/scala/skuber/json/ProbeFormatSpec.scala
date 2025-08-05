package skuber.json

import org.specs2.mutable.Specification
import play.api.libs.json._
import skuber._
import skuber.json.format._

/**
  * @author David O'Riordan
  */
class ProbeFormatSpec extends Specification {
  "This is a unit specification for the skuber Prob related json formatter.\n ".txt

  "read probe json - GRPC" >> {
    val probeJsonStr =
      """
      {
        "grpc": {
          "port": 8090
        },
        "initialDelaySeconds": 30,
        "timeoutSeconds": 5
      }
      """
    val actualProbe = Json.parse(probeJsonStr).as[Probe]
    val expectedProbe = Probe(
      action = GRPCAction(8090),
      initialDelaySeconds = 30,
      timeoutSeconds = 5,
    )
    actualProbe mustEqual expectedProbe
  }

  "read probe json - ExecCommand" >> {
    val probeJsonStr =
      """
      {
        "exec": {
          "command": [
            "/usr/local/bin/etcd",
            "-listen-client-urls"
          ]
        },
        "initialDelaySeconds": 30,
        "timeoutSeconds": 5
      }
      """
    val actualProbe = Json.parse(probeJsonStr).as[Probe]
    val expectedProbe = Probe(
      action = ExecAction(
        command = List(
          "/usr/local/bin/etcd",
          "-listen-client-urls"
        )
      ),
      initialDelaySeconds = 30,
      timeoutSeconds = 5,
    )
    actualProbe mustEqual expectedProbe
  }

  "read probe json - ExecCommand" >> {
    val probeJsonStr =
      """
      {
        "exec": {
          "command": [
            "/usr/local/bin/etcd",
            "-listen-client-urls"
          ]
        },
        "initialDelaySeconds": 30,
        "timeoutSeconds": 5
      }
      """
    val actualProbe = Json.parse(probeJsonStr).as[Probe]
    val expectedProbe = Probe(
      action = ExecAction(
        command = List(
          "/usr/local/bin/etcd",
          "-listen-client-urls"
        )
      ),
      initialDelaySeconds = 30,
      timeoutSeconds = 5,
    )
    actualProbe mustEqual expectedProbe
  }

  "read probe json - HTTPGetAction with integer port" >> {
    val probeJsonStr =
      """
      {
        "httpGet": {
          "port": 8080,
          "host": "localhost",
          "path": "/healthz",
          "schema": "HTTP"
        },
        "initialDelaySeconds": 10,
        "timeoutSeconds": 2
      }
    """
    val actualProbe = Json.parse(probeJsonStr).as[Probe]
    val expectedProbe = Probe(
      action = HTTPGetAction(
        port = Left(8080),
        host = "localhost",
        path = "/healthz",
        schema = "HTTP"
      ),
      initialDelaySeconds = 10,
      timeoutSeconds = 2
    )
    actualProbe mustEqual expectedProbe
  }

  "read probe json - TCPSocketAction" >> {
    val probeJsonStr =
      """
      {
        "tcpSocket": {
          "port": 1234
        },
        "initialDelaySeconds": 5,
        "timeoutSeconds": 1
      }
    """
    val actualProbe = Json.parse(probeJsonStr).as[Probe]
    val expectedProbe = Probe(
      action = TCPSocketAction(
        port = Left(1234)
      ),
      initialDelaySeconds = 5,
      timeoutSeconds = 1
    )
    actualProbe mustEqual expectedProbe
  }

  "read probe json - UnknownHandler fallback" >> {
    val probeJsonStr =
      """
      {
        "someOtherField": {
          "value": 1
        },
        "initialDelaySeconds": 2,
        "timeoutSeconds": 1
      }
    """
    val actualProbe = Json.parse(probeJsonStr).as[Probe]
    val expectedProbe = Probe(
      action = UnknownHandler,
      initialDelaySeconds = 2,
      timeoutSeconds = 1
    )
    actualProbe mustEqual expectedProbe
  }


}
