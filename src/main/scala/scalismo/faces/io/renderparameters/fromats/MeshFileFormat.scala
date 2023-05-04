package scalismo.faces.io.renderparameters.fromats

import scalismo.faces.parameters.MeshFile

import scala.util.{Failure, Success, Try}

import java.net.URI

object MeshFileFormat {

  import RenderParameterJSONFormats.uriMapper

  implicit val meshFileMapper: upickle.default.ReadWriter[MeshFile] = upickle.default
    .readwriter[ujson.Value]
    .bimap(
      muri =>
        ujson.Obj(
          "meshURI" -> muri.toString,
          "@type" -> "MeshFile"
        ),
      v =>
        Try {
          v("@type").str
        } match {
          case Success(typeName) =>
            if (typeName != "MeshFile")
              throw new IllegalArgumentException("Expected MeshFile entry")
            MeshFile(
              meshURI = new URI(v("meshURI").str)
            )
          case Failure(e) =>
            throw new IllegalArgumentException("Expected field with the stored type.")
        }
    )
}
