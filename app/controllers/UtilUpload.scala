package controllers

import java.io.File

import play.api.libs.Files
import play.api.mvc.{MultipartFormData, Request}

trait UtilUpload {

  def uploadFile(item: String, fichier: String, request: Request[MultipartFormData[Files.TemporaryFile]]) : String = {
    val nomFichier = request.body.file("monFichier").map { monFichier =>
      if (monFichier.filename.isEmpty) {
        fichier
      } else {
        val racine = item match {
          case "matos" => "images/matos"
          case "gpx" => "gpx/randos"
          case _ => ""
        }
        val contentType = monFichier.contentType
        monFichier.ref.moveTo(new File("%s/public/%s/%s"
          .format(System.getenv("PWD"), racine, monFichier.filename)))
        monFichier.filename
      }
    }.getOrElse(fichier)
    nomFichier
  }
}
