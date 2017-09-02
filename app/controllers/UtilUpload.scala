package controllers

import java.io.File
import java.nio.file.attribute.PosixFilePermissions

import sys.process._
import play.Environment
import play.api.libs.Files
import play.api.mvc.{MultipartFormData, Request}

import scala.reflect.io.{File, Path}

trait UtilUpload {

  def uploadFile(item: String, fichier: String, request: Request[MultipartFormData[Files.TemporaryFile]]) : Option[String] = {
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
        val fichierServeur = new java.io.File("%s/contenu/%s/%s".
          format(Environment.simple.rootPath.getCanonicalPath, racine, monFichier.filename.replaceAll(" ","_")))
        monFichier.ref.moveTo(fichierServeur, true)
        fichierServeur.setReadable(true, false)
        fichierServeur.setWritable(true, false)
        if(scala.reflect.io.File(Path("%s/public".format(Environment.simple().rootPath().getCanonicalPath))).exists) {
          val origine = fichierServeur.getAbsolutePath
          val destination = origine.replaceAll("/contenu/", "/public/contenu/")
          val commande  = "cp %s %s".format(origine, destination)
          commande !
        }
        monFichier.filename.replaceAll(" ","_")
      }
    }.getOrElse(fichier)
    if(nomFichier.isEmpty) {
      None
    } else {
      Some(nomFichier)
    }
  }
}
