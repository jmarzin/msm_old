package models

import play.api.libs.json._
import play.twirl.api.Html

case class Materiel(id: Long, nom: String, description: String, photo: String, poids: Int) {
  def descriptionCourte: String = {
    val desc = this.description.replaceAll("<.*?>","")
      .replaceAll("&eacute;","é")
      .replaceAll("&agrave;","à")
      .replaceAll("@acirc;","â")
      .replaceAll("&egrave;","è")
      .replaceAll("&ecirc;","ê")
      .replaceAll("&ocirc;","ô")
      .replaceAll("&ugrave;","ù")
      .replaceAll("&ucirc;","û")
    val coupure = desc.indexOf(" ", 120)
    if(coupure == -1) {
      return desc
    }
    desc.substring(0, coupure) + " ..."
  }
}

object Materiel {

  implicit val materielFormat = Json.format[Materiel]
}