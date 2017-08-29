package controllers


import javax.inject.Inject

import dal.GpxRep
import models.{Gpx, Materiel}
import org.joda.time.DateTime
import play.Environment
import play.api.mvc.{Action, Controller, Flash}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.data.Form
import play.api.data.Forms._

import scala.concurrent.ExecutionContext

case class GpxForm(id: Long, idTrek: Long, titre: String, sousTitre: String, description: String, listeMatos: String, nomFichier: String, altitudeMinimum: Int, altitudeMaximum: Int, ascensionTotale: Int, descenteTotale: Int,
                   heureDebut: String, heureFin: String, distanceTotale: Int, depart: String,
                   arrivee: String, coordonneesPix: String, typegpx: String, listeFichiers: Seq[String])


class GpxController @Inject() (repo: GpxRep, val messagesApi: MessagesApi)
                                (implicit ec: ExecutionContext) extends Controller with I18nSupport with UtilUpload {

  val formGpx = Form(
    mapping(
      "id" -> longNumber,
      "idTrek" -> longNumber,
      "titre" -> nonEmptyText,
      "sousTitre" -> nonEmptyText,
      "description" -> text,
      "listeMatos" -> text,
      "nomFichier" -> text,
      "altitudeMinimum" -> number,
      "altitudeMaximum" -> number,
      "ascensionTotale" -> number,
      "descenteTotale" -> number,
      "heureDebut" -> text,
      "heureFin" -> text,
      "distanceTotale" -> number,
      "depart" -> text,
      "arrivee" -> text,
      "coordonneesPix" -> text,
      "typegpx" -> text,
      "listeFichiers" -> seq(text)
    )(GpxForm.apply)(GpxForm.unapply)
  )

  def listGpx(typegpx: String) = Action { implicit request =>
    val gpxs = if(typegpx == "T")
                  repo.listAllT.sortWith(_.heureDebut > _.heureDebut)
               else
                  repo.listAllR.sortWith(_.heureDebut > _.heureDebut)
    Ok(views.html.gpx.list(gpxs, typegpx))
  }

  def listGpxTrk(id: Long) = Action { implicit request =>
    val gpxs = repo.listByTrekId(id).sortWith(_.heureDebut < _.heureDebut)
    Ok(views.html.gpx.list(gpxs, "R"))
  }

  def showGpx(id: Long) = Action { implicit request =>
    repo.get(id).map { gpx =>
      new DateTime(gpx.heureDebut)
      Ok(views.html.gpx.details(gpx))
    }.getOrElse(NotFound)
  }

  def show = Action { implicit request =>
    //Gpx.fusion(Seq("public/gpx/etape1.GPX",
    //  "public/gpx/etape2.GPX",
    //  "public/gpx/etape3.GPX",
    //  "public/gpx/etape4.GPX",
    //  "public/gpx/etape5.GPX",
    //  "public/gpx/etape6.GPX",
    //  "public/gpx/etape7.GPX",
    //  "public/gpx/etape8.GPX"),
    //  "public/gpx/senda2017.gpx")
    val gpx = Gpx.creer("T",0L, 0L, "Titre", "Sous-titre", "Description de la rando", null, "senda2017.gpx")
    Ok(views.html.gpx.details(gpx))
  }

  def deleteGpx(id: Long) = Action { implicit request =>
    if(!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.showGpx(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      repo.removeById(id)
      Redirect(routes.GpxController.listGpx("R")).flashing("success" -> "Tracé supprimé")
    }
  }

  def newGpx(typegpx: String) = Action { implicit request =>
    if(!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.listGpx("R")).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      var listeFichiers = Seq[(Long, String, Long)]()
      val form = if (request.flash.get("error").isDefined) {
        val errorForm = this.formGpx.bind(request.flash.data)
        errorForm
      } else {
        this.formGpx.fill(GpxForm(0L, 0L, "", "", "", "", "", 0, 0, 0, 0, "", "", 0, "", "", "", typegpx,
            listeFichiers.map(f => f._1.toString + "," + f._2)))
      }
      if(typegpx == "R") {
        val liste = ("" +: new java.io.File("%s/contenu/gpx/randos/".
          format(Environment.simple.rootPath.getCanonicalPath)).
          listFiles.map(f => f.getName).filter(_.toLowerCase.endsWith(".gpx")).
          toSeq).diff(repo.listGpxFiles)
        listeFichiers = liste.map((0L, _, 0L))
      } else {
        listeFichiers = repo.listCandidatTrek(0L).sortWith(_._2 < _._2)
      }
      Ok(views.html.gpx.edit(form, listeCandidats = listeFichiers))
    }
  }

  def saveGpx = Action(parse.multipartFormData) { implicit request =>
    //au retour, nomFichier contient gpx.nomFichier
    //           listeFichier le ou  les fichiers candidats du serveur
    //           monFichier le fichier local
    val typegpx = request.body.dataParts("typegpx").head
    if (!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.listGpx(typegpx)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val newForm = this.formGpx.bindFromRequest()
      newForm.fold(
        hasErrors = { form =>
          Redirect(routes.GpxController.newGpx(newForm.data("typegpx"))).flashing(Flash(form.data) +
            ("error" -> "Erreur de saisie"))
        },
        success = { newGpx =>
          var idx = 0L
          if(newForm.data("typegpx") == "R") {
            val nomFichier = uploadFile("gpx", newForm.data("listeFichiers"), request)
            if (nomFichier == "") {
              idx = repo.add(Gpx(newGpx.id, newGpx.idTrek, newGpx.titre, newGpx.sousTitre, newGpx.description, newGpx.listeMatos,
                nomFichier, newGpx.altitudeMinimum, newGpx.altitudeMaximum, newGpx.ascensionTotale, newGpx.descenteTotale,
                newGpx.heureDebut, newGpx.heureFin, newGpx.distanceTotale, newGpx.depart,
                newGpx.arrivee, newGpx.coordonneesPix, newGpx.typegpx))
            } else {
              idx = repo.add(Gpx.creer("R", newGpx.id, newGpx.idTrek, newGpx.titre, newGpx.sousTitre,
                newGpx.description, newGpx.listeMatos, nomFichier))
            }
          } else {
            val nextIndex = repo.lastValueIndex + 1
            val listeFichiers = for(f <- newForm.get.listeFichiers) yield {
              val sub = f.replaceAll("[\\(\\)]","").split(",")
              (sub(0),sub(1))
            }
            Gpx.fusion(listeFichiers.map(_._2), nextIndex.toString + ".gpx")
            idx = repo.add(Gpx.creer("T", newGpx.id, newGpx.idTrek, newGpx.titre, newGpx.sousTitre,
              newGpx.description, newGpx.listeMatos, nextIndex.toString + ".gpx"))
            repo.rattacheGpx(idx, listeFichiers.map(_._1.toLong))
          }
          Redirect(routes.GpxController.showGpx(idx))
            .flashing("success" -> ("Création réussie du tracé " + idx.toString))
        }
      )
    }
  }

  def editGpx(id: Long) = Action { implicit request =>
    if (!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.showGpx(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      var listeFichiers = Seq[(Long, String, Long)]()
      repo.get(id).map { gpx =>
        if(gpx.typegpx == "R") {
          val liste = ("" +: new java.io.File("%s/contenu/gpx/randos/".
            format(Environment.simple.rootPath.getCanonicalPath))
            .listFiles.map(f => f.getName).filter(_.toLowerCase.endsWith(".gpx")).toSeq).diff(repo.listGpxFiles)
          listeFichiers = liste.map((0L, _, 0L))
        } else {
          listeFichiers = repo.listCandidatTrek(gpx.id).sortWith(_._2 < _._2)
        }
        val gpxForm = GpxForm(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description,
          gpx.listeMatos, gpx.nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale,
          gpx.descenteTotale, gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart, gpx.arrivee,
          gpx.coordonneesPix, gpx.typegpx, Seq())
        Ok(views.html.gpx.edit(this.formGpx.fill(gpxForm), Option(id), listeCandidats = listeFichiers))
      }.getOrElse(NotFound)
    }
  }

  def updateGpx(id: Long) = Action(parse.multipartFormData) { implicit request =>
    if (!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.showGpx(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val newForm = this.formGpx.bindFromRequest()
      newForm.fold(
        hasErrors = { form =>
          Ok(views.html.gpx.edit(form, Option(id))).flashing(Flash(form.data) +
            ("error" -> "Erreur de saisie"))
        },
        success = { gpx =>
          if(newForm.data("typegpx") == "R") {
            val nomFichier = uploadFile("gpx", newForm.data("listeFichiers"), request)
            if (repo.get(id).get.nomFichier == nomFichier || nomFichier.isEmpty)
              repo.update(Gpx(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos,
                nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale, gpx.descenteTotale,
                gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart,
                gpx.arrivee, gpx.coordonneesPix, gpx.typegpx))
            else {
              repo.update(Gpx.creer("R", gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos, nomFichier))
            }
          } else {
            val listeFichiers = for(f <- newForm.get.listeFichiers) yield {
              val sub = f.replaceAll("[\\(\\)]","").split(",")
              (sub(0),sub(1))
            }
            val listeFic = listeFichiers.map(_._2)
            val listeAExclure = repo.listCandidatTrek(id).filter(_._3 == id).map(_._2)
            if(listeFic.size == listeAExclure && listeFic.diff(listeAExclure).isEmpty) {
              repo.update(Gpx(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos,
                gpx.nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale, gpx.descenteTotale,
                gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart,
                gpx.arrivee, gpx.coordonneesPix, gpx.typegpx))
            } else {
              Gpx.fusion(listeFichiers.map(_._2), id.toString + ".gpx")
              repo.update(Gpx.creer("T", gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre,
                gpx.description, gpx.listeMatos, id.toString + ".gpx"))
              repo.rattacheGpx(id, listeFichiers.map(_._1.toLong))
            }
          }
          Redirect(routes.GpxController.showGpx(gpx.id))
              .flashing("success" -> ("Modification réussie du tracé " + id.toString))
        }
      )
    }
  }
}
