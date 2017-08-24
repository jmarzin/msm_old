package controllers


import javax.inject.Inject

import dal.GpxRep
import models.{Gpx, Materiel}
import org.joda.time.DateTime
import play.api.mvc.{Action, Controller, Flash}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.data.Form
import play.api.data.Forms._

import scala.concurrent.ExecutionContext

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
      "typegpx" -> text
    )(Gpx.apply)(Gpx.unapply)
  )

  def listGpx(typegpx: String) = Action { implicit request =>
    val gpxs = if(typegpx == "T")
                  repo.listAllT.sortWith(_.heureDebut > _.heureDebut)
               else
                  repo.listAllR.sortWith(_.heureDebut > _.heureDebut)
    Ok(views.html.gpx.list(gpxs))
  }

  def listGpxTrk(id: Long) = Action { implicit request =>
    val gpxs = repo.listByTrekId(id).sortWith(_.heureDebut < _.heureDebut)
    Ok(views.html.gpx.list(gpxs))
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
    val gpx = Gpx.creer(0L, 0L, "Titre", "Sous-titre", "Description de la rando", null, "senda2017.gpx")
    Ok(views.html.gpx.details(gpx))
  }

  def deleteGpx(id: Long) = Action { implicit request =>
    if(!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.showGpx(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      repo.removeById(id)
      Redirect(routes.GpxController.listGpx("R")).flashing("success" -> "Mot supprimé")
    }
  }

  def newGpx = Action { implicit request =>
    if(!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.listGpx("R")).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val form = if (request.flash.get("error").isDefined) {
        val errorForm = this.formGpx.bind(request.flash.data)
        errorForm
      } else {
        this.formGpx.fill(Gpx(0L, 0L, "", "", "", "", "", 0, 0, 0, 0, "", "", 0, "", "", "","R"))
      }
      Ok(views.html.gpx.edit(form))
    }
  }

  def saveGpx = Action(parse.multipartFormData) { implicit request =>
    if (!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.listGpx("R")).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val newForm = this.formGpx.bindFromRequest()
      val nomFichier = uploadFile("gpx", newForm.data("nomFichier"), request)
      newForm.fold(
        hasErrors = { form =>
          Redirect(routes.GpxController.newGpx).flashing(Flash(form.data) +
            ("error" -> "Erreur de saisie"))
        },
        success = { newGpx =>
          var idx = 0L
          if(nomFichier == "") {
            idx = repo.add(Gpx(newGpx.id, newGpx.idTrek, newGpx.titre, newGpx.sousTitre, newGpx.description, newGpx.listeMatos,
              nomFichier, newGpx.altitudeMinimum, newGpx.altitudeMaximum, newGpx.ascensionTotale, newGpx.descenteTotale,
              newGpx.heureDebut, newGpx.heureFin, newGpx.distanceTotale, newGpx.depart,
              newGpx.arrivee, newGpx.coordonneesPix,newGpx.typegpx))
          } else {
            idx = repo.add(Gpx.creer(newGpx.id, newGpx.idTrek, newGpx.titre, newGpx.sousTitre, newGpx.description, newGpx.listeMatos, nomFichier))
          }
          Redirect(routes.GpxController.showGpx(idx))
            .flashing("success" -> ("Création réussie du tracé Gpx " + idx.toString))
        }
      )
    }
  }

  def editGpx(id: Long) = Action { implicit request =>
    if (!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.showGpx(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      repo.get(id).map { gpx =>
        Ok(views.html.gpx.edit(this.formGpx.fill(gpx), Option(id)))
      }.getOrElse(NotFound)
    }
  }

  def updateGpx(id: Long) = Action(parse.multipartFormData) { implicit request =>
    if (!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.showGpx(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val newForm = this.formGpx.bindFromRequest()
      val nomFichier = uploadFile("gpx", newForm.data("nomFichier"), request)
      newForm.fold(
        hasErrors = { form =>
          Ok(views.html.gpx.edit(form, Option(id))).flashing(Flash(form.data) +
            ("error" -> "Erreur de saisie"))
        },
        success = { gpx =>
          if(repo.get(id).get.nomFichier == nomFichier || nomFichier.isEmpty)
            repo.update(Gpx(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos,
              nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale, gpx.descenteTotale,
              gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart,
              gpx.arrivee, gpx.coordonneesPix, gpx.typegpx))
          else {
            repo.update(Gpx.creer(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos, nomFichier))
          }
          Redirect(routes.GpxController.showGpx(gpx.id))
              .flashing("success" -> ("Modification réussie du matériel " + id.toString))
        }
      )
    }
  }
}
