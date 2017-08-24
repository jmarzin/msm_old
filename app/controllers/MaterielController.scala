package controllers

import java.io.File
import javax.inject.Inject

import dal.MaterielRep
import models.Materiel
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.Files

import scala.concurrent.ExecutionContext

class MaterielController @Inject() (repo: MaterielRep, val messagesApi: MessagesApi)
                                   (implicit ec: ExecutionContext) extends Controller with I18nSupport with UtilUpload {
  val formMateriel = Form(
      mapping(
        "id" -> longNumber,
        "nom" -> nonEmptyText,
        "description" -> nonEmptyText,
        "photo" -> nonEmptyText,
        "poids" -> number(min = 1)
      )(Materiel.apply)(Materiel.unapply)
    )

  def listMateriel = Action { implicit request =>
    val materiels = repo.listAll.sortWith(_.poids > _.poids)
    Ok(views.html.materiels.list(materiels))
  }

  def showMateriel(id: Long) = Action { implicit request =>
    repo.get(id).map { materiel =>
      Ok(views.html.materiels.details(materiel))
    }.getOrElse(NotFound)
  }

  def deleteMateriel(id: Long) = Action { implicit request =>
    if(!request.session.get("admin").isDefined) {
      Redirect(routes.MaterielController.showMateriel(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      repo.removeById(id)
      Redirect(routes.MaterielController.listMateriel()).flashing("success" -> "Mot supprimé")
    }
  }

  def newMateriel = Action { implicit request =>
    if(!request.session.get("admin").isDefined) {
      Redirect(routes.MaterielController.listMateriel()).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val form = if (request.flash.get("error").isDefined) {
        val errorForm = this.formMateriel.bind(request.flash.data)
        errorForm
      } else {
        this.formMateriel.fill(Materiel(0L, "", "", "0pasdimage.jpg", 0))
      }
      Ok(views.html.materiels.edit(form))
    }
  }

  def saveMateriel = Action(parse.multipartFormData) { implicit request =>
    if (!request.session.get("admin").isDefined) {
      Redirect(routes.MaterielController.listMateriel()).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val newForm = this.formMateriel.bindFromRequest()
      val nomImage = uploadFile("matos", newForm.data("photo"), request)
      newForm.fold(
        hasErrors = { form =>
          Redirect(routes.MaterielController.newMateriel).flashing(Flash(form.data) +
            ("error" -> "Erreur de saisie"))
        },
        success = { newMateriel =>
          val id = repo.add(Materiel(newMateriel.id, newMateriel.nom, newMateriel.description, nomImage, newMateriel.poids))
          Redirect(routes.MaterielController.showMateriel(id))
            .flashing("success" -> ("Création réussie du matériel " + id.toString))
        }
      )
    }
  }

  def editMateriel(id: Long) = Action { implicit request =>
    if (!request.session.get("admin").isDefined) {
      Redirect(routes.MaterielController.showMateriel(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      repo.get(id).map { materiel =>
        Ok(views.html.materiels.edit(this.formMateriel.fill(materiel), Option(id)))
      }.getOrElse(NotFound)
    }
  }

  def updateMateriel(id: Long) = Action(parse.multipartFormData) { implicit request =>
    if (!request.session.get("admin").isDefined) {
      Redirect(routes.MaterielController.showMateriel(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      val newForm = this.formMateriel.bindFromRequest()
      val nomImage = uploadFile("matos", newForm.data("photo"), request)
      newForm.fold(
        hasErrors = { form =>
          Ok(views.html.materiels.edit(form, Option(id))).flashing(Flash(form.data) +
            ("error" -> "Erreur de saisie"))
        },
        success = { materiel =>
          repo.update(Materiel(materiel.id, materiel.nom, materiel.description, nomImage, materiel.poids))
          Redirect(routes.MaterielController.showMateriel(materiel.id))
            .flashing("success" -> ("Modification réussie du matériel " + id.toString))
        }
      )
    }
  }
}
