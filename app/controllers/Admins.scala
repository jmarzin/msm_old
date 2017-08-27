package controllers

import java.io.File

import akka.util.Crypt._
import models.Admin
import play.api.data.Form
import play.api.data.Forms.{ignored, mapping, nonEmptyText, single}
import play.api.i18n.Messages
import play.api.mvc.{Action, Controller, Flash}
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.libs.json.Json

/**
 * Created by jacques on 25/02/15.
 */
class Admins extends Controller{

  private val adminForm: Form[Admin] = Form(
    mapping(
      "password" -> nonEmptyText.verifying(
        "Mot de passe erroné ", md5(_) == "38B5188C3032225C37392EF863057344")
    )(models.Admin.apply)(Admin.unapply)
  )

  val foo = adminForm.copy()

  def password = Action { implicit request =>
    val form = if (request.flash.get("error").isDefined)
      adminForm.bind(request.flash.data)
    else
      adminForm

    Ok(views.html.admins.admin(form))
  }

  def checkPassword = Action { implicit request =>
    val newAdminForm = adminForm.bindFromRequest()
    newAdminForm.fold(
      hasErrors = { form =>
        Redirect(routes.Admins.password()).
          flashing(Flash(form.data) +
          ("error" -> Messages("validation.errors")))
      },
      success = { success =>
        val message = "Vous êtes administrateur"
        Redirect(routes.Application.index()).flashing("success" -> message)
          .withSession(request.session - "admin" + ("admin" -> "true"))
      }
    )
  }

  def upLoadImage = Action(parse.multipartFormData) {implicit request =>
    val reponse = request.body.file("file").map { monFichier =>
      if (monFichier.filename.isEmpty) {
        "{}"
      } else {
        val contentType = monFichier.contentType
        val fichierServeur = new File("%s/public/images/%s"
          .format(System.getenv("PWD"), monFichier.filename))
        monFichier.ref.moveTo(fichierServeur)
        """{"location" : "/assets/images/%s"}""".format(monFichier.filename)
      }
    }.getOrElse("{}")
    Ok(Json.parse(reponse))
  }
}
