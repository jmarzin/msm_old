package controllers


import javax.inject.Inject

import dal.{GpxRep, MaterielRep, TrekMaterielRep}
import models.{Gpx, Materiel}
import play.Environment
import play.api.mvc.{Action, Controller, Flash}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.data.Form
import play.api.data.Forms._

import scala.concurrent.ExecutionContext

case class GpxForm(id: Long, idTrek: Long, titre: String, sousTitre: String, description: String, listeMatos: String, nomFichier: String, altitudeMinimum: Int, altitudeMaximum: Int, ascensionTotale: Int, descenteTotale: Int,
                   heureDebut: String, heureFin: String, distanceTotale: Int, depart: String,
                   arrivee: String, coordonneesPix: String, typegpx: String, listeFichiers: Seq[String], listeMaterielsTreks: Seq[String])


class GpxController @Inject() (repoGpx: GpxRep,
                               repoTrekMateriels: TrekMaterielRep,
                               repoMateriels: MaterielRep,
                               val messagesApi: MessagesApi)
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
      "listeFichiers" -> seq(text),
      "listeMaterielsTrek" -> seq(text)
    )(GpxForm.apply)(GpxForm.unapply)
  )

  val taillePage = 20

  def listGpx(typegpx: String, page: Int) = Action { implicit request =>
    var gpxs = if(typegpx == "T")
                  repoGpx.listAllT.sortWith(_.heureDebut > _.heureDebut)
               else
                  repoGpx.listAllR.sortWith(_.heureDebut > _.heureDebut)
    var pageAAfficher = page
    var nbPages = Math.ceil(gpxs.size/taillePage.toFloat).toInt
    pageAAfficher = pageAAfficher max 1
    pageAAfficher = pageAAfficher min nbPages
    if(pageAAfficher != 1) {
      gpxs = gpxs.drop(taillePage * (pageAAfficher - 1))
    }
    if(gpxs.size > taillePage) {
      gpxs = gpxs.slice(0, taillePage)
    }
    Ok(views.html.gpx.list(gpxs, typegpx, pageAAfficher, nbPages))
  }

  def listGpxTrk(id: Long, page: Int) = Action { implicit request =>
    var gpxs = repoGpx.listByTrekId(id).sortWith(_.heureDebut < _.heureDebut)
    var pageAAfficher = page
    var nbPages = Math.ceil(gpxs.size/taillePage.toFloat).toInt
    pageAAfficher = pageAAfficher max 1
    pageAAfficher = pageAAfficher min nbPages
    if(pageAAfficher != 1) {
      gpxs = gpxs.drop(taillePage * (pageAAfficher - 1))
    }
    if(gpxs.size > taillePage) {
      gpxs = gpxs.slice(0, taillePage)
    }
    Ok(views.html.gpx.list(gpxs, "R", pageAAfficher, nbPages))
  }

  def apropos = Action { implicit request =>
    repoGpx.get(1L).map { gpx =>
      val existeListeMateriels = repoTrekMateriels.exist(gpx.id)
      Ok(views.html.gpx.details(gpx, existeListeMateriels))
    }.getOrElse(NotFound)
  }

  def showGpx(id: Long) = Action { implicit request =>
    repoGpx.get(id).map { gpx =>
      val existeListeMateriels = repoTrekMateriels.exist(gpx.id)
      Ok(views.html.gpx.details(gpx, existeListeMateriels))
    }.getOrElse(NotFound)
  }

  def deleteGpx(id: Long) = Action { implicit request =>
    if(!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.showGpx(id)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      repoGpx.get(id).map { gpx =>
        repoGpx.removeById(id)
        if (gpx.typegpx == "R") {
          Redirect(routes.GpxController.listGpx("R", 1)).flashing("success" -> "Tracé supprimé")
        } else {
          Redirect(routes.GpxController.listGpx("T", 1)).flashing("success" -> "Tracé supprimé")
        }
      }.getOrElse(NotFound)
    }
  }

  def newGpx(typegpx: String) = Action { implicit request =>
    if(!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.listGpx("R", 1)).flashing("warning" -> "Vous n'êtes pas administrateur")
    } else {
      var listeFichiers = Seq[(Long, String, Long)]()
      var listeMateriels = Seq[Materiel]()
      val form = if (request.flash.get("error").isDefined) {
        val errorForm = this.formGpx.bind(request.flash.data)
        errorForm
      } else {
        this.formGpx.fill(GpxForm(0L, 0L, "", "", "", "", "", 0, 0, 0, 0, "", "", 0, "", "", "", typegpx,
            listeFichiers.map(f => f._1.toString + "," + f._2), Seq()))
      }
      if(typegpx == "R") {
        val liste = ("" +: new java.io.File("%s/contenu/gpx/randos/".
          format(Environment.simple.rootPath.getCanonicalPath)).
          listFiles.map(f => f.getName).filter(_.toLowerCase.endsWith(".gpx")).
          toSeq).diff(repoGpx.listGpxFiles)
        listeFichiers = liste.map((0L, _, 0L))
      } else {
        listeFichiers = repoGpx.listCandidatTrek(0L).sortWith(_._2 < _._2)
        listeMateriels = repoMateriels.listAll
      }
      Ok(views.html.gpx.edit(form,
        listeCandidats = listeFichiers,
        listeMateriels = listeMateriels))
    }
  }

  def saveGpx = Action(parse.multipartFormData) { implicit request =>
    //au retour, nomFichier contient gpx.nomFichier
    //           listeFichier le ou  les fichiers candidats du serveur
    //           monFichier le fichier local
    val typegpx = request.body.dataParts("typegpx").head
    if (!request.session.get("admin").isDefined) {
      Redirect(routes.GpxController.listGpx(typegpx, 1)).flashing("warning" -> "Vous n'êtes pas administrateur")
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
            val nomFichier = uploadFile("gpx", newForm.data("listeFichiers"), request).getOrElse("")
            if (nomFichier == "") {
              idx = repoGpx.add(Gpx(newGpx.id, newGpx.idTrek, newGpx.titre, newGpx.sousTitre, newGpx.description, newGpx.listeMatos,
                nomFichier, newGpx.altitudeMinimum, newGpx.altitudeMaximum, newGpx.ascensionTotale, newGpx.descenteTotale,
                newGpx.heureDebut, newGpx.heureFin, newGpx.distanceTotale, newGpx.depart,
                newGpx.arrivee, newGpx.coordonneesPix, newGpx.typegpx))
            } else {
              idx = repoGpx.add(Gpx.creer("R", newGpx.id, newGpx.idTrek, newGpx.titre, newGpx.sousTitre,
                newGpx.description, newGpx.listeMatos, nomFichier))
            }
          } else {
            val nextIndex = repoGpx.lastValueIndex + 1
            val listeFichiers = for(f <- newForm.get.listeFichiers) yield {
              val sub = f.replaceAll("[\\(\\)]","").split(",")
              (sub(0),sub(1))
            }
            val listeMaterielsARattacher = for(f <- newGpx.listeMaterielsTreks) yield {
              val sub = f.replaceAll("[\\(\\)]","").split(",")
              sub(0).toLong
            }
            Gpx.fusion(listeFichiers.map(_._2), nextIndex.toString + ".gpx")
            idx = repoGpx.add(Gpx.creer("T", newGpx.id, newGpx.idTrek, newGpx.titre, newGpx.sousTitre,
              newGpx.description, newGpx.listeMatos, nextIndex.toString + ".gpx"))
            repoGpx.rattacheGpx(idx, listeFichiers.map(_._1.toLong))
            repoTrekMateriels.maj(idx, listeMaterielsARattacher)
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
      var listeMateriels = Seq[Materiel]()
      var listeMaterielsTrek = Seq[Materiel]()
      repoGpx.get(id).map { gpx =>
        if(gpx.typegpx == "R") {
          val liste = ("" +: new java.io.File("%s/contenu/gpx/randos/".
            format(Environment.simple.rootPath.getCanonicalPath))
            .listFiles.map(f => f.getName).filter(_.toLowerCase.endsWith(".gpx")).toSeq).diff(repoGpx.listGpxFiles)
          listeFichiers = liste.map((0L, _, 0L))
        } else {
          listeFichiers = repoGpx.listCandidatTrek(gpx.id).sortWith(_._2 < _._2)
          listeMateriels = repoMateriels.listAll
          listeMaterielsTrek = repoTrekMateriels.list(gpx.id).map(id => repoMateriels.get(id).get)
        }
        val gpxForm = GpxForm(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description,
          gpx.listeMatos, gpx.nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale,
          gpx.descenteTotale, gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart, gpx.arrivee,
          gpx.coordonneesPix, gpx.typegpx,
          listeFichiers.filter(_._3 == gpx.id).map(f => f._1.toString + "," + f._2),
          listeMaterielsTrek.map(m => m.id.toString + "," + m.nom))
        Ok(views.html.gpx.edit(this.formGpx.fill(gpxForm),
          Option(id),
          listeCandidats= listeFichiers,
          listeMateriels = listeMateriels))
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
          var listeFichiers = Seq[(Long, String, Long)]()
          var listeMateriels = Seq[Materiel]()
          if(newForm.data("typegpx") == "R") {
            val liste = ("" +: new java.io.File("%s/contenu/gpx/randos/".
              format(Environment.simple.rootPath.getCanonicalPath))
              .listFiles.map(f => f.getName).filter(_.toLowerCase.endsWith(".gpx")).toSeq).diff(repoGpx.listGpxFiles)
            listeFichiers = liste.map((0L, _, 0L))
          } else {
            listeFichiers = repoGpx.listCandidatTrek(newForm.data("id").toLong).sortWith(_._2 < _._2)
            listeMateriels = repoMateriels.listAll
          }
          Ok(views.html.gpx.edit(form,
            Option(id),
            listeCandidats = listeFichiers,
            listeMateriels = listeMateriels)).flashing(Flash(form.data) +
            ("error" -> "Erreur de saisie"))
        },
        success = { gpx =>
          if(newForm.data("typegpx") == "R") {
            val nomFichier = uploadFile("gpx", newForm.data("listeFichiers"), request).getOrElse(gpx.nomFichier)
            if (repoGpx.get(id).get.nomFichier == nomFichier || nomFichier.isEmpty)
              repoGpx.update(Gpx(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos,
                nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale, gpx.descenteTotale,
                gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart,
                gpx.arrivee, gpx.coordonneesPix, gpx.typegpx))
            else {
              repoGpx.update(Gpx.creer("R", gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos, nomFichier))
            }
          } else {
            val listeFichiers = for(f <- newForm.get.listeFichiers) yield {
              val sub = f.replaceAll("[\\(\\)]","").split(",")
              (sub(0),sub(1))
            }
            val listeFic = listeFichiers.map(_._2)
            val listeAExclure = repoGpx.listCandidatTrek(id).filter(_._3 == id).map(_._2)
            if(listeFic.size == listeAExclure && listeFic.diff(listeAExclure).isEmpty) {
              repoGpx.update(Gpx(gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.listeMatos,
                gpx.nomFichier, gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale, gpx.descenteTotale,
                gpx.heureDebut, gpx.heureFin, gpx.distanceTotale, gpx.depart,
                gpx.arrivee, gpx.coordonneesPix, gpx.typegpx))
            } else {
              Gpx.fusion(listeFichiers.map(_._2), id.toString + ".gpx")
              repoGpx.update(Gpx.creer("T", gpx.id, gpx.idTrek, gpx.titre, gpx.sousTitre,
                gpx.description, gpx.listeMatos, id.toString + ".gpx"))
              repoGpx.rattacheGpx(id, listeFichiers.map(_._1.toLong))
            }
            val listeMaterielsARattacher = for(f <- gpx.listeMaterielsTreks) yield {
              val sub = f.replaceAll("[\\(\\)]","").split(",")
              sub(0).toLong
            }
            repoTrekMateriels.maj(gpx.id, listeMaterielsARattacher)
          }
          Redirect(routes.GpxController.showGpx(gpx.id))
              .flashing("success" -> ("Modification réussie du tracé " + id.toString))
        }
      )
    }
  }
}
