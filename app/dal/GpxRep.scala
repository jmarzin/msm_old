package dal

import javax.inject.{Inject, Singleton}

import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import models.{Gpx, Materiel}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

@Singleton
class GpxRep @Inject() (dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {

  val dbConfig = dbConfigProvider.get[JdbcProfile]

  // These imports are important, the first one brings db into scope, which will let you do the actual db operations.
  // The second one brings the Slick DSL into scope, which lets you define the table and other queries.
  import dbConfig._
  import driver.api._

  private class GpxTable(tag: Tag) extends Table[Gpx](tag, "gpx") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def idTrek = column[Long]("idtrek")
    def titre = column[String]("titre")
    def sousTitre = column[String]("soustitre")
    def description = column[String]("description")
    def listeMatos = column[String]("listematos")
    def nomFichier = column[String]("nomfichier")
    def altitudeMinimum = column[Int]("altitudeminimum")
    def altitudeMaximum = column[Int]("altitudemaximum")
    def ascensionTotale = column[Int]("ascensiontotale")
    def descenteTotale = column[Int]("descentetotale")
    def heureDebut = column[String]("heuredebut")
    def heureFin = column[String]("heurefin")
    def distanceTotale = column[Int]("distancetotale")
    def depart = column[String]("depart")
    def arrivee = column[String]("arrivee")
    def coordonneesPix = column[String]("coordonneespix")
    def typegpx = column[String]("typegpx")


    def * = (id, idTrek, titre, sousTitre, description, listeMatos, nomFichier, altitudeMinimum, altitudeMaximum,
      ascensionTotale, descenteTotale, heureDebut, heureFin, distanceTotale, depart, arrivee, coordonneesPix, typegpx) <> ((Gpx.apply _).tupled, Gpx.unapply)
  }

  /**
    * The starting point for all queries on the people table.
    */
  private val gpxs = TableQuery[GpxTable]

  def add(gpx: Gpx): Long = {
    val fut = db.run((gpxs returning gpxs.map(_.id)) += gpx)
    Await.result(fut, 2.seconds)
  }

  def removeById(id: Long): Int = {
    val fut = db.run(gpxs.filter(_.id === id).delete)
    Await.result(fut, 2.seconds)
  }

  def get(id: Long): Option[Gpx] = {
    val fut = db.run(gpxs.filter(_.id === id).result.headOption)
    Await.result(fut, 2.seconds)
  }

  def update(gpx: Gpx) : Int = {
    val query = gpxs.
      filter(_.id === gpx.id).
      map(g => (g.idTrek, g.titre, g.sousTitre, g.description, g.nomFichier, g.altitudeMinimum, g.altitudeMaximum,
      g.ascensionTotale, g.descenteTotale, g.heureDebut, g.heureFin, g.distanceTotale, g.depart, g.arrivee, g.coordonneesPix,
      g.listeMatos))
    val fut = db.run(query.update(gpx.idTrek, gpx.titre, gpx.sousTitre, gpx.description, gpx.nomFichier,
      gpx.altitudeMinimum, gpx.altitudeMaximum, gpx.ascensionTotale, gpx.descenteTotale, gpx.heureDebut, gpx.heureFin,
      gpx.distanceTotale, gpx.depart, gpx.arrivee, gpx.coordonneesPix, gpx.listeMatos))
    Await.result(fut, 2.seconds)
  }

  def listAllR: Seq[Gpx] = {
    val fut = db.run(gpxs.filter(_.typegpx === "R").result)
    Await.result(fut, 2.seconds)
  }

  def listAllT: Seq[Gpx] = {
    val fut = db.run(gpxs.filter(_.typegpx === "T").result)
    Await.result(fut, 2.seconds)
  }

  def listByTrekId(id: Long): Seq[Gpx] = {
    val fut= db.run(gpxs.filter(_.idTrek === id).result)
    Await.result(fut, 2.seconds)
  }
}


