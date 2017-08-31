package dal

import javax.inject.{Inject, Singleton}

import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import models.{Materiel, TrekMateriel}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

@Singleton
class MaterielRep @Inject() (dbConfigProvider: DatabaseConfigProvider,
                             repoTrekMateriels: TrekMaterielRep)(implicit ec: ExecutionContext) {

  val dbConfig = dbConfigProvider.get[JdbcProfile]

  // These imports are important, the first one brings db into scope, which will let you do the actual db operations.
  // The second one brings the Slick DSL into scope, which lets you define the table and other queries.
  import dbConfig._
  import driver.api._

  private class MaterielTable(tag: Tag) extends Table[Materiel](tag, "materiel") {

    def id = column[Long]("id", O.PrimaryKey,O.AutoInc)
    def nom = column[String]("nom")
    def description = column[String]("description")
    def photo = column[String]("photo")
    def poids = column[Int]("poids")

    def * = (id, nom, description, photo, poids) <> ((Materiel.apply _).tupled, Materiel.unapply)
  }

  /**
    * The starting point for all queries on the people table.
    */
  private val materiels = TableQuery[MaterielTable]

  def add(materiel: Materiel): Long = {
    val fut = db.run((materiels returning materiels.map(_.id)) += materiel)
    Await.result(fut, 2.seconds)
  }

  def removeById(id: Long): Int = {
    val fut = db.run(materiels.filter(_.id === id).delete)
    Await.result(fut, 2.seconds)
  }

  def get(id: Long): Option[Materiel] = {
    val fut = db.run(materiels.filter(_.id === id).result.headOption)
    Await.result(fut, 2.seconds)
  }

  def update(materiel:Materiel) : Int = {
    val query = materiels.
      filter(_.id === materiel.id).
      map(mat => (mat.nom, mat.description, mat.photo, mat.poids))
    val fut = db.run(query.update(materiel.nom, materiel.description, materiel.photo, materiel.poids))
    Await.result(fut, 2.seconds)
  }

  def listAll: Seq[Materiel] = {
    val fut = db.run(materiels.result)
    Await.result(fut, 2.seconds)
  }

  def listTrekMateriel(idTrek: Long): Seq[Materiel] = {
    val listeMateriels = repoTrekMateriels.list(idTrek)
    for(idMat <- listeMateriels) yield {
      val fut = db.run(materiels.filter(_.id === idMat).result.head)
      Await.result(fut, 2.seconds)
    }
  }
}

