package controllers

import models.MaDate
import org.joda.time.DateTime
import play.api._
import play.api.mvc._

class Application extends Controller {

  def index = Action { implicit request =>
    val maintenant = DateTime.now
    val aujourdhui = new MaDate(maintenant.getYear, maintenant.getMonthOfYear, maintenant.getDayOfMonth)
    val retraite = new MaDate(2018,7,1)
    val depart = new MaDate(2019,5,11)
    val ecartRetraite = aujourdhui.ecartA(retraite)
    val ecartDepart = aujourdhui.ecartA(depart)
    Ok(views.html.index(ecartRetraite, ecartDepart))
  }
}