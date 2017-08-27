package controllers

import java.io.File

import models.MaDate
import org.joda.time.DateTime
import play.api._
import play.api.libs.json.Json
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


//  def upload(request):
//  form = myForm(request.POST, request.FILES)
//  if form.is_valid():
//    image = form.save()
//  return HttpResponse("<script>top.$('.mce-btn.mce-open').parent().find('.mce-textbox').val('%s').closest('.mce-window').find('.mce-primary').click();</script>" % image.get_absolute_url())
//  return HttpResponse("<script>alert('%s');</script>" % escapejs('\n'.join([v[0] for k, v in form.errors.items()])))
}