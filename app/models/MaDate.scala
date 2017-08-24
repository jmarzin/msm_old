package models

import org.joda.time.DateTime

class MaDate(var annee: Int, var mois: Int, var jour: Int) {

  private def ecartTexte(valeur: Int, mot: String): String= {
    if (valeur <= 0) return ""
    if (valeur == 1 || mot.endsWith("s")) return valeur + " " + mot
    valeur + " " + mot + "s"
  }

  private def ecartAnnee(date: MaDate): String= {
    var annee = date.annee - this.annee
    if (date.mois < this.mois || (date.mois == this.mois && date.jour < this.jour)) annee -= 1
    ecartTexte(annee, "an")
  }

  private def ecartMois(date: MaDate): String= {
    var mois = date.mois - this.mois
    if (date.jour < this.jour) mois -= 1
    if (mois <  0) mois += 12
    ecartTexte(mois, "mois")
  }

  private def ecartJours(date: MaDate): String= {
    if(date.jour == this.jour) return ""
    var mois2 = date.mois
    var annee2 = date.annee
    if (date.mois < this.mois || (date.mois == this.mois && date.jour < this.jour)) {
      if (date.mois == 1) {
        mois2 = 12
        annee2 -= 1
      } else {
        mois2 -= 1
      }
    }
    var jour2 = this.jour
    if(jour2 == 31) {
      if(mois2 == 2) {
        jour2 = 28
      } else {
        jour2 = 30
      }
    }
    val dateT2 = DateTime.parse(annee2.toString+'-'+mois2.toString+'-'+jour2.toString)
    val dateT =DateTime.parse(date.annee.toString+'-'+date.mois.toString+'-'+date.jour.toString)
    val jours = ((dateT.getMillis - dateT2.getMillis)/1000/60/60/24).toInt
      ecartTexte(jours, "jour")
  }

  def ecartA(date: MaDate): String= {
    if (new DateTime("%s-%s-%s".format(this.annee.toString, this.mois.toString, this.jour.toString)).getMillis >=
      new DateTime("%s-%s-%s".format(date.annee.toString, date.mois.toString, date.jour.toString)).getMillis) {
      return "Je suis déjà parti !"
    }
    (this.ecartAnnee(date) + " " + this.ecartMois(date) + " " + this.ecartJours(date).replaceAll("  "," ")).trim
  }
}
