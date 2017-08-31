package models

import org.joda.time.DateTime
import play.Environment
import play.api.libs.json.Json

import scala.reflect.io.Path
import scala.xml.{Elem, XML}

case class Gpx(id: Long, idTrek: Long, titre: String, sousTitre: String, description: String, listeMatos: String, nomFichier: String, altitudeMinimum: Int, altitudeMaximum: Int, ascensionTotale: Int, descenteTotale: Int,
               heureDebut: String, heureFin: String, distanceTotale: Int, depart: String,
               arrivee: String, coordonneesPix: String, typegpx: String) {

}

object Gpx {

  implicit val gpxFormat = Json.format[Gpx]
  val precision = 1000D

  private def distance(pointA: (BigDecimal, BigDecimal), pointB: (BigDecimal, BigDecimal)): Double = {
    val a = 6378137D
    val b = 6356752.314245D
    val f = 1 / 298.257223563
    val L = Math.toRadians((pointB._2 - pointA._2).toDouble)
    val U1 = Math.atan((1 - f) * Math.tan(Math.toRadians(pointA._1.toDouble)))
    val U2 = Math.atan((1 - f) * Math.tan(Math.toRadians(pointB._1.toDouble)))
    val sinU1 = Math.sin(U1)
    val cosU1 = Math.cos(U1)
    val sinU2 = Math.sin(U2)
    val cosU2 = Math.cos(U2)
    var cosSqAlpha = .0
    var sinSigma = .0
    var cos2SigmaM = .0
    var cosSigma = .0
    var sigma = .0
    var lambda = L
    var lambdaP = .0
    var iterLimit = 100
    do {
      val sinLambda = Math.sin(lambda)
      val cosLambda = Math.cos(lambda)
      sinSigma = Math.sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda) + (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda) * (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda))
      if (sinSigma == 0) return 0
      cosSigma = sinU1 * sinU2 + cosU1 * cosU2 * cosLambda
      sigma = Math.atan2(sinSigma, cosSigma)
      val sinAlpha = cosU1 * cosU2 * sinLambda / sinSigma
      cosSqAlpha = 1 - sinAlpha * sinAlpha
      cos2SigmaM = cosSigma - 2 * sinU1 * sinU2 / cosSqAlpha
      val C = f / 16 * cosSqAlpha * (4 + f * (4 - 3 * cosSqAlpha))
      lambdaP = lambda
      lambda = L + (1 - C) * f * sinAlpha * (sigma + C * sinSigma * (cos2SigmaM + C * cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM)))
    } while ( {
      Math.abs(lambda - lambdaP) > 1e-12 && {
        iterLimit -= 1; iterLimit
      } > 0
    })
    if (iterLimit == 0) return 0
    val uSq = cosSqAlpha * (a * a - b * b) / (b * b)
    val A = 1 + uSq / 16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)))
    val B = uSq / 1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)))
    val deltaSigma = B * sinSigma * (cos2SigmaM + B / 4 * (cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM) - B / 6 * cos2SigmaM * (-3 + 4 * sinSigma * sinSigma) * (-3 + 4 * cos2SigmaM * cos2SigmaM)))
    val s = b * A * (sigma - deltaSigma)
    s
  }

  def creer(typegxp: String, id: Long, idTrek: Long, titre: String, sousTitre: String, description: String, listeMatos: String, filename: String): Gpx = {
    var xml: Elem = null
    var altitudes: Seq[Seq[Double]] = Seq()
    var altitudesPix: Seq[Seq[Int]] = Seq()
    var altitudeMinimum: Int = Int.MaxValue
    var altitudeMaximum: Int = Int.MinValue
    var ascensionTotale: Int = 0
    var descenteTotale: Int = 0
    var heureDebut: String = ""
    var heureFin: String = ""
    var distanceTotale: Int = 0
    var distancesCumulees: Seq[Seq[Double]] = Seq()
    var distancesCumuleesPix: Seq[Seq[Int]] = Seq()
    var depart: (BigDecimal, BigDecimal) = (0,0)
    var arrivee: (BigDecimal, BigDecimal) = (0, 0)
    var coordonneesPix: Seq[Seq[(Int, Int)]] = Seq()

    val racine = if(typegxp == "R") {
      "%s/contenu/gpx/randos/".format(Environment.simple.rootPath.getAbsolutePath)
    } else {
      "%s/contenu/gpx/treks/".format(Environment.simple.rootPath.getAbsolutePath)
    }

    xml = XML.loadFile(racine + filename)
    var ini = 0D
    for(trk <- xml \\ "trk") {
      val alt = (trk \\ "ele").map(_.text.toDouble)
        altitudes +:= alt
        altitudeMinimum = altitudeMinimum min alt.reduceLeft(_ min _).toInt
        altitudeMaximum = altitudeMaximum max alt.reduceLeft(_ max _).toInt
        val diffAltitudes = (alt.tail zip alt).map(f => f._1 - f._2)
        ascensionTotale += diffAltitudes.filter(_ > 0).sum.toInt
        descenteTotale += diffAltitudes.filter(_ < 0).sum.abs.toInt
        val trkpt = (trk \\ "trkpt").map(t => (BigDecimal(t \ "@lat" text) , BigDecimal(t \ "@lon" text)))
        val distances = (trkpt.tail zip trkpt).map(p => distance(p._1, p._2))
        distanceTotale += distances.sum.toInt
        val dist = ini +: (for (d <- distances) yield {
          ini = ini + d
          ini
        })
        distancesCumulees +:= dist
    }
    var reduction = (altitudeMaximum - altitudeMinimum)/ Gpx.precision
    for (t <- altitudes) {
      altitudesPix +:= t.map(a => (1000 - (a - altitudeMinimum) / reduction).toInt)
    }
    reduction = distanceTotale / (2 * Gpx.precision)
    for (t <- distancesCumulees) {
      distancesCumuleesPix +:= t.map(d => (d / reduction) toInt)
    }
    val heures = (xml \\ "trk" \\ "time").map(h => new DateTime(h.text).getMillis)
    if(heures.isEmpty){
      heureDebut = ""
      heureFin = ""
    } else {
      heureDebut = new DateTime(heures.reduceLeft(_ min _)).toString()
      heureFin = new DateTime(heures.reduceLeft(_ max _)).toString()
    }

    val trkpt = (xml \\ "trkpt").map(t => (BigDecimal(t \ "@lat" text) , BigDecimal(t \ "@lon" text)))
    arrivee = trkpt.last
    depart = trkpt.head
    for(i <- altitudesPix.indices) {
      coordonneesPix +:= (distancesCumuleesPix(i) zip altitudesPix(i)).distinct
    }
    new Gpx(id, idTrek, titre, sousTitre, description, listeMatos, filename, altitudeMinimum, altitudeMaximum,
      ascensionTotale, descenteTotale, heureDebut, heureFin,
      distanceTotale, "%s,%s".format(depart._1, depart._2), "%s,%s".format(arrivee._1, arrivee._2),
      { var html = ""
        for(trace <- coordonneesPix) {
          html += "<polyline points=\""
          for(c <- trace) {
            html += c._1 + "," + c._2 + " "
          }
          html += "\" style=\"fill: none;stroke: #f00;stroke-width: 5\"></polyline>\n"
        }
        html
      },
      typegxp)
  }

  def fusion(listeFichiersGpx: Seq[String], fichierGpxResultat: String) : Unit = {
    var resultat = ""
    for (fichier <- listeFichiersGpx) {
      val xml = XML.loadFile("%s/contenu/gpx/randos/%s".format(Environment.simple.rootPath.getAbsolutePath, fichier))
      if (resultat == "") {
        resultat = xml.toString()
      } else {
        val pattern = "(?s).*(<trk>.*?</trk>).*".r
        val pattern(trace) = xml.toString()
        resultat = resultat.replaceAll("(?s)</gpx>",
          "  %s\n\n</gpx>".format(trace))
      }
    }
    val xml = XML.loadString(resultat)
    val latitudes = (xml \\ "@lat").map(l => BigDecimal(l.text))
    val minlat = latitudes reduceLeft (_ min _)
    val maxlat = latitudes reduceLeft (_ max _)
    val longitudes = (xml \\ "@lon").map(l => BigDecimal(l.text))
    val minlon = longitudes reduceLeft (_ min _)
    val maxlon = longitudes reduceLeft (_ max _)
    resultat = resultat.replaceAll("(?s)<bounds .*?/>",
      "<bounds maxlat=\"%s\" maxlong=\"%s\" minlat=\"%s\" minlon=\"%s\"/>".format(maxlat, maxlon, minlat, minlon))
    XML.save("%s/contenu/gpx/treks/%s".format(Environment.simple.rootPath.getCanonicalPath, fichierGpxResultat), XML.loadString(resultat))
    if (scala.reflect.io.File(Path("%s/public".format(Environment.simple().rootPath().getCanonicalPath))).exists) {
      XML.save("%s/public/contenu/gpx/treks/%s".format(Environment.simple.rootPath.getCanonicalPath, fichierGpxResultat), XML.loadString(resultat))
    }
  }
}
