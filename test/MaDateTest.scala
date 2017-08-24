import models.MaDate
import org.joda.time
import org.joda.time.{DateTime, Instant}
import org.scalatest.FunSuite

class MaDateTest extends FunSuite {
  val date = new MaDate(2017,7,1)

  test("Une date a une année, un mois et un jour") {
    assert(date.annee == 2017)
    assert(date.mois == 7)
    assert(date.jour == 1)
  }

  val date2 = new MaDate(2018,7,1)
  test("Une date a une fonction ecartA une autre date") {
    assert(date.ecartA(date2).isInstanceOf[String])
  }

  test("L'écart entre le 2018-07-01 et le 2017-07-01 est Je suis déjà parti !") {
    assert(date2.ecartA(date) == "Je suis déjà parti !")
  }

  test("L'écart entre le 2017-07-01 et le 2018-07-01 est 1 an") {
    assert(date.ecartA(date2) == "1 an")
  }

  test("L'écart entre le 2017-07-01 et le 2017-08-01 est 1 mois") {
    assert(date.ecartA(new MaDate(2017,8,1)) == "1 mois")
  }

  test("L'écart entre le 2017-07-01 et le 2017-07-02 est 1 jour") {
    assert(date.ecartA(new MaDate(2017,7,2)) == "1 jour")
  }

  test("L'écart entre le 2017-07-27 et le 2018-07-01 est de 11 mois 4 jours") {
    assert(new MaDate(2017,7,27).ecartA(new MaDate(2018,7,1)) == "11 mois 4 jours")
  }

  test("L'écart entre le 2017-07-27 et le 2019-05-11 est de 1 an 9 mois 14 jours") {
    assert(new MaDate(2017,7,27).ecartA(new MaDate(2019,5,11)) == "1 an 9 mois 14 jours")
  }
}
