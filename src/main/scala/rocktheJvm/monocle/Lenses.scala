package rocktheJvm.monocle

import monocle.PLens

object Lenses {
  case class Guitar(make: String, model: String)
  case class Guitarist(name: String, favoriteGuitar: Guitar)
  case class RockBand(name: String, yearFormed: Int, leadGuitarist: Guitarist)

  val metallica = RockBand("Metallica", 1981, Guitarist("Kirk Hammett", Guitar("ESP", "M II")))
  val metallicaFixed = metallica.copy(
    leadGuitarist = metallica.leadGuitarist.copy(
      favoriteGuitar = metallica.leadGuitarist.favoriteGuitar.copy(
        model = metallica.leadGuitarist.favoriteGuitar.model.replace(" ", "-")
      )
    )
  )

  val kirksFavGuitar = Guitar("ESP", "M II")
  import monocle.Lens
  import monocle.macros.GenLens

  val guitarModelLens: Lens[Guitar, String] = GenLens[Guitar](_.model)
  // inspect a field or get the model
  val guitarModel = guitarModelLens.get(kirksFavGuitar)
  // modify a field
  val formattedGuitar = guitarModelLens.modify(_.replace(" ", "-"))(kirksFavGuitar) // Guitar("ESP", "M-II")
  val leadGuitaristLens: Lens[RockBand, Guitarist] = GenLens[RockBand](_.leadGuitarist)
  val guitarLens: Lens[Guitarist, Guitar] = GenLens[Guitarist](_.favoriteGuitar)
  val composedLens: Lens[RockBand, String] = leadGuitaristLens.composeLens(guitarLens).composeLens(guitarModelLens)
  val metallicaFixed2 = composedLens.modify(_.replace(" ", "-"))(metallica)

  // prims
  sealed trait Shape
  case class Circle(radius: Double) extends Shape
  case class Rectangle(w: Double, h: Double) extends Shape
  case class Triangle(a: Double, b: Double, c:Double) extends Shape

  val aCircle = Circle(20)
  val aRectangle = Rectangle(10, 20)
  val aTriangle = Triangle(3, 4, 5)
  val aShape: Shape = aCircle
  val newCircle: Shape = aShape match {
    case Circle(r) => Circle(r + 10)
    case d => d
  }
  import monocle.Prism
  val circlePrims = Prism[Shape, Double] {
    case Circle(r) => Some(r)
    case _ => None
  }(r => Circle(r))

  val anotherCircle = circlePrims(10) // "smart constructor"
  val radius = circlePrims.getOption(anotherCircle)  // Some(10)
  val noRadius = circlePrims.getOption(aRectangle) // None

  // combinations
  case class Icon(background:String, shape: Shape)
  case class Logo(color: String)
  case class BrandIdentity(logo: Logo, icon: Icon)

  val iconLens = GenLens[BrandIdentity](_.icon)
  val shapeLens = GenLens[Icon](_.shape)
  val brandCircleR = iconLens.composeLens(shapeLens).composePrism(circlePrims)
  val aBrandC = BrandIdentity(Logo("red"), Icon("white", Circle(45)))
  val aBrandR = BrandIdentity(Logo("red"), Icon("white", Rectangle(10,20)))
  val enlargedRadius = brandCircleR.modify(_ + 10)
  def main(args: Array[String]): Unit = {
      println(formattedGuitar)
      println(metallicaFixed2)
      println(enlargedRadius(aBrandR))
      println(enlargedRadius(aBrandC))
  }
}
