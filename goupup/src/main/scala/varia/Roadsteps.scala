object RoadSystem {
  case class Section(val a: Int, val b: Int, val c: Int)
  type RoadSystem = List[Section]

  trait Label
  case object A extends Label
  case object B extends Label
  case object C extends Label

  type Path = List[(Label, Int)]

  def roadStep(paths: (Path, Path), section: Section): (Path, Path) = {
    val sumPathA = paths._1.map(_._2).sum
    val sumPathB = paths._2.map(_._2).sum
    val Section(a, b, c) = section
    val straightA = sumPathA + a
    val crossB =  sumPathA + c + b
    val straightB = sumPathB + b
    val crossA = sumPathB + c + a
    val newPathA = if(straightA < crossB) (A, a) :: paths._1 else (C, c) :: (B, b) :: paths._1
    val newPathB = if(straightB < crossA) (B, b) :: paths._2 else (C, c) :: (A, a) :: paths._2
    (newPathA, newPathB)
  }

  val roadSystem: RoadSystem = List(Section(2,4,7), Section(10, 34, 1), Section(2,4,5))

  def optimalPath(roadSystem: RoadSystem): (Path, Path) = {
    roadSystem.foldLeft((List[(Label, Int)](),List[(Label, Int)]()))((paths, elem) => roadStep(paths, elem))
  }
}
