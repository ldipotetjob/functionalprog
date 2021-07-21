import .audienceInt

sealed trait Parseable

case class Centimeters(value: Double) extends AnyVal
case class Meters(value: Double) extends AnyVal
case class Kilometers(value: Double) extends AnyVal
implicit def meters2centimeters(meters: Meters): Centimeters = Centimeters(meters.value * 100)

case class FootballMatch(season: String, league: String, audience: audienceInt) extends Parseable
case class FootballMatchOpt(season: String, league: String, audience: Option[audienceInt]) extends Parseable

implicit def optStr2str(audience: Option[audienceInt]): Int = audience match {
  case Some (audience) => audience
  case _ => 0
}

val centimeters: Centimeters = Meters(2.5)

val pp = FootballMatchOpt("ggg", "qq", None)
val pp1: FootballMatch= pp.copy()