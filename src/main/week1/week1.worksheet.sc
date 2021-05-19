abstract class JSON

case class JSeq (elems: List[JSON]) extends JSON
case class JObj (bindings: Map[String, JSON]) extends JSON
case class JNum (num: Double) extends JSON
case class JStr (str: String) extends JSON
case class JBool (b: Boolean) extends JSON
case object JNull extends JSON

val data: List[JSON] = List(
  JObj(Map(
    "firstName" -> JStr("Andres"),
    "lastName" -> JStr("Villarreal"),
    "address" -> JObj(Map(
      "stretAddress" -> JStr("calle 33A"),
      "state" -> JStr("Ant"),
      "postalCode" -> JNum(50030)
    )),
    "phoneNumbers" -> JSeq(List(
      JObj(Map(
        "type" -> JStr("home"),
        "number" -> JStr("222 555-1234")
      )),
      JObj(Map(
        "type" -> JStr("fax"),
        "number" -> JStr("646 555-4567")
      ))
    ))
  ))
)

def show(json: JSON): String = json match {
  case JSeq(elems) =>
    "[" + (elems map show mkString ", ") + "]"
  case JObj(bindings) =>
    val assocs = bindings map {
      case (key, value) => "\"" + key + "\": " + show(value)
    }
    "{" + (assocs mkString ", ") + "}"
  case JNum(num) => num.toString
  case JStr(str) => '\"' + str + '\"'
  case JBool(b) => b.toString
  case JNull => "null"
}


println(show(data.head))


val f: String => String = { case "ping" => "pong "}

f("ping")
// f("abc") <= throws a scala.MatchError because "abc" is not defined into f function.

val fPartial: PartialFunction[String, String] = { case "ping" => "pong" }

fPartial("ping")
// fPartial.("pong") <= throws a scala.MatchError because "abc" is not defined into f function.
fPartial.isDefinedAt("ping")
fPartial.isDefinedAt("pong")


for {
  JObj(bindings) <- data
  JSeq(phones) = bindings("phoneNumbers")
  JObj(phone) <- phones
  JStr(digits) = phone("number")
  if digits startsWith "222"
} yield (bindings("firstName"), bindings("lastName"))


