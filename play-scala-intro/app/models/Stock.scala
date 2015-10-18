package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Stock(symbol: String, price: Double) 

object Stock {
implicit val stockFormat = Json.format[Stock]

}