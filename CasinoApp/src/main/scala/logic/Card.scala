package logic

case class Card(suit: String, value: String)

object Card {
  val suits = List("\u2665", "\u2666", "\u2663", "\u2660")
  val values = List("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")

  def createDeck(): List[Card] = {
    for {
      suit <- suits
      value <- values
    } yield Card(suit, value)
  }

  def cardToString(card: Card): String = {
    s"${card.value}${card.suit}"
  }
}
