package logic

import scala.util.Random

class Deck {
  private var cards: List[Card] = Random.shuffle(Card.createDeck())

  def drawCard(): Option[Card] = {
    cards match {
      case Nil => None
      case head :: tail =>
        cards = tail
        Some(head)
    }
  }

  def removeCard(card: Card): Unit = {
    cards = cards.filterNot(_ == card)
  }
}
