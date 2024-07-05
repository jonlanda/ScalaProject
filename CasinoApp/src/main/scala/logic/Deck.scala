package logic

import scala.util.Random

case class Deck(cards: List[Card]) {
  def drawCard(): (Option[Card], Deck) = {
    cards match {
      case Nil => (None, this)
      case head :: tail => (Some(head), Deck(tail))
    }
  }

  def removeCard(card: Card): Deck = {
    Deck(cards.filterNot(_ == card))
  }

  def shuffle(): Deck = {
    Deck(Random.shuffle(cards))
  }
}

object Deck {
  def apply(): Deck = new Deck(Random.shuffle(Card.createDeck()))
}
