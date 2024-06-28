package UI.blackjack

import logic.{Blackjack, Card}

import scala.swing._
import scala.swing.event._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class GamePanel(blackjack: Blackjack, switchToUserPanel: () => Unit) extends BoxPanel(Orientation.Vertical) {
  val dealerHandLabel = new Label("Dealer's Hand")
  val dealerHandArea = new TextArea {
    rows = 1
    columns = 20
    editable = false
  }

  val currentPlayerLabel = new Label("")
  val currentPlayerHandArea = new TextArea {
    rows = 1
    columns = 20
    editable = false
  }

  val playersList = new ListView[String] {
    visibleRowCount = 10
    fixedCellWidth = 200
  }

  val hitButton = new Button {
    text = "Hit"
  }

  val standButton = new Button {
    text = "Stand"
  }

  val returnButton = new Button {
    text = "Return"
  }

  contents += new BoxPanel(Orientation.Vertical) {
    contents += dealerHandLabel
    contents += dealerHandArea
  }

  contents += new FlowPanel {
    contents += new BoxPanel(Orientation.Vertical) {
      contents += new Label("Players:")
      contents += new ScrollPane(playersList)
    }
    contents += new BoxPanel(Orientation.Vertical) {
      contents += currentPlayerLabel
      contents += currentPlayerHandArea
    }
  }

  contents += new FlowPanel {
    contents += hitButton
    contents += standButton
  }
  contents += returnButton

  listenTo(hitButton, standButton, returnButton)

  var currentPlayerIndex: Int = 0

  def resetGame(): Unit = {
    dealerHandArea.text = ""
    currentPlayerLabel.text = ""
    currentPlayerHandArea.text = ""
    blackjack.start()
    val dealerHand = blackjack.getDealerHand
    dealerHandArea.text = dealerHand.map(cardToString).mkString(", ")
    updatePlayersList()
    currentPlayerIndex = 0
    if (blackjack.getPlayers.nonEmpty) {
      showCurrentPlayerTurn()
    }
  }

  def updatePlayersList(): Unit = {
    playersList.listData = blackjack.getPlayers.map(player => s"${player.name}")
  }

  def showCurrentPlayerTurn(): Unit = {
    if (currentPlayerIndex < blackjack.getPlayers.length) {
      val currentPlayer = blackjack.getPlayers(currentPlayerIndex)
      currentPlayerLabel.text = s"${currentPlayer.name}'s turn"
      currentPlayerHandArea.text = blackjack.getPlayerHand(currentPlayer).map(cardToString).mkString(", ")
    } else {
      dealerTurnAndResults()
    }
  }

  def dealerTurnAndResults(): Unit = {
    dealerHandArea.text = blackjack.getDealerHand.map(cardToString).mkString(", ")
    val results = blackjack.determineWinners(message => dealerHandArea.append(message + "\n"))
    results.foreach(result => dealerHandArea.append(result + "\n"))
  }

  def playerBusts(): Unit = {
    if (currentPlayerIndex < blackjack.getPlayers.length) {
      val currentPlayer = blackjack.getPlayers(currentPlayerIndex)
      val playerTotal = blackjack.handValue(blackjack.getPlayerHand(currentPlayer))
      currentPlayerHandArea.append(s"\n${currentPlayer.name} busts with $playerTotal!\n")
      updatePlayerListWithScores()
      currentPlayerIndex += 1
      Future {
        Thread.sleep(2000)
        showCurrentPlayerTurn()
      }
    }
  }

  def updatePlayerListWithScores(): Unit = {
    playersList.listData = blackjack.getPlayers.map { player =>
      val playerTotal = blackjack.handValue(blackjack.getPlayerHand(player))
      s"${player.name}: $playerTotal"
    }
  }

  reactions += {
    case ButtonClicked(`hitButton`) =>
      if (currentPlayerIndex < blackjack.getPlayers.length) {
        val currentPlayer = blackjack.getPlayers(currentPlayerIndex)
        val result = blackjack.hit(currentPlayer)
        val newCard = blackjack.getPlayerHand(currentPlayer).last
        currentPlayerHandArea.append(s", ${cardToString(newCard)}")
        if (blackjack.handValue(blackjack.getPlayerHand(currentPlayer)) > 21) {
          playerBusts()
        }
      }

    case ButtonClicked(`standButton`) =>
      if (currentPlayerIndex < blackjack.getPlayers.length) {
        val currentPlayer = blackjack.getPlayers(currentPlayerIndex)
        val playerTotal = blackjack.handValue(blackjack.getPlayerHand(currentPlayer))
        currentPlayerHandArea.append(s"\nPlayer stands with $playerTotal\n")
        updatePlayerListWithScores()
        currentPlayerIndex += 1
        showCurrentPlayerTurn()
      }

    case ButtonClicked(`returnButton`) =>
      switchToUserPanel()
  }

  def cardToString(card: Card): String = {
    s"${card.value}${card.suit}"
  }
}
