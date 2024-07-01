package UI.blackjack

import logic.Blackjack
import scala.swing._
import scala.swing.event._

class BetPanel(var blackjack: Blackjack, switchToUserPanel: (Blackjack) => Unit, switchToGamePanel: (Blackjack) => Unit, updateUserPanel: () => Unit) extends BoxPanel(Orientation.Vertical) {
  val returnButton = new Button {
    text = "Return to User Panel"
  }
  val startGameButton = new Button {
    text = "Start Game"
  }

  def updateBetPanel(): Unit = {
    contents.clear()

    blackjack.getPlayers.foreach { player =>
      val betField = new TextField {
        columns = 10
      }
      val placeBetButton = new Button {
        text = s"Place Bet for ${player.name}"
      }
      val betPlacedCheckBox = new CheckBox {
        text = s"${player.name} placed bet"
        enabled = false
      }

      contents += new FlowPanel {
        contents += new Label(s"${player.name} (balance: ${player.balance}): ")
        contents += betField
        contents += placeBetButton
        contents += betPlacedCheckBox
      }

      listenTo(placeBetButton)

      contents += returnButton
      contents += startGameButton

      reactions += {
        case ButtonClicked(`placeBetButton`) =>
          val bet = betField.text.toDouble
          println(s"Placing bet of $bet for player ${player.name}")
          blackjack.placeBet(player, bet) match {
            case Right(updatedBlackjack) =>
              blackjack = updatedBlackjack
              betPlacedCheckBox.selected = true
              println(s"Updated blackjack state: $blackjack")
            case Left(error) =>
              Dialog.showMessage(contents.head, error, title = "Bet Error")
          }
          updateUserPanel()
      }
    }
  }

  listenTo(returnButton, startGameButton)

  reactions += {
    case ButtonClicked(`returnButton`) =>
      switchToUserPanel(blackjack)

    case ButtonClicked(`startGameButton`) =>
      println(s"Starting game with state: $blackjack")
      switchToGamePanel(blackjack)
  }
}
