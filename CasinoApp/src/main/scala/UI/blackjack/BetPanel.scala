package UI.blackjack

import logic.Blackjack
import scala.swing._
import scala.swing.event._

class BetPanel(
                blackjack: Blackjack,
                switchToUserPanel: () => Unit,
                switchToGamePanel: () => Unit,
                updateUserPanel: () => Unit
              ) extends BoxPanel(Orientation.Vertical) {
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
          if (blackjack.placeBet(player, bet)) {
            betPlacedCheckBox.selected = true
          } else {
            Dialog.showMessage(contents.head, s"${player.name} has insufficient balance", title = "Bet Error")
          }
          updateUserPanel()
      }
    }
  }

  listenTo(returnButton, startGameButton)

  reactions += {
    case ButtonClicked(`returnButton`) =>
      switchToUserPanel()

    case ButtonClicked(`startGameButton`) =>
      switchToGamePanel()
  }
}
