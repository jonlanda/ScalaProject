class Casino {
  private var players: List[Player] = List()
  private val blackjack = new Blackjack()

  def start(): Unit = {
    println("Welcome to the Casino!")
    var running = true
    while (running) {
      println("1. Add Player")
      println("2. Play Blackjack")
      println("3. Exit")
      print("Choose an option: ")
      val choice = scala.io.StdIn.readInt()

      choice match {
        case 1 => addPlayer()
        case 2 => playBlackjack()
        case 3 => running = false
        case _ => println("Invalid choice, please try again.")
      }
    }
  }

  private def addPlayer(): Unit = {
    print("Enter player name: ")
    val name = scala.io.StdIn.readLine()
    print("Enter starting balance: ")
    val balance = scala.io.StdIn.readDouble()
    val player = Player(name, balance)
    players = player :: players
    blackjack.addPlayer(player)
    println(s"Player $name added with balance $balance.")
  }

  private def playBlackjack(): Unit = {
    if (players.isEmpty) {
      println("No players available. Add players first.")
    } else {
      blackjack.start()
    }
  }
}
