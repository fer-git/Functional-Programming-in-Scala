val x = "Hello, world"
val r1 = x.reverse
val r2 = x.reverse

case class Player(name: String, score: Int)

def printWinner(p: Player): Unit =
  println(p.name + " is the winner!")

def declareWinner(p1: Player, p2: Player): Unit =
  if (p1.score > p2.score) printWinner(p1)
  else printWinner(p2)

val sue = Player("Sue", 7)
val bob = Player("Bob", 8)
declareWinner(sue, bob)

def winner(p1: Player, p2: Player): Player =
  if (p1.score > p2.score) p1 else p2

def declareWinner2(p1: Player, p2: Player): Unit =
  printWinner(winner(p1, p2))

val players = List(Player("Sue", 7), Player("Bob", 8),
  Player("Joe", 4))

val p = players.reduceLeft(winner)

printWinner(p)