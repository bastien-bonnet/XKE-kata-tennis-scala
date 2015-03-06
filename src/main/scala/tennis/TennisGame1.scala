package tennis

case class TennisGame1(val player1Name: String, val player2Name: String) extends TennisGame {
	var score1: Int = 0
	var score2: Int = 0

	def wonPoint(playerName: String) {
		if (playerName == player1Name)
			score1 += 1
		else
			score2 += 1
	}

	def calculateScore(): String = {
		if (score1 == score2) {
			equalScore
		} else if (score1 >= 4 || score2 >= 4) {
			advantageScore
		} else {
			scoreName(score1) + "-" + scoreName(score2)
		}
	}

	def equalScore: String = {
		score1 match {
			case 0 | 1 | 2 => scoreName(score1) + "-All"
			case _ => "Deuce"
		}
	}

	def advantageScore: String = {
		val delta = score1 - score2
		val score = if (Math.abs(delta) == 1) "Advantage " else "Win for "
		val winner = if (delta > 0) player1Name else player2Name
		
		score + winner
	}

	def scoreName(score: Int): String = {
		score match {
			case 0 => "Love"
			case 1 => "Fifteen"
			case 2 => "Thirty"
			case 3 => "Forty"
		}
	}

}
