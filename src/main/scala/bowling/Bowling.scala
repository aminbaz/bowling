package bowling

object Bowling extends App{
  val game = Game(frames = List(Frame(1,1),Frame(1,1),Frame(1,1)))

  println(game.scoreFrames)
  println(game.sumResultFrames(game.scoreFrames,0))

}

case class Game(frames : List[Frame]){
  def updateFramesWithBonus(frames : List[Frame]) : List[Frame] = {
    def getScoreOfNextRoll(index : Int) : Int = {
      if(index < frames.size){
        frames(index+1).firstRoll
      }else{
        0
      }
    }
    def getScoreOf2NextRoll(index : Int) : Int = {
      if (index < frames.size - 1) {
        if (frames(index + 1).bonus == "strikes") {
          frames(index + 1).firstRoll + frames(index + 2).firstRoll
        } else {
          frames(index + 1).firstRoll + frames(index + 1).secondRoll
        }
      }else if(index < frames.size){
        frames(index + 1).firstRoll + frames(index + 1).secondRoll
      }else{
        0
      }
    }
    frames.map(x=>{
      x.bonus match {
        case "strikes" => x.copy(scoreFrame = 10+getScoreOf2NextRoll(frames.indexOf(x)))
        case "spares" => x.copy(scoreFrame = 10+getScoreOfNextRoll(frames.indexOf(x)))
        case _ => x
      }
    })
  }
  val scoreFrames = updateFramesWithBonus(frames.map(x => {
    if(x.firstRoll == 10){
      x.copy(scoreFrame=x.firstRoll,bonus = "strikes")
    }else if(x.firstRoll + x.secondRoll == 10){
      x.copy(scoreFrame=x.firstRoll + x.secondRoll, bonus = "spares")
    }else{
      x.copy(scoreFrame=x.firstRoll + x.secondRoll)
    }
  }))

  def sumResultFrames(frames : List[Frame], sum : Int): Int ={
    if(frames.isEmpty){
      sum
    }else{
      sumResultFrames(frames.tail,sum+frames.head.scoreFrame)
    }
  }
}

case class Frame(firstRoll : Int, secondRoll : Int, bonus : String = null, scoreFrame : Int = 0){
}