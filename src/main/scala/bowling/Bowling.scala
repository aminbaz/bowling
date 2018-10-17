package bowling

object Bowling extends App{
  val game = Game(frames = List(Frame(1,1),Frame(1,1),Frame(1,1)))

  println(game.scoreFrames)
  println(game.sumResultFrames(game.scoreFrames,0))

}

case class Game(frames : List[Frame]){
  /**
    * Take a list of Frame and edit them with the bonus when it's a strike or a spare
    * @param frames List of 10 frames
    */
  def updateFramesWithBonus(frames : List[Frame]) : List[Frame] = {
    /**
      * Take the score of the next roll to edit the score when its a spares
      * @param index to find the frame which contains a spares bonus
      * @return
      */
    def getScoreOfNextRoll(index : Int) : Int = {
      if(index < frames.size){
        frames(index+1).firstRoll
      }else{
        0
      }
    }

    /**
      * Get the score of the 2 next rolls to add it when its a strikes
      * @param index to find the frame which contains a spares bonus
      * @return
      */
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

  /**
    * This Variable will contains the edited list of frame with the bonus
    */
  val scoreFrames = updateFramesWithBonus(frames.map(x => {
    if(x.firstRoll == 10){
      x.copy(scoreFrame=x.firstRoll,bonus = "strikes")
    }else if(x.firstRoll + x.secondRoll == 10){
      x.copy(scoreFrame=x.firstRoll + x.secondRoll, bonus = "spares")
    }else{
      x.copy(scoreFrame=x.firstRoll + x.secondRoll)
    }
  }))

  /**
    * Compute the sum of each score frame
    * @param frames List of 10 frames
    * @param add the result in each iteration
    */
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