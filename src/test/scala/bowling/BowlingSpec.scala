package bowling

import org.scalatest.{FunSpec, Matchers}

class BowlingSpec extends FunSpec with Matchers {

  it("All roll with 0 pin down should give a score of 0") {
    val game = Game(frames = List(Frame(0,0),Frame(0,0),Frame(0,0),Frame(0,0),Frame(0,0),Frame(0,0),Frame(0,0),Frame(0,0),Frame(0,0),Frame(0,0)))
    assert(game.sumResultFrames(game.scoreFrames,0) == 0)
  }
  it("Only strike should give a score of 300") {
    val game = Game(frames = List(Frame(10,0),Frame(10,0),Frame(10,0),Frame(10,0),Frame(10,0),Frame(10,0),Frame(10,0),Frame(10,0),Frame(10,0),Frame(10,0)))
    assert(game.sumResultFrames(game.scoreFrames,0) == 300)
  }
  it("All roll with 1 pin down should give a score of 20") {
    val game = Game(frames = List(Frame(1,1),Frame(1,1),Frame(1,1),Frame(1,1),Frame(1,1),Frame(1,1),Frame(1,1),Frame(1,1),Frame(1,1),Frame(1,1)))
    assert(game.sumResultFrames(game.scoreFrames,0) == 20)
  }

  def testTheUpdate(game : Game): List[Frame] ={
    game.frames.map(x => {
      if(x.firstRoll == 10){
        x.copy(scoreFrame=x.firstRoll,bonus = "strikes")
      }else if(x.firstRoll + x.secondRoll == 10){
        x.copy(scoreFrame=x.firstRoll + x.secondRoll, bonus = "spares")
      }else{
        x.copy(scoreFrame=x.firstRoll + x.secondRoll)
      }
    })
  }

  it("it should edit the property bonus of a Frame asa strikes") {
    val game = Game(frames = List(Frame(10,0),Frame(5,4)))
    val result =  testTheUpdate(game)
    assert(result == List(Frame(10,0,"strikes",10),Frame(5,4,null,9)))
  }

  it("it should edit the property bonus of a Frame as spares") {
    val game = Game(frames = List(Frame(5,5),Frame(5,4)))
    val result = testTheUpdate(game)
    assert(result == List(Frame(5,5,"spares",10),Frame(5,4,null,9)))
  }

  it("it should update the score result by taking values from others Frames (strikes)") {
    val game = Game(frames = List(Frame(10,0),Frame(10,0),Frame(10,0)))
    val result =  game.updateFramesWithBonus(testTheUpdate(game))
    assert(result == List(Frame(10,0,"strikes",30),Frame(10,0,"strikes",30),Frame(10,0,"strikes",30)))
  }

  it("it should update the score result by taking values from others Frames (spares)") {
    val game = Game(frames = List(Frame(5,5),Frame(5,5),Frame(5,0)))
    val result =  game.updateFramesWithBonus(testTheUpdate(game))
    assert(result == List(Frame(5,5,"spares",15),Frame(5,5,"spares",15),Frame(5,0,null,5)))
  }

  it("it should compute the score of a game (result 35)") {
    val game = Game(frames = List(Frame(5,5),Frame(5,5),Frame(5,0)))
    val result =  game.updateFramesWithBonus(testTheUpdate(game))
    assert(game.sumResultFrames(result,0) == 35)
  }

  it("it should compute the score of a game (result 60)") {
    val game = Game(frames = List(Frame(10,0),Frame(10,0),Frame(10,0)))
    val result =  game.updateFramesWithBonus(testTheUpdate(game))
    assert(game.sumResultFrames(result,0) == 90)
  }

}
