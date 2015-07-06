package example
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

import objects._
import scala.math.{min, max}



@JSExport
object ScalaJSExample 
{
	def handlePlayerMovement(player : Actor, keys : collection.mutable.Set[Int], g : Game) =
	{
		if (keys(38)) player.moveLoc(0, -2, g)
	    if (keys(37)) player.moveLoc(-2, 0, g)
	    if (keys(39)) player.moveLoc(2, 0, g)
	    if (keys(40)) player.moveLoc(0, 2, g)
	}

	@JSExport
	def main(canvas: html.Canvas): Unit = 
	{
		dom.console.log("butts butts butts")
		val keysDown = collection.mutable.Set[Int]()
		val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

	
		//Make the game
		val g = new Game(GV.GAMEX, GV.GAMEY)

		//Make the player
		//val player = 
		//g.addActor(player)

		//Make the map
		g.genMap()

		//MAKEA THA ZOMBIE
		//val zed = new Zombie(GV.GAMEX / 2 - 10, GV.GAMEY - 50, 20, g.player)
		val zed = new Spitter(new Pt(GV.GAMEX / 2 - 10, GV.GAMEY - 50))
		g.addActor(zed)

		val puddle = new CausticAcid(new Pt(100, 100), 70, 5)
		//g.addActor(puddle)

		//make a hooman
		val hooman = new Human(new Pt(g.player.loc.x + 50, g.player.loc.y), 50)
		g.addActor(hooman)

		//make a wall
		val wall = new Wall(new Pt(200, 400), new Pt(200, 10))
		g.addObj(wall)

		val zedSpawner = new ZombieSpawner(new Pt(GV.GAMEX / 2 - 10, GV.GAMEY - 50), 20)
		g.addActor(zedSpawner)


		def clear() = 
		{
			ctx.fillStyle = s"rgb(200, 200, 200)"
			ctx.fillRect(0, 0, GV.FULLX, GV.FULLY)
		}

		def run()
		{
			//bump score
			//g.score += 1

			//Check if the game is over
			if(g.player.hp <= 0)
			{
				//it's over
				ctx.font = "75px sans-serif"
				ctx.fillStyle = "white"
				ctx.fillText("It's over!", GV.GAMEX/2, GV.GAMEY/2)
			}
			else
			{
				//dom.console.log("py: " + g.player.locY)
				//Check if the player has moved to the next map
				if(g.player.loc.y < 0)
				{
					//score boost!
					g.score += g.difficulty * 10

					dom.console.log("Generating new map")

					//save actors because genMap deletes them
					val oldActs = g.acts.clone

					//Load new map
					g.genMap()

					dom.console.log("Copying " + oldActs.size + " actors")

					//Update the delays on all our delayed guys
					for(delAct <- g.delayedActs)
					{
						delAct.time += GV.GAMEY / delAct.speed
					}

					//Add all the actors as delays
					for(a <- oldActs)
					{
						if(a != g.player)
						{
							val time : Int = (a.loc.y / a.speed).toInt

							//Move them to valid spots
							a.loc.y = GV.GAMEY //all the way down
							a.loc.x = max(a.loc.x, GV.GAMEX / 2 - 50) //minimum right they can be
							a.loc.x = min(a.loc.x, GV.GAMEX/2 + 50 - a.size.x)

							a.moveToNewMap(g)

							val delayedAct = new DelayedActor(a, time)

							g.addDelayed(delayedAct)
						}
					}

					
				}
				//handle player movement
				handlePlayerMovement(g.player, keysDown, g)

				//Now handle all other ais
				g.runAllAIs()

				//Clear the screen
				clear()

				//Draw health bars and such

				//health
				ctx.fillStyle = s"rgb(200, 0, 0)"
				ctx.fillRect(GV.GAMEX, 0, max((GV.FULLX - GV.GAMEX) * g.player.hp * 1.0/g.player.maxHp, 0), 80)
				ctx.fillStyle = "black"
				ctx.font = "12px sans-serif"
				ctx.fillText("health", GV.GAMEX, 10)

				//score!
				ctx.fillStyle = "black"
				ctx.font = "12px sans-serif"
				ctx.fillText("score", GV.GAMEX, 90)
				ctx.font = "50px sans-serif"
				ctx.fillText(g.score.toString, GV.GAMEX, 130)
				

				//Draw the map
				g.drawAll(ctx)
			}
		}

		clear()


		// dom.onkeypress = {(e: dom.KeyboardEvent) =>
	 //      if (e.keyCode.toInt == 32) bullets = player +: bullets
	 //    }
	    dom.onkeydown = {(e: dom.KeyboardEvent) =>
	      keysDown.add(e.keyCode.toInt)
	    }
	    
	    dom.onkeyup = {(e: dom.KeyboardEvent) =>
	      keysDown.remove(e.keyCode.toInt)
	    }


		dom.setInterval(() => run, 20)
	}
}
