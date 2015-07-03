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

		ctx.font = "75px sans-serif"


		//Make the game
		val g = new Game(GVs.GAMEX, GVs.GAMEY)

		//Make the player
		//val player = 
		//g.addActor(player)

		//Make the map
		g.genMap()

		//MAKEA THA ZOMBIE
		val zed = new Zombie(GVs.GAMEX / 2 - 10, GVs.GAMEY - 50, 20, g.player)
		g.addActor(zed)

		//make a wall
		val wall = new Wall(200, 400, 200, 10)
		g.addObj(wall)

		val zedSpawner = new ZombieSpawner(GVs.GAMEX / 2 - 10, GVs.GAMEY - 50, 20)
		g.addActor(zedSpawner)


		def clear() = 
		{
			ctx.fillStyle = s"rgb(200, 200, 200)"
			ctx.fillRect(0, 0, GVs.FULLX, GVs.FULLY)
		}

		def run()
		{
			//Check if the game is over
			if(g.player.hp <= 0)
			{
				//it's over
				ctx.fillStyle = "white"
				ctx.fillText("It's over!", GVs.GAMEX/2, GVs.GAMEY/2)
			}
			else
			{
				//dom.console.log("py: " + g.player.locY)
				//Check if the player has moved to the next map
				if(g.player.locY < 0)
				{
					dom.console.log("Generating new map")

					val oldActs = g.acts.clone

					//Load new map
					g.genMap()

					dom.console.log("Copying " + oldActs.size + " actors")

					//Add all the actors as delays
					for(a <- oldActs)
					{
						if(a != g.player)
						{
							a match
							{
								case delayed : DelayedActor =>
									delayed.time += GVs.GAMEY
									g.addActor(delayed)
								case _ =>
									val time = a.locY

									//Move them to valid spots
									a.locY = GVs.GAMEY //all the way down
									a.locX = max(a.locX, GVs.GAMEX / 2 - 50) //minimum right they can be
									a.locX = min(a.locX, GVs.GAMEX/2 + 50 - a.sizeX)

									val delayedAct = new DelayedActor(a, time)

									g.addActor(delayedAct)
							}
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
				ctx.fillStyle = s"rgb(200, 0, 0)"
				ctx.fillRect(GVs.GAMEX, 0, max((GVs.FULLX - GVs.GAMEX) * g.player.hp * 1.0/g.player.maxHp, 0), 80)
				

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
