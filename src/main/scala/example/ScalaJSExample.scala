package example
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

import objects._




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
		val g = new Game(GVs.GAMEX, GVs.GAMEY)

		//Make the player
		//val player = 
		//g.addActor(player)

		//Make the map
		g.genMap()

		//MAKEA THA ZOMBIE
		//val zed = new Zombie(20, 20, GVs.NORMUNITSIZE, GVs.NORMUNITSIZE, 20, player)
		//g.addActor(zed)

		val zedSpawner = new ZombieSpawner(20, 20, 20)
		g.addActor(zedSpawner)


		def clear() = 
		{
			ctx.fillStyle = "black"
			ctx.fillRect(0, 0, GVs.GAMEX, GVs.GAMEY)
		}

		def run()
		{
			//handle player movement
			handlePlayerMovement(g.player, keysDown, g)

			//Now handle all other ais
			g.runAllAIs()

			//Clear the screen
			clear()

			//Draw the map
			g.drawAll(ctx)
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
