package example
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

import objects._
import game._
import globalvars._
import enemies._
import allies._
import scala.math.{min, max}



@JSExport
object ScalaJSExample 
{
	def handlePlayerMovement(player : Player, keys : collection.mutable.Set[Int], g : Game) =
	{
		var dx = 0.0
		var dy = 0.0

		if (keys(38)) dy -= player.speed
	    if (keys(37)) dx -= player.speed
	    if (keys(39)) dx += player.speed
	    if (keys(40)) dy += player.speed

	    for(i <- 1 to player.speed.toInt)
	    {
	    	player.moveLoc(dx / player.speed, dy/player.speed, g)
	    }

	    //player.moveLoc(dx, dy, g)

	    if (keys(32)) 
	    {
	    	player.useItem(g)
	    	keys.remove(32) //we don't want to use another item until they release space and repress it
	    }
	}

	@JSExport
	def main(canvas: html.Canvas): Unit = 
	{
		dom.console.log("butts butts butts")
		val keysDown = collection.mutable.Set[Int]()
		val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]


	
		//Make the game
		val g = new Game(GV.GAMEX, GV.GAMEY, ctx)

		//Make the player
		//val player = 
		//g.addActor(player)
		//g.addHeadText(g.player, "hi there", 200)
		//g.player.usable = new UsableLandMine(g.player)

		//Make the map
		g.genMap()

		//MAKEA THA ZOMBIE
		//val zed = new Zombie(GV.GAMEX / 2 - 10, GV.GAMEY - 50, 20, g.player)
		val zed = new Spitter(new Pt(GV.GAMEX / 2 - 10, GV.GAMEY - 50))
		//g.addActor(zed)

		//val item = new LandMine(new Pt(100, 100))
		//g.addActor(item)

		val puddle = new FirePatch(new Pt(100, 100), 100, 5000)
		//g.addActor(puddle)

		//make a hooman
		//var hooman : Actor = null

		val item = new EqHeavyArmor()
		val gitem = new GroundEquip(new Pt(g.player.loc.x + 50, g.player.loc.y), item, "ak47")
		g.addActor(gitem)
		

		//stick a gun on the ground
		//val thegun = new Gun(GV.AK47_FIRETIME, GV.AK47_DAMAGE, GV.AK47_RANGE, GV.AK47_APS, null)
		//val thegun = new Gun(GV.SNIPER_FIRETIME, GV.SNIPER_DAMAGE, GV.SNIPER_RANGE, GV.SNIPER_APS, GV.SNIPER_AIMTIME, null)
		//val grounded = new GroundGun(new Pt(g.player.loc.x + 50, g.player.loc.y), thegun, "ak47")
		//g.addActor(grounded)

		//make a wall
		val wall = new Wall(new Pt(200, 400), new Pt(200, 10))
		//g.addObj(wall)

		val zedSpawner = new ZombieSpawner(GV.ZOMBIESPAWNER_SPAWNRATE)
		g.addActor(zedSpawner)


		def clear() = 
		{
			ctx.fillStyle = s"rgb(200, 200, 200)"
			ctx.fillRect(0, 0, GV.FULLX, GV.FULLY)
		}

		def drawSidebar() =
		{
			//Draw health bars and such
			//clear the background of the sidebar
			ctx.fillStyle = s"rgb(200, 200, 200)"
			ctx.fillRect(GV.GAMEX, 0, GV.FULLX - GV.GAMEX, GV.FULLY)

			var barY = 0

			//health
			ctx.fillStyle = s"rgb(200, 0, 0)"
			ctx.fillRect(GV.GAMEX, barY, max((GV.FULLX - GV.GAMEX) * g.player.hp * 1.0/g.player.maxHp, 0), 80)
			ctx.fillStyle = "black"
			ctx.font = "12px sans-serif"
			ctx.fillText("health", GV.GAMEX, barY + 10)
			barY += 80

			//ammo
			ctx.fillStyle = s"rgb(0, 0, 200)"
			ctx.fillRect(GV.GAMEX, barY, min(g.player.getGun.ammo/3, GV.FULLX - GV.GAMEX), 80)
			ctx.fillStyle = "black"
			ctx.font = "12px sans-serif"
			ctx.fillText("ammo", GV.GAMEX, barY + 10)
			barY += 80

			//score!
			ctx.fillStyle = "black"
			ctx.font = "12px sans-serif"
			ctx.fillText("score", GV.GAMEX, barY + 10)
			ctx.font = "50px sans-serif"
			ctx.fillText(g.score.toString, GV.GAMEX, barY + 50)
			barY += 80

			val endBar = barY

			//Display the player's items
			ctx.font = "12px sans-serif"
			for(item <- g.player.usableItems)
			{
				//draw the item
				val img = g.images(item.displayName)
				g.ctx.drawImage(img, GV.GAMEX + 5, barY, GV.NORMUNITSIZE*2, GV.NORMUNITSIZE*2)

				//write its name
				ctx.fillText(item.name, GV.GAMEX + 30, barY + 12)

				//bump the counter
				barY += GV.NORMUNITSIZE*2 + 5
			}

			barY = endBar
			//draw the player's team!
			for(a <- g.acts)
			{
				a match {
					case h : BaseHuman if h.name != "human" => //draw it
						//draw a tiny health bar
						ctx.fillStyle = s"rgb(200, 0, 0)"
						ctx.fillRect(GV.GAMEX + 100, barY, max(100 * h.hp * 1.0/h.maxHp, 0), 5)

						//draw a tiny ammo bar
						ctx.fillStyle = s"rgb(0, 0, 200)"
						ctx.fillRect(GV.GAMEX + 100, barY + 5, min(h.getGun.ammo / 10, 100), 5)
			
						val img = g.images(h.bigDisplayname)
						g.ctx.drawImage(img, GV.GAMEX + 100, barY + 10, GV.NORMUNITSIZE*2, GV.NORMUNITSIZE*2)

						//write its name
						ctx.fillText(h.name, GV.GAMEX + 125, barY + 22)
						barY += GV.NORMUNITSIZE*2 + 5 + 5 + 5
					case h : TankRider => //also draw it
						//draw a tiny health bar
						ctx.fillStyle = s"rgb(200, 0, 0)"
						ctx.fillRect(GV.GAMEX + 100, barY, max(100 * h.hp * 1.0/h.maxHp, 0), 5)

						val img = g.images(h.bigDisplayname)
						g.ctx.drawImage(img, GV.GAMEX + 100, barY + 5, GV.NORMUNITSIZE*2, GV.NORMUNITSIZE*2)

						//write its name
						ctx.fillText(h.name, GV.GAMEX + 125, barY + 17)
						barY += GV.NORMUNITSIZE*2 + 5 + 5
					case _ => //don't draw anything else
				}	
			}

			//draw a separatoer
			barY += 5
			ctx.fillStyle = s"rgb(0, 0, 0)"
			ctx.fillRect(GV.GAMEX + 100, barY, 100, 3)
			barY += 8

			//draw the player's items
			for((slot, i) <- g.player.equips)
			{
				//dom.console.log(i.name)
				//draw the item
				val img = g.images(i.displayName)
				g.ctx.drawImage(img, GV.GAMEX + 100, barY, GV.NORMUNITSIZE*2, GV.NORMUNITSIZE*2)

				//write its name
				ctx.fillText(i.name, GV.GAMEX + 125, barY + 12)

				//bump the counter
				barY += GV.NORMUNITSIZE*2 + 5 + 5
			}
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
				ctx.fillStyle = "black"
				ctx.fillText("It's over!", GV.GAMEX/2, GV.GAMEY/2)
			}
			else
			{
				//dom.console.log("py: " + g.player.locY)
				//Check if the player has moved to the next map
				if(g.player.loc.y < 0)
				{
					g.loadNewMap()
				}
				
				//handle player movement
				handlePlayerMovement(g.player, keysDown, g)

				//Now handle all other ais
				g.runAllAIs()

				//Clear the screen
				clear()

				//Draw the map
				g.drawAll()

				drawSidebar()
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


		dom.setInterval(() => run, 25)
	}
}
