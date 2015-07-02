package objects

import org.scalajs.dom
import scala.math.{abs, signum, sqrt, pow}

object GVs
{
	val GAMEX = 600
	val GAMEY = 600

	val FULLX = 800
	val FULLY = 800

	val NORMUNITSIZE = 20
}

case class SimpleLine(start : (Int, Int), end : (Int, Int), color : String)


class Obj(locX_ : Int, locY_ : Int, sizeX_ : Int, sizeY_ : Int)
{
	val sizeX = sizeX_
	val sizeY = sizeY_
	var locX = locX_
	var locY = locY_

	var blocksMovement = true

	def collides(loX : Int, loY : Int, sX : Int, sY : Int) : Boolean =
	{
		//return abs(locX - loX) * 2 < sizeX + sX && abs(locY - loY) * 2 < sizeY + sY
		if(locX < loX + sX &&
			locX + sizeX > loX &&
			locY < loY + sY &&
			locY + sizeY > loY)
		{
			return true
		}
		return false
	}

	def collides(other : Obj) : Boolean =
	{
		collides(other.locX, other.locY, other.sizeX, other.sizeY)
	}

	def draw(ctx : dom.CanvasRenderingContext2D) =
	{
	}
}




class Actor(locX_ : Int, locY_ : Int, 
			sizeX_ : Int, sizeY_ : Int,
			hp_ : Int, faction_ : String,
			name_ : String)
extends Obj(locX_, locY_, sizeX_, sizeY_)
{
	var maxHp = hp_
	var hp = hp_
	var name = name_

	var faction = faction_

	var momentum = (0, 0)

	def changeLoc(newX : Int, newY : Int) =
	{
		locX = newX
		locY = newY
	}

	def canMoveTo(newX : Int, newY : Int, g : Game) : Boolean = 
	{
		for(o <- g.objs)
		{
			if(o != this && o.blocksMovement && o.collides(newX, newY, sizeX, sizeY))
			{
				dom.console.log(name + " can't move there!")
				return false
			}
		}
		return true
	}

	def moveLoc(dx : Int, dy : Int, g : Game) : Boolean =
	{
		var moved = false
		//Check collisions
		if(dx != 0 && canMoveTo(locX + dx, locY, g)) //check x movement
		{
			locX += dx
			moved = true
		}

		if(dy != 0 && canMoveTo(locX, locY + dy, g))
		{
			locY += dy
			moved = true
		}
		
		return moved
	}

	override def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = "red"
		ctx.fillRect(locX, locY, sizeX, sizeY)
	}

	//Has the unit's ai pick a move and then does the move
	def aiMove(g : Game) =
	{
		//Knockback
		val changeX = momentum._1 * 3 / 4
		val changeY = momentum._2 * 3 / 4
		moveLoc(changeX, changeY, g)
		momentum = (changeX, changeY)
	}

	def takeDamage(source : Actor, damage : Int, g : Game) =
	{
		dom.console.log(source.name + " does " + damage + " damage to " + name)
		hp -= damage

		//Now compute knockback
		val dx = -1 * signum(source.locX - locX)
		val dy = -1 * signum(source.locY - locY)

		momentum = (dx * damage, dy * damage)

		//Are we dead?
		if(hp <= 0) //we're dead
		{
			g.acts remove this
			g.objs remove this
		}
	}
}

class Human(locX_ : Int, locY_ : Int, 
			hp_ : Int)
extends Actor(locX_, locY_, GVs.NORMUNITSIZE, GVs.NORMUNITSIZE, hp_, "human", "human")
{
	var timeToNextShot = 0
	var shotCooldown = 20
	override def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = "red"
		ctx.fillRect(locX, locY, sizeX, sizeY)
	}

	override def aiMove(g : Game) =
	{
		//Knockback
		val changeX = momentum._1 * 3 / 4
		val changeY = momentum._2 * 3 / 4
		moveLoc(changeX, changeY, g)
		momentum = (changeX, changeY)

		//Now, check if we can SHOOT STUFF
		if(timeToNextShot == 0)
		{
			var closestAct : Actor = g.acts.head
			var distance = 100000
			for(a <- g.acts)
			{
				if(a.faction != "NA" && a.faction != faction)
				{
					val dist = sqrt(pow(a.locX - locX, 2) + pow(a.locY - locY, 2))
					if(dist < distance) //new closest
					{
						closestAct = a
						distance = dist.toInt //TODO: CHECK FOR LINE OF SIGHT
					}
				}
			}

			if(distance <= 200)
			{
				//SHOOOOT ITTTT
				closestAct.takeDamage(this, 10, g)
				timeToNextShot = shotCooldown

				//add it to the graphics
				g.linesToDraw += SimpleLine((locX, locY), (closestAct.locX, closestAct.locY), "black")
			}
		}
		else
		{
			timeToNextShot -= 1
		}
	}
}




class Zombie(locX_ : Int, locY_ : Int, 
			hp_ : Int,
			target_ : Actor)
extends Actor(locX_, locY_, GVs.NORMUNITSIZE, GVs.NORMUNITSIZE, hp_, "zombie", "zombie")
{
	var target = target_

	override def draw(ctx: dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = s"rgb(30, 150, 30)"
		ctx.fillRect(locX, locY, sizeX, sizeY)
	}

	override def aiMove(g : Game) =
	{
		//Knockback
		val changeX = momentum._1 * 3 / 4
		val changeY = momentum._2 * 3 / 4
		moveLoc(changeX, changeY, g)
		momentum = (changeX, changeY)


		//Get which way we should move
		val dx = signum(target.locX - locX)
		val dy = signum(target.locY - locY)

		//try to move there, see if it works
		moveLoc(dx, dy, g)
		
		if(target.collides(locX + dx, locY + dy, sizeX, sizeY))
		{
			//Hell yeah we are, bit him!
			//g.player.hp -= 10 //ZOMBIEDAMAGE
			target.takeDamage(this, 10, g)
		}
	}
}

class ZombieSpawner(locX_ : Int, locY_ : Int, 
			spawnRate_ : Int)
extends Actor(locX_, locY_, GVs.NORMUNITSIZE, GVs.NORMUNITSIZE, -1, "NA", "zombie spawner")
{
	val spawnRate = spawnRate_
	var timeToNextSpawn = spawnRate
	blocksMovement = false

	override def draw(ctx: dom.CanvasRenderingContext2D) =
	{
		//Don't draw anything, this is a proxy for zombies coming in off the edge of the map
		//ctx.fillStyle = s"rgb(30, 150, 30)"
		//ctx.fillRect(locX, locY, sizeX, sizeY)
	}

	override def aiMove(g : Game) =
	{
		//if it's time:
		if(timeToNextSpawn == 0)
		{
			//Spawn a zombie if there's room:
			val newZed = new Zombie(-100, -100, 20, g.player)
			if(newZed.canMoveTo(locX, locY, g)) //there's room!
			{
				g.addActor(newZed)
				newZed.changeLoc(locX, locY)
			}

			timeToNextSpawn = spawnRate
		}
		else
		{
			timeToNextSpawn -= 1
		}
	}
}

//Terrain stuff
class Wall(locX_ : Int, locY_ : Int, sizeX_ : Int, sizeY_ : Int) extends Obj(locX_, locY_, sizeX_, sizeY_)
{
	override def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = "blue"
		ctx.fillRect(locX, locY, sizeX, sizeY)
	}
}






class Game(mSizeX : Int, mSizeY : Int)
{
	val mapSizeX = mSizeX
	val mapSizeY = mSizeY

	//graphics-related
	val linesToDraw = scala.collection.mutable.Buffer[SimpleLine]()

	//all the things
	val objs = scala.collection.mutable.Set[Obj]()
	
	val acts = scala.collection.mutable.Set[Actor]()

	//make the player
	val player = new Human(GVs.GAMEX / 2, GVs.GAMEY/2, 100)
	player.name = "Tim Jones"
	addActor(player)


	def addObj(newObj : Obj) =
	{
		objs += newObj
	}

	def addActor(newAct : Actor) = 
	{
		acts += newAct
		objs += newAct
	}

	def runAllAIs() =
	{
		acts map(_.aiMove(this))
	}

	def drawAll(ctx : dom.CanvasRenderingContext2D) =
	{
		//draw lines
		while(! linesToDraw.isEmpty)
		{
			val line = linesToDraw.remove(0)

			val (sx, sy) = line.start
			val (ex, ey) = line.end

			//now draw it
			ctx.beginPath()
			ctx.moveTo(sx, sy)
			ctx.fillStyle = line.color
			ctx.lineTo(ex, ey)
			ctx.stroke
		}

		//Draw all the objects
		objs map(_.draw(ctx))
	}

	def genMap() =
	{
		//Bounding walls
		val topWall = new Wall(0, 0, GVs.GAMEX, 5)
		val leftWall = new Wall(0, 0, 5, GVs.GAMEY)
		val rightWall = new Wall(GVs.GAMEX - 5, 0, 5, GVs.GAMEY)
		val botWall = new Wall(0, GVs.GAMEY - 5, GVs.GAMEX, 5)

		addObj(topWall)
		addObj(leftWall)
		addObj(rightWall)
		addObj(botWall)
	}
}