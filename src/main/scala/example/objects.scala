package objects

import org.scalajs.dom
import scala.math.{abs, signum}

object GVs
{
	val GAMEX = 600
	val GAMEY = 600

	val NORMUNITSIZE = 20
}




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
			hp_ : Int, name_ : String)
extends Obj(locX_, locY_, sizeX_, sizeY_)
{
	var maxHp = hp_
	var hp = hp_
	var name = name_

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
				dom.console.log(name + "can't move there!")
				return false
			}
		}
		return true
	}

	def moveLoc(dx : Int, dy : Int, g : Game) : Boolean =
	{
		var moved = false
		//Check collisions
		if(canMoveTo(locX + dx, locY, g)) //check x movement
		{
			locX += dx
			moved = true
		}

		if(canMoveTo(locX, locY + dy, g))
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
	def aiMove(g : Game) = {}
}



class Zombie(locX_ : Int, locY_ : Int, 
			sizeX_ : Int, sizeY_ : Int,
			hp_ : Int,
			target_ : Actor)
extends Actor(locX_, locY_, sizeX_, sizeY_, hp_, "zombie")
{
	var target = target_

	override def draw(ctx: dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = s"rgb(30, 150, 30)"
		ctx.fillRect(locX, locY, sizeX, sizeY)
	}

	override def aiMove(g : Game) =
	{
		//Get which way we should move
		val dx = signum(target.locX - locX)
		val dy = signum(target.locY - locY)

		moveLoc(dx, dy, g)
	}
}

class ZombieSpawner(locX_ : Int, locY_ : Int, 
			spawnRate_ : Int)
extends Actor(locX_, locY_, GVs.NORMUNITSIZE, GVs.NORMUNITSIZE, -1, "zombie spawner")
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
			val newZed = new Zombie(-100, -100, GVs.NORMUNITSIZE, GVs.NORMUNITSIZE, 20, g.player)
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

	//all the things
	val objs = scala.collection.mutable.Set[Obj]()
	
	val acts = scala.collection.mutable.Set[Actor]()

	//make the player
	val player = new Actor(GVs.GAMEX / 2, GVs.GAMEY/2, GVs.NORMUNITSIZE, GVs.NORMUNITSIZE, 100, "Tim Jones")
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
		for(a <- acts)
		{
			a.aiMove(this)
		}
	}

	def drawAll(ctx : dom.CanvasRenderingContext2D) =
	{
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