package objects

import org.scalajs.dom
import scala.math.{abs, signum, sqrt, pow, min, max, round}

object GVs
{
	val GAMEX = 600
	val GAMEY = 600

	val FULLX = 800
	val FULLY = 800

	val NORMUNITSIZE = 20
}





case class SimpleLine(start : (Int, Int), end : (Int, Int), color : String)
{
	def length() =
	{
		val dist = sqrt(pow(start._1 - end._1, 2) + pow(start._2 - end._2, 2))
		dist
	}

	def points() =
	{
		dom.console.log("points from " + start + " to " + end)
		var cx : Double = start._1.toDouble
		var cy : Double = start._2.toDouble

		val dx = (end._1 - start._1) / length
		val dy = (end._2 - start._2) / length

		val ps  = scala.collection.mutable.Buffer[(Int, Int)]()
		while(round(cx) != end._1 || round(cy) != end._2)
		{
			dom.console.log(cx + ", " + cy)
			val pair = (round(cx).toInt, round(cy).toInt)
			ps += pair
			cx += dx
			cy += dy
		}

		ps
	}
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

	def inside(x : Int, y : Int) : Boolean = 
	{
		if(x > locX &&
			x < locX + sizeX &&
			y > locY &&
			y < locY + sizeY)
		{
			return true
		}
		return false
	}

	def collidesLine(l : SimpleLine) : Boolean =
	{
		//dom.console.log("Checking " + (locX, locY))
	    // Completely outside.
	    if ((l.start._1 <= locX && l.end._1 <= locX) || 
	    	(l.start._2 <= locY && l.end._2 <= locY) || 
	    	(l.start._1 >= locX + sizeX && l.end._1 >= locX + sizeX) || 
	    	(l.start._2 >= locY + sizeY && l.end._2 >= locY + sizeY))
	        return false

	    val m = (l.end._2 - l.start._2) * 1.0 / (l.end._1 - l.start._1)

	    //Left wall
	    val y1 = m * (locX - l.start._1) + l.start._2
	    if (y1 > locY && y1 < locY + sizeY) return true

	    //right wall
	    val y2 = m * (locX + sizeX - l.start._1) + l.start._2
	    if (y2 > locY && y2 < locY + sizeY) return true

	    //top wall
	    val x1 = (locY - l.start._2) / m + l.start._1
	    if (x1 > locX && x1 < locX + sizeX) return true

	    //bot wall
	    val x2 = (locY + sizeY - l.start._2) / m + l.start._1
	    if (x2 > locX && x2 < locX + sizeX) return true

	    return false
	}

	def pointSetCollides(ps : scala.collection.mutable.Seq[(Int, Int)]) : Boolean = 
	{
		val filtered = ps filter(p => ! inside(p._1, p._2))
		return filtered.length == ps.length
	}

	def hasLosTo(o : Obj, g : Game) : Boolean =
	{
		val l = SimpleLine(center, o.center, "black")

		for(ob <- g.objs)
		{
			if(ob != this && ob != o && ob.collidesLine(l)) return false
		}
		
		return true
	}

	def draw(ctx : dom.CanvasRenderingContext2D) =
	{
	}

	def center() = 
	{
		(locX + sizeX / 2, locY + sizeY / 2)
	}

	def distanceTo(o : Obj) : Int =
	{
		return sqrt(pow(o.locX - locX, 2) + pow(o.locY - locY, 2)).toInt
	}
}




class Actor(locX_ : Int, locY_ : Int, 
			sizeX_ : Int, sizeY_ : Int,
			hp_ : Int, speed_ : Int,
			faction_ : String, name_ : String)
extends Obj(locX_, locY_, sizeX_, sizeY_)
{
	var maxHp = hp_
	var hp = hp_
	var name = name_
	var speed = speed_

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
				//dom.console.log(name + " can't move there!")
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

	def getClosestEnemyInLOS(g : Game) : Actor = 
	{
		var closestAct : Actor = null
		var distance = 100000
		for(a <- g.acts)
		{
			if(a.faction != "NA" && a.faction != faction &&
				hasLosTo(a, g))
			{
				val dist = distanceTo(a)
				if(dist < distance) //new closest
				{
					closestAct = a
					distance = dist.toInt //TODO: CHECK FOR LINE OF SIGHT
				}
			}
		}

		return closestAct
	}

	def moveToNewMap(g : Game)
	{
		//meh
	}
}

class BaseHuman(locX_ : Int, locY_ : Int, 
			hp_ : Int)
extends Actor(locX_, locY_, GVs.NORMUNITSIZE, GVs.NORMUNITSIZE, hp_, 2, "human", "human")
{
	var timeToNextShot = 0
	var shotCooldown = 20

	var gun = new Gun(20, 10, 200, this)

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

		gun.tick()

		//Now, check if we can SHOOT STUFF
		if(gun.canShoot)
		{
			val closestAct = getClosestEnemyInLOS(g)
			if(closestAct != null)
			{
				val distance =  distanceTo(closestAct)

				if(distance <= gun.range)
				{
					gun.shoot(closestAct, g)
				}
			}
		}
	}
}

class Human(locX_ : Int, locY_ : Int, 
			hp_ : Int)
extends BaseHuman(locX_, locY_, hp_)
{
	var dest = (-1, -1)

	override def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = "red"
		ctx.fillRect(locX, locY, sizeX, sizeY)
	}

	override def aiMove(g : Game) =
	{
		//Bot humans do everything player humans do except for movement
		super.aiMove(g)

		dom.console.log("Hooman: " + (locX, locY))

		//Now decide where to go!
		if(dest == (-1, -1))
		{
			//pick a new dest!
			dest = (g.player.locX + g.r.nextInt(200) - 100, g.player.locY + g.r.nextInt(200) - 100)
		}

		//Now cheap pathfinding as usual
		var dx = min(dest._1 - locX, 2)
		dx = max(dx, -2)
		
		var dy = min(dest._2 - locY, 2)
		dy = max(dy, -2)

		if(! moveLoc(dx, dy, g))
		{
			dest = (-1, -1)
		}
	}

	override def moveToNewMap(g : Game)
	{
		dest = (-1, -1)
	}
}

class Gun(firingSpeed_ : Int, damage_ : Int, range_ : Int, owner_ : Actor)
{
	val firingSpeed = firingSpeed_
	val damage = damage_
	val range = range_

	var owner = owner_

	var shotCountdown = 0

	def canShoot() =
	{
		shotCountdown == 0
	}

	def shoot(target : Actor, g : Game)
	{
		//SHOOOOT ITTTT
		target.takeDamage(owner, damage, g)
		shotCountdown = firingSpeed

		//add it to the graphics
		g.linesToDraw += SimpleLine(owner.center(), target.center(), "black")
	}

	def tick()
	{
		if(shotCountdown > 0) shotCountdown -= 1
	}
}



class Zombie(locX_ : Int, locY_ : Int, 
			hp_ : Int,
			target_ : Actor)
extends Actor(locX_, locY_, GVs.NORMUNITSIZE, GVs.NORMUNITSIZE, hp_, 1, "zombie", "zombie")
{
	var target = target_

	var direction = (0, 0)

	val chanceToChangeDirection = 400

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

		//Can we see the player?
		if(hasLosTo(g.player, g))
		{
			val dest = (g.player.locX, g.player.locY)

			val dx = signum(target.locX - locX)
			val dy = signum(target.locY - locY)

			direction = (dx, dy)
		}
		else if((0, 0) == direction)
		{
			//we're stuck, wander in a different direction
			direction = (g.r.nextInt(3) - 1, g.r.nextInt(3) - 1)
		}		

		//try to move there, see if it works
		if(! moveLoc(direction._1, direction._2, g))
		{
			direction = (0, 0)
		}
		else
		{
			//even if it worked, we sometimes wanna quit
			if(g.r.nextInt(chanceToChangeDirection) == 0)
			{
				direction = (0, 0)
			}
		}
		
		if(target.collides(locX + direction._1, locY + direction._2, sizeX, sizeY))
		{
			//Hell yeah we are, bit him!
			//g.player.hp -= 10 //ZOMBIEDAMAGE
			target.takeDamage(this, 10, g)
		}
	}
}

class ZombieSpawner(locX_ : Int, locY_ : Int, 
			spawnRate_ : Int)
extends Actor(locX_, locY_, GVs.NORMUNITSIZE, GVs.NORMUNITSIZE, -1, 1, "NA", "zombie spawner")
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

class DelayedActor(act_ : Actor, delayTime_ : Int)
extends Actor(-1, -1, 0, 0, -1, act_.speed, "none", "delayed " + act_.name)
{
	val act = act_
	var time = delayTime_

	override def draw(ctx: dom.CanvasRenderingContext2D) =
	{
		//nothin'
	}

	override def aiMove(g : Game)
	{
		//Tick
		if(time <= 0)
		{
			if(! g.collision(act))
			{
				//Spawn the dude
				g.addActor(act)
				g.removeActor(this)
			}
			else
			{
				//dom.console.log(act.name + " unable to spawn!")
			}
		}
		else
		{
			time -= 1
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
	var difficulty = 0
	var score = 0

	val r = new scala.util.Random()

	val mapSizeX = mSizeX
	val mapSizeY = mSizeY

	//graphics-related
	val linesToDraw = scala.collection.mutable.Buffer[SimpleLine]()

	//all the things
	val objs = scala.collection.mutable.Set[Obj]()
	
	val acts = scala.collection.mutable.Set[Actor]()

	//make the player
	val player = new BaseHuman(GVs.GAMEX / 2, GVs.GAMEY/2, 100)
	player.name = "Tim Jones"
	addActor(player)

	def collision(ob : Obj) : Boolean =
	{
		for(o <- objs)
		{
			if(o != ob && o.blocksMovement && o.collides(ob))
			{
				//dom.console.log(ob.getClass + " collision with " + o.getClass)
				return true
			}
		}
		return false
	}


	def addObj(newObj : Obj) =
	{
		objs += newObj
	}

	def addActor(newAct : Actor) = 
	{
		acts += newAct
		objs += newAct
	}

	def removeActor(act : Actor) =
	{
		acts remove act
		objs remove act
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
		dom.console.log("Generating map with difficulty " + difficulty)
		objs.clear()
	
		acts.clear()

		if(difficulty != 0) player.changeLoc(player.locX, GVs.GAMEY - 50)
		addActor(player)

		//Bounding walls
		val topWallLeft = new Wall(0, 0, GVs.GAMEX/2 - 50, 5)
		val topWallRight = new Wall(GVs.GAMEX/2 + 50, 0, GVs.GAMEX/2 - 50, 5)
		val leftWall = new Wall(0, 0, 5, GVs.GAMEY)
		val rightWall = new Wall(GVs.GAMEX - 5, 0, 5, GVs.GAMEY)
		val botWallLeft = new Wall(0, GVs.GAMEY - 5, GVs.GAMEX / 2 - 50, 5)
		val botWallRight = new Wall(GVs.GAMEX/2 + 50, GVs.GAMEY - 5, GVs.GAMEX/2 - 50, 5)

		addObj(topWallLeft)
		addObj(topWallRight)
		addObj(leftWall)
		addObj(rightWall)
		addObj(botWallLeft)
		addObj(botWallRight)

		val topTopBound = new Wall(GVs.GAMEX / 2 - 50, -20, 100, 5)
		val topLeftBound = new Wall(GVs.GAMEX / 2 - 55, -20, 5, 20)
		val topRightBound = new Wall(GVs.GAMEX / 2 + 50, -20, 5, 20)

		val botBotBound = new Wall(GVs.GAMEX / 2 - 50, GVs.GAMEY + 20, 100, 5)
		val botLeftBound = new Wall(GVs.GAMEX / 2 - 55, GVs.GAMEY, 5, 20)
		val botRightBound = new Wall(GVs.GAMEX / 2 + 50, GVs.GAMEY, 5, 20)

		addObj(topTopBound)
		addObj(topLeftBound)
		addObj(topRightBound)
		addObj(botBotBound)
		addObj(botLeftBound)
		addObj(botRightBound)

		
		//More stuff!
		//Add random walls
		//TODO: make sure there's a clear path to the next level
		for(i <- 1 to difficulty)
		{
			val r = scala.util.Random
			val width = r.nextInt(80) + 20
			val height = r.nextInt(80) + 20
			val x = r.nextInt(GVs.GAMEX - width)
			val y = r.nextInt(GVs.GAMEY - height - GVs.GAMEY / 5)
			//Random wall
			val randWall = new Wall(x, y, width, height)
			addObj(randWall)
		}

		//Add random enemies
		for(i <- 1 to difficulty * 3)
		{
			val r = scala.util.Random
			val x = r.nextInt(GVs.GAMEX - GVs.NORMUNITSIZE)
			val y = r.nextInt(GVs.GAMEY - GVs.NORMUNITSIZE - GVs.GAMEY/4)
			//Random wall
			val randDude = new Zombie(x, y, 20, player)
			if(! collision(randDude))
			{
				addActor(randDude)
			}
		}


		//Now, increase the difficulty
		difficulty += 1
	}
}