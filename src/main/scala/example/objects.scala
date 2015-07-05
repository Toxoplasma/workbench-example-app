package objects

import org.scalajs.dom
import scala.math.{abs, signum, sqrt, pow, min, max, round}

object GV
{
	val GAMEX = 600
	val GAMEY = 600

	val FULLX = 800
	val FULLY = 800

	val NORMUNITSIZE = 10
	val BIGUNITSIZE = 12
}

case class Pt(x : Int, y : Int)

case class PtD(x : Double, y : Double)


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

	def unitStep() : (Double, Double) =
	{
		val ux = (end._1 - start._1) / length
		val uy = (end._2 - start._2) / length

		return (ux, uy)
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

	def distanceTo(p : Pt) : Int =
	{
		return sqrt(pow(p.x - locX, 2) + pow(p.y - locY, 2)).toInt
	}
}




class Actor(locX_ : Int, locY_ : Int, 
			sizeX_ : Int, sizeY_ : Int,
			hp_ : Int, speed_ : Int,
			faction_ : String, points_ : Int, name_ : String)
extends Obj(locX_, locY_, sizeX_, sizeY_)
{
	var maxHp = hp_
	var hp = hp_
	var name = name_
	var speed = speed_
	var points = points_

	var faction = faction_

	var momentum = (0, 0)

	var important = false

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

		push(source, damage, g)

		//Are we dead?
		if(hp <= 0) //we're dead
		{
			g.acts remove this
			g.objs remove this

			g.score += points
		}
	}

	def push(source : Actor, amount : Int, g : Game) =
	{
		//Now compute knockback
		val dx = -1 * signum(source.locX - locX)
		val dy = -1 * signum(source.locY - locY)

		momentum = (dx * amount, dy * amount)
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
extends Actor(locX_, locY_, GV.NORMUNITSIZE, GV.NORMUNITSIZE, hp_, 2, "human", 0, "human")
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

		//dom.console.log("Hooman: " + (locX, locY))

		//Now decide where to go!
		if(dest == (-1, -1))
		{
			//pick a new dest!
			val destX = g.player.locX + g.r.nextInt(GV.NORMUNITSIZE * 10) - GV.NORMUNITSIZE*5
			val destY = g.player.locY + g.r.nextInt(GV.NORMUNITSIZE * 10) - GV.NORMUNITSIZE*5
			dest = (destX, destY)
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



class Zombie(locX_ : Int, locY_ : Int)
extends Actor(locX_, locY_, 
	GV.NORMUNITSIZE, GV.NORMUNITSIZE, 
	20, 1, 
	"zombie", 1, "zombie")
{
	var target : Actor = null

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

		//Get closest in-LOS enemy
		target = getClosestEnemyInLOS(g)
		if(target != null)
		{
			val dest = (target.locX, target.locY)

			val dx = signum(dest._1 - locX)
			val dy = signum(dest._2 - locY)

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
		
		if(target != null && target.collides(locX + direction._1, locY + direction._2, sizeX, sizeY))
		{
			//Hell yeah we are, bit him!
			//g.player.hp -= 10 //ZOMBIEDAMAGE
			target.takeDamage(this, 10, g)
		}
	}
}

class Charger(locX_ : Int, locY_ : Int)
extends Actor(locX_, locY_, 
	GV.BIGUNITSIZE, GV.BIGUNITSIZE, 
	200, 1, 
	"zombie", 5, "charger")
{
	var direction = (0, 0)

	val chanceToChangeDirection = 400

	var state = "moving"
	var chargeLine : SimpleLine = null
	var chargeError = PtD(0.0, 0.0)
	var chargeCooldown = 400
	var chargeTimer = 0
	var chargeRange = 400

	val chargeSpeed = 4

	override def draw(ctx: dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = s"rgb(100, 150, 100)"
		ctx.fillRect(locX, locY, sizeX, sizeY)
	}

	override def aiMove(g : Game) =
	{
		//we don't do knockback while charging

		//tick charge timer
		chargeTimer = max(0, chargeTimer - 1)

		if(state == "moving")
		{
			//Knockback
			val changeX = momentum._1 * 2 / 4
			val changeY = momentum._2 * 2 / 4
			moveLoc(changeX, changeY, g)
			momentum = (changeX, changeY)


			//Get closest in-LOS enemy
			val closestAct = getClosestEnemyInLOS(g)
			if(closestAct != null) //possible charge
			{
				chargeLine = SimpleLine((locX, locY), (closestAct.locX, closestAct.locY), "black")
			}


			if(chargeLine != null && chargeLine.length <= chargeRange && chargeTimer <= 0) //CHARGE!
			{
				state = "charging"
				dom.console.log("Charging")
				chargeTimer = chargeCooldown
			}
			else
			{
				//we're not charging, proceed as normal
				if((0, 0) == direction)
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
			}
		}
		else if(state == "charging")
		{
			if(distanceTo(Pt(chargeLine.start._1, chargeLine.start._2)) > chargeRange)
			{
				//we've gone too far, stop charging
				state = "moving"
				//dom.console.log("Charging->Moving, too far from start")
				chargeLine = null
				chargeError = PtD(0.0, 0.0)
			}
			else
			{
				for(i <- 1 to chargeSpeed)
				{
					if(state == "charging") //if we hit someone we don't want to keep taking steps
					{
						//take a step along the line
						val (dx, dy) = chargeLine.unitStep
						chargeError = PtD(chargeError.x + dx, chargeError.y + dy)

						val adx = round(chargeError.x).toInt
						val ady = round(chargeError.y).toInt

						//dom.console.log("ad: " + (adx, ady))

						chargeError = PtD(chargeError.x - adx, chargeError.y - ady)

						//try to move in that direction
						moveLoc(adx, ady, g)
						
						//did we crash into an actor?
						for(a <- g.acts)
						{
							if(a != this && a.collides(locX + adx, locY + ady, sizeX, sizeY))
							{
								//we did!
								//knock 'em up
								a.takeDamage(this, 10, g)
								a.push(this, 40, g)
								state = "moving" //we hit someone
								//dom.console.log("Charging->Moving, hit someone")
							}
						}
					}
				}
			}
		}
	}
}

class ZombieSpawner(locX_ : Int, locY_ : Int, 
			spawnRate_ : Int)
extends Actor(locX_, locY_, GV.NORMUNITSIZE, GV.NORMUNITSIZE, -1, 1, "NA", 0, "zombie spawner")
{
	val spawnRate = spawnRate_
	var timeToNextSpawn = spawnRate
	blocksMovement = false
	important = true

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
			val newZed = new Zombie(-100, -100)
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
extends Actor(-1, -1, 0, 0, -1, act_.speed, "none", 0, "delayed " + act_.name)
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
				g.removeDelayed(this)
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

	def splitWithDoorAt(i : Int, doorSize : Int) : (Wall, Wall) = 
	{
		//horizontal
		if(sizeX >= sizeY)
		{
			val w1 = new Wall(locX, locY, i, sizeY)
			val w2 = new Wall(locX + i + doorSize, locY, sizeX - i - doorSize, sizeY)
			return (w1, w2)
		}
		else //vertical
		{
			val w1 = new Wall(locX, locY, sizeX, i)
			val w2 = new Wall(locX, locY + i + doorSize, sizeX, sizeY - i - doorSize)
			return (w1, w2)
		}
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
	val delayedActs = scala.collection.mutable.Set[DelayedActor]()

	//make the player
	val player = new BaseHuman(GV.GAMEX / 2, GV.GAMEY/2, 100)
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

	def addDelayed(newDel : DelayedActor) =
	{
		delayedActs += newDel
	}
	def removeDelayed(del : DelayedActor) =
	{
		delayedActs remove del
	}

	def removeActor(act : Actor) =
	{
		acts remove act
		objs remove act
	}


	def runAllAIs() =
	{
		acts map(_.aiMove(this))
		delayedActs map(_.aiMove(this))
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

	def genFarms2() =
	{
		//make a starter room
		val starter = new Wall(100, 100, 100, 100)

		objs += starter

		for(i <- 1 to 10)
		{
			//pick which wall we're going to add it to
			//val newRoom = new Wall(0, 0, )
		}
	}

	def genFarms() =
	{
		def moveRoom(walls : scala.collection.mutable.Set[Wall], x : Int, y : Int) =
		{
			for(w <- walls)
			{
				w.locX += x
				w.locY += y
			}
		}


		//Pick area
		val houseWidth = 300
		val houseHeight = 300

		val minRoomSize = 50
		val maxRoomSize = 150

		//make a new room
		val leftOWall = new Wall(100, 100, 5, houseHeight)
		val topOWall = new Wall(100, 100, houseWidth, 5)
		val rightOWall = new Wall(100 + houseWidth, 100, 5, leftOWall.sizeY + 5)
		val botOWall = new Wall(100, 100 + houseHeight, topOWall.sizeX, 5)

		val house = scala.collection.mutable.Set[Obj](leftOWall, topOWall, rightOWall, botOWall)

		objs ++= house

		for(i <- 1 to 10)
		{
			//make a new room
			val leftWall = new Wall(0, 0, 5,
				r.nextInt(maxRoomSize - minRoomSize) + minRoomSize)
			
			val topWall = new Wall(0, 0, r.nextInt(maxRoomSize - minRoomSize) + minRoomSize, 5)

			val rightWall = new Wall(topWall.sizeX, 0, 5, leftWall.sizeY + 5)
			val botWall = new Wall(0, leftWall.sizeY, topWall.sizeX, 5)

			val roomWalls = scala.collection.mutable.Set[Wall](leftWall, topWall, rightWall, botWall)

			// val whichHasDoor = r.nextInt(4)

			// match whichHasDoor
			// {
			// 	0 => 
			// }

			//now pick where in the house it should go
			//pick an existing wall
			val matchWall = house.toVector(r.nextInt(house.size))
			//horiz or vertical?
			var x = 0
			var y = 0
			var valid = true
			if(matchWall.sizeX == 5) //vertical
			{
				if(leftWall.sizeY >= matchWall.sizeY)
				{
					valid = false
				}
				else
				{
					y = matchWall.locY + r.nextInt(matchWall.sizeY)// - leftWall.sizeY)
					if(r.nextInt(2) == 0) //match left wall
					{
						//if we're matching our left wall with 
						//the house's right wall it's outide the house
						if(matchWall == rightOWall)
						{
							valid = false
						}

						roomWalls remove leftWall
						moveRoom(roomWalls, 5, 0)
						x = matchWall.locX
					}
					else //match right wall
					{
						//our right with house left
						if(matchWall == leftOWall)
						{
							valid = false
						}

						x = matchWall.locX - topWall.sizeX
					}
				}
			}
			else //horizontal
			{
				if(topWall.sizeX >= matchWall.sizeX)
				{
					valid = false
				}
				else
				{
					x = matchWall.locX + r.nextInt(matchWall.sizeX)// - topWall.sizeX)
					if(r.nextInt(2) == 0) //match top wall
					{
						//our top with the house's bottom
						if(matchWall == botOWall)
						{
							valid = false
						}


						roomWalls remove topWall
						moveRoom(roomWalls, 0, 5)
						y = matchWall.locY
					}
					else //match bottom wall
					{
						if(matchWall == topOWall)
						{
							valid = false
						}


						y = matchWall.locY - leftWall.sizeY
					}
				}
			}

			//final checks: if any wall is outside the house
			if(rightWall.locX >  rightOWall.locX ||
				botWall.locY > botOWall.locY)
			{
				valid = false
			}

			if(valid) //it should fit
			{
				//now shift by that much
				moveRoom(roomWalls, x, y)

				//remove invalid walls
				val validWalls = roomWalls filter(! collision(_))

				//All done, it should fit!
				//house ++= validWalls
				objs ++= validWalls
			}
			
		}

		//objs ++= house
	}

	def genMap() =
	{
		dom.console.log("Generating map with difficulty " + difficulty)
		objs.clear()
	
		acts.clear()

		if(difficulty != 0) player.changeLoc(player.locX, GV.GAMEY - 50)
		addActor(player)

		//Bounding walls
		val topWallLeft = new Wall(0, 0, GV.GAMEX/2 - 50, 5)
		val topWallRight = new Wall(GV.GAMEX/2 + 50, 0, GV.GAMEX/2 - 50, 5)
		val leftWall = new Wall(0, 0, 5, GV.GAMEY)
		val rightWall = new Wall(GV.GAMEX - 5, 0, 5, GV.GAMEY)
		val botWallLeft = new Wall(0, GV.GAMEY - 5, GV.GAMEX / 2 - 50, 5)
		val botWallRight = new Wall(GV.GAMEX/2 + 50, GV.GAMEY - 5, GV.GAMEX/2 - 50, 5)

		addObj(topWallLeft)
		addObj(topWallRight)
		addObj(leftWall)
		addObj(rightWall)
		addObj(botWallLeft)
		addObj(botWallRight)

		val topTopBound = new Wall(GV.GAMEX / 2 - 50, -20, 100, 5)
		val topLeftBound = new Wall(GV.GAMEX / 2 - 55, -20, 5, 20)
		val topRightBound = new Wall(GV.GAMEX / 2 + 50, -20, 5, 20)

		val botBotBound = new Wall(GV.GAMEX / 2 - 50, GV.GAMEY + 20, 100, 5)
		val botLeftBound = new Wall(GV.GAMEX / 2 - 55, GV.GAMEY, 5, 20)
		val botRightBound = new Wall(GV.GAMEX / 2 + 50, GV.GAMEY, 5, 20)

		addObj(topTopBound)
		addObj(topLeftBound)
		addObj(topRightBound)
		addObj(botBotBound)
		addObj(botLeftBound)
		addObj(botRightBound)

		//farmhouse in the... middle?
		//genFarms()

		
		//More stuff!
		//Add random walls
		//TODO: make sure there's a clear path to the next level
		for(i <- 1 to difficulty)
		{
			val r = scala.util.Random
			val width = r.nextInt(80) + 20
			val height = r.nextInt(80) + 20
			val x = r.nextInt(GV.GAMEX - width)
			val y = r.nextInt(GV.GAMEY - height - GV.GAMEY / 5)
			//Random wall
			val randWall = new Wall(x, y, width, height)
			addObj(randWall)
		}

		//Add random zombies
		for(i <- 1 to difficulty * 3)
		{
			val x = r.nextInt(GV.GAMEX - GV.NORMUNITSIZE)
			val y = r.nextInt(GV.GAMEY - GV.NORMUNITSIZE - GV.GAMEY/4)
			//Random wall
			val randDude = new Zombie(x, y)
			if(! collision(randDude))
			{
				addActor(randDude)
			}
		}

		//add random specials
		for(i <- 1 to difficulty)
		{
			val t = r.nextInt(1)
			val x = r.nextInt(GV.GAMEX - GV.NORMUNITSIZE)
			val y = r.nextInt(GV.GAMEY - GV.NORMUNITSIZE - GV.GAMEY/4)

			if(t == 0) //charger
			{
				val randDude = new Charger(x, y)
				if(! collision(randDude))
				{
					addActor(randDude)
				}
			}
		}


		//Now, increase the difficulty
		difficulty += 1
	}
}