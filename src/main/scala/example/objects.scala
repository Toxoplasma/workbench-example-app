package objects

import org.scalajs.dom
import scala.math.{abs, signum, sqrt, pow, min, max, round}
import Array._

object GV
{
	val GAMEX = 600
	val GAMEY = 600

	val FULLX = 800
	val FULLY = 800

	val NORMUNITSIZE = 10
	val BIGUNITSIZE = 14
}

class Pt(x_ : Double, y_ : Double)
{
	var x = x_
	var y = y_

	def is_zero() : Boolean =
	{
		return x == 0.0 && y == 0.0
	}

	def is_neg_one() : Boolean =
	{
		return x == -1.0 && y == -1.0
	}

	def cloone() : Pt =
	{
		return new Pt(x, y)
	}

	override def toString(): String = "(" + x + ", " + y + ")"
}


case class SimpleLine(start : Pt, end : Pt, color : String)
{
	def length() =
	{
		val dist = sqrt(pow(start.x - end.x, 2) + pow(start.y - end.y, 2))
		dist
	}

	def unitStep() : Pt =
	{
		val ux = (end.x - start.x) / length
		val uy = (end.y - start.y) / length

		return new Pt(ux, uy)
	}
}






class Obj(loc_ : Pt, size_ : Pt)
{
	var loc = loc_
	val size = size_

	var blocksMovement = true
	var blocksLos = false
	var alwaysVisible = false

	var lowPriority = false

	def collides(lo : Pt, s : Pt) : Boolean =
	{
		//return abs(loc.x - loX) * 2 < size.x + sX && abs(loc.y - loY) * 2 < size.y + sY
		if(loc.x < lo.x + s.x &&
			loc.x + size.x > lo.x &&
			loc.y < lo.y + s.y &&
			loc.y + size.y > lo.y)
		{
			return true
		}
		return false
	}

	def collides(other : Obj) : Boolean =
	{
		collides(other.loc, other.size)
	}

	def inside(x : Double, y : Double) : Boolean = 
	{
		if(x > loc.x &&
			x < loc.x + size.x &&
			y > loc.y &&
			y < loc.y + size.y)
		{
			return true
		}
		return false
	}

	def collidesLine(l : SimpleLine) : Boolean =
	{
		//dom.console.log("Checking " + (loc.x, loc.y))
	    // Completely outside.
	    if ((l.start.x <= loc.x && l.end.x <= loc.x) || 
	    	(l.start.y <= loc.y && l.end.y <= loc.y) || 
	    	(l.start.x >= loc.x + size.x && l.end.x >= loc.x + size.x) || 
	    	(l.start.y >= loc.y + size.y && l.end.y >= loc.y + size.y))
	        return false

	    val m = (l.end.y - l.start.y) * 1.0 / (l.end.x - l.start.x)

	    //Left wall
	    val y1 = m * (loc.x - l.start.x) + l.start.y
	    if (y1 > loc.y && y1 < loc.y + size.y) return true

	    //right wall
	    val y2 = m * (loc.x + size.x - l.start.x) + l.start.y
	    if (y2 > loc.y && y2 < loc.y + size.y) return true

	    //top wall
	    val x1 = (loc.y - l.start.y) / m + l.start.x
	    if (x1 > loc.x && x1 < loc.x + size.x) return true

	    //bot wall
	    val x2 = (loc.y + size.y - l.start.y) / m + l.start.x
	    if (x2 > loc.x && x2 < loc.x + size.x) return true

	    return false
	}

	def pointSetCollides(ps : scala.collection.mutable.Seq[Pt]) : Boolean = 
	{
		val filtered = ps filter(p => ! inside(p.x, p.y))
		return filtered.length == ps.length
	}

	/*def hasLosTo(o : Obj, g : Game) : Boolean =
	{
		val l = SimpleLine(center, o.center, "black")

		for(ob <- g.objs)
		{
			if(ob != this && ob != o && ob.collidesLine(l)) return false
		}
		
		return true
	}*/

	def hasClearCenterLosTo(p : Pt, o : Obj, g : Game) : Boolean =
	{
		var l = SimpleLine(center, p, "black")
		var losBroken = false
		for(ob <- g.objs)
		{
			if(ob != this && ob != o && ob.collidesLine(l)) losBroken = true
		}
		if(! losBroken) return true

		return false
	}

	def hasCenterLosTo(p : Pt, o : Obj, g : Game) : Boolean =
	{
		var l = SimpleLine(center, p, "black")
		var losBroken = false
		for(ob <- g.objs)
		{
			if(ob != this && ob != o && 
				ob.blocksLos && ob.collidesLine(l)) losBroken = true
		}
		if(! losBroken) return true

		return false
	}

	def hasLosTo(o : Obj, g : Game) : Boolean =
	{
		return hasCenterLosTo(o.center, o, g) ||
			   hasCenterLosTo(o.upperLeft, o, g) ||
			   hasCenterLosTo(o.upperRight, o, g) ||
			   hasCenterLosTo(o.lowerLeft, o, g) ||
			   hasCenterLosTo(o.lowerRight, o, g)
	}

	def hasClearLosTo(o : Obj, g : Game) : Boolean =
	{
		return hasClearCenterLosTo(o.center, o, g) ||
			   hasClearCenterLosTo(o.upperLeft, o, g) ||
			   hasClearCenterLosTo(o.upperRight, o, g) ||
			   hasClearCenterLosTo(o.lowerLeft, o, g) ||
			   hasClearCenterLosTo(o.lowerRight, o, g)
	}

	def draw(ctx : dom.CanvasRenderingContext2D) =
	{
	}

	def center() = 
	{
		new Pt(loc.x + size.x / 2, loc.y + size.y / 2)
	}

	def upperLeft() = loc.cloone
	def upperRight() = new Pt(loc.x + size.x, loc.y)
	def lowerLeft() = new Pt(loc.x, loc.y + size.y)
	def lowerRight() = new Pt(loc.x + size.x, loc.y + size.y)

	def distanceTo(o : Obj) : Int =
	{
		return sqrt(pow(o.loc.x - loc.x, 2) + pow(o.loc.y - loc.y, 2)).toInt
	}

	def distanceTo(p : Pt) : Int =
	{
		return sqrt(pow(p.x - loc.x, 2) + pow(p.y - loc.y, 2)).toInt
	}
}




class Actor(loc_ : Pt, size_ : Pt,
			hp_ : Int, speed_ : Int,
			faction_ : String, points_ : Int, name_ : String)
extends Obj(loc_, size_)
{
	var maxHp = hp_
	var hp = hp_
	var name = name_
	var speed = speed_
	var points = points_

	var faction = faction_

	var momentum = new Pt(0, 0)

	var important = false

	def changeLoc(newLoc : Pt) = //newX : Int, newY : Int) =
	{
		loc = newLoc.cloone
		// loc.x = newX
		// loc.y = newY
	}

	def changeLocRel(dx : Double, dy : Double) =
	{
		loc.x += dx
		loc.y += dy
	}

	def canMoveTo(newLoc : Pt, g : Game) : Boolean = 
	{
		for(o <- g.objs)
		{
			if(o != this && o.blocksMovement && o.collides(newLoc, size))
			{
				//dom.console.log(name + " can't move there!")
				return false
			}
		}
		return true
	}

	def moveLoc(dx : Double, dy : Double, g : Game) : Boolean =
	{
		var moved = false
		//Check collisions
		if(dx != 0 && canMoveTo(new Pt(loc.x + dx, loc.y), g)) //check x movement
		{
			loc.x += dx
			moved = true
		}

		if(dy != 0 && canMoveTo(new Pt(loc.x, loc.y + dy), g))
		{
			loc.y += dy
			moved = true
		}
		
		return moved
	}

	override def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = "red"
		ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	//Has the unit's ai pick a move and then does the move
	def aiMove(g : Game) =
	{
		//Knockback
		val changeX = momentum.x * 3 / 4
		val changeY = momentum.y * 3 / 4
		moveLoc(changeX, changeY, g)
		momentum = new Pt(changeX, changeY)
	}

	def takeDamage(source : Actor, damage : Int, pushFactor : Int, g : Game) =
	{
		dom.console.log(source.name + " does " + damage + " damage to " + name)
		hp -= damage

		push(source, damage * pushFactor, g)

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
		val dx = -1 * signum(source.loc.x - loc.x)
		val dy = -1 * signum(source.loc.y - loc.y)

		momentum = new Pt(dx * amount, dy * amount)
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

class BaseHuman(loc_ : Pt, hp_ : Int)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), hp_, 2, "human", 0, "human")
{
	var timeToNextShot = 0
	var shotCooldown = 20

	var gun = new Gun(20, 10, 200, this)

	override def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = "red"
		ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		//Knockback
		val changeX = momentum.x * 3 / 4
		val changeY = momentum.y * 3 / 4
		moveLoc(changeX, changeY, g)
		momentum = new Pt(changeX, changeY)

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

class Human(loc_ : Pt, hp_ : Int)
extends BaseHuman(loc_, hp_)
{
	var dest = new Pt(-1, -1)

	override def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = "pink"
		ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		//Bot humans do everything player humans do except for movement
		super.aiMove(g)

		//dom.console.log("Hooman: " + (loc.x, loc.y))

		//Now decide where to go!
		if(dest.is_neg_one)
		{
			//pick a new dest!
			val destX = g.player.loc.x + g.r.nextInt(GV.NORMUNITSIZE * 10) - GV.NORMUNITSIZE*5
			val destY = g.player.loc.y + g.r.nextInt(GV.NORMUNITSIZE * 10) - GV.NORMUNITSIZE*5
			dest = new Pt(destX, destY)
		}

		//Now cheap pathfinding as usual
		var dx = min(dest.x - loc.x, 2)
		dx = max(dx, -2)
		
		var dy = min(dest.y - loc.y, 2)
		dy = max(dy, -2)

		if(! moveLoc(dx, dy, g))
		{
			dest = new Pt(-1, -1)
		}
	}

	override def moveToNewMap(g : Game)
	{
		dest = new Pt(-1, -1)
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
		target.takeDamage(owner, damage, 1, g)
		shotCountdown = firingSpeed

		//add it to the graphics
		g.linesToDraw += SimpleLine(owner.center(), target.center(), "black")
	}

	def tick()
	{
		if(shotCountdown > 0) shotCountdown -= 1
	}
}



class Zombie(loc_ : Pt)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE),
	20, 1, 
	"zombie", 1, "zombie")
{
	var target : Actor = null

	var direction = new Pt(0, 0)

	val chanceToChangeDirection = 400

	override def draw(ctx: dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = s"rgb(30, 150, 30)"
		ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		//Knockback
		val changeX = momentum.x * 3 / 4
		val changeY = momentum.y * 3 / 4
		moveLoc(changeX, changeY, g)
		momentum = new Pt(changeX, changeY)

		//Get closest in-LOS enemy
		target = getClosestEnemyInLOS(g)
		if(target != null)
		{
			val dest = target.loc

			val dx = signum(dest.x - loc.x)
			val dy = signum(dest.y - loc.y)

			direction = new Pt(dx, dy)
		}
		else if(direction.is_zero)
		{
			//we're stuck, wander in a different direction
			direction = new Pt(g.r.nextInt(3) - 1, g.r.nextInt(3) - 1)
		}		

		//try to move there, see if it works
		if(! moveLoc(direction.x, direction.y, g))
		{
			direction = new Pt(0, 0)
		}
		else
		{
			//even if it worked, we sometimes wanna quit
			if(g.r.nextInt(chanceToChangeDirection) == 0)
			{
				direction = new Pt(0, 0)
			}
		}
		
		if(target != null && target.collides(new Pt(loc.x + direction.x, loc.y + direction.y), new Pt(size.x, size.y)))
		{
			//Hell yeah we are, bit him!
			//g.player.hp -= 10 //ZOMBIEDAMAGE
			target.takeDamage(this, 10, 1, g)
		}
	}
}

class Charger(loc_ : Pt)
extends Actor(loc_, new Pt(GV.BIGUNITSIZE, GV.BIGUNITSIZE),
	200, 1, 
	"zombie", 5, "charger")
{
	var direction = (0, 0)

	val chanceToChangeDirection = 400

	var state = "moving"
	var chargeLine : SimpleLine = null
	var chargeError = new Pt(0.0, 0.0)
	var chargeCooldown = 400
	var chargeTimer = 0
	var chargeRange = 400

	val chargeSpeed = 4

	override def draw(ctx: dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = s"rgb(100, 150, 100)"
		ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		//we don't do knockback while charging

		//tick charge timer
		chargeTimer = max(0, chargeTimer - 1)

		if(state == "moving")
		{
			//Knockback
			val changeX = momentum.x * 2 / 4
			val changeY = momentum.y * 2 / 4
			moveLoc(changeX, changeY, g)
			momentum = new Pt(changeX, changeY)


			//Get closest in-LOS enemy
			val closestAct = getClosestEnemyInLOS(g)
			if(closestAct != null) //possible charge
			{
				chargeLine = SimpleLine(loc.cloone, closestAct.loc.cloone, "black")
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
			if(distanceTo(chargeLine.start) > chargeRange)
			{
				//we've gone too far, stop charging
				state = "moving"
				//dom.console.log("Charging->Moving, too far from start")
				chargeLine = null
				chargeError = new Pt(0.0, 0.0)
			}
			else
			{
				for(i <- 1 to chargeSpeed)
				{
					if(state == "charging") //if we hit someone we don't want to keep taking steps
					{
						//take a step along the line
						//val (dx, dy) = chargeLine.unitStep
						val d = chargeLine.unitStep
						chargeError = new Pt(chargeError.x + d.x, chargeError.y + d.y)

						val adx = round(chargeError.x).toInt
						val ady = round(chargeError.y).toInt

						//dom.console.log("ad: " + (adx, ady))

						chargeError = new Pt(chargeError.x - adx, chargeError.y - ady)

						//try to move in that direction
						moveLoc(adx, ady, g)
						
						//did we crash into an actor?
						for(a <- g.acts)
						{
							if(a != this && a.collides(new Pt(loc.x + adx, loc.y + ady), size))
							{
								//we did!
								//knock 'em up
								a.takeDamage(this, 10, 4, g)
								//a.push(this, 40, g)
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

class Spitter(loc_ : Pt)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE),
	70, 1, 
	"zombie", 5, "spitter")
{
	var direction = (0, 0)
	val chanceToChangeDirection = 400

	var spitTimer = 0
	val spitRate = 500
	val spitRadius = 40
	val spitReduceRate = 4

	override def draw(ctx: dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = s"rgb(0, 255, 0)"
		ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		//we don't do knockback while charging

		//tick charge timer
		spitTimer = max(0, spitTimer - 1)

		
		//Knockback
		val changeX = momentum.x * 3 / 4
		val changeY = momentum.y * 3 / 4
		moveLoc(changeX, changeY, g)
		momentum = new Pt(changeX, changeY)


		//Get closest in-LOS enemy
		if(spitTimer <= 0)
		{
			val closestAct = getClosestEnemyInLOS(g)
			if(closestAct != null) //possible charge
			{
				//spit at them!

				//draw a line
				val spitLine = SimpleLine(loc.cloone, closestAct.loc.cloone, "black")
				//g.linesToDraw += spitLine

				//make a spit
				val spit = new CausticAcid(closestAct.loc.cloone, spitRadius, spitReduceRate)
				//g.addActor(spit)

				val proj = new ProjectileActor(spit, spitLine, 6)
				g.addActor(proj)

				//we spit!
				spitTimer = spitRate
			}
		}


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










class CausticAcid(loc_ : Pt, radius_ : Int, reduceRate_ : Int)
extends Actor(new Pt(loc_.x - radius_, loc_.y - radius_), new Pt(radius_ * 2, radius_ * 2),
				-1, 0, "NA", 0, "acid splat")
{
	var radius = radius_

	var reduceRate = reduceRate_
	var reduceTimer = reduceRate

	val minRadius = GV.NORMUNITSIZE

	blocksMovement = false
	lowPriority = true

	override def draw(ctx: dom.CanvasRenderingContext2D) = //TODO: switch this to take in game rather than ctx
	{
		ctx.fillStyle = s"rgb(0, 255, 0)"
		ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		//first, check if anyone gets damaged
		for(a <- g.acts)
		{
			if(a.faction != "NA" && collides(a)) //if it's a real actor and we collide with it
			{
				a.takeDamage(this, 1, 0, g)
			}
		}

		//Now, shrink a bit if necessary
		if(reduceTimer == 0)
		{
			//are we at 0?
			if(radius <= minRadius)
			{
				//delete ourselves
				g.removeActor(this)
			}
			else
			{
				reduceTimer = reduceRate
				radius -= 1
				changeLoc(new Pt(loc.x + 1, loc.y + 1))
				size.x -= 2
				size.y -= 2
			}
		}
		else if(reduceTimer > 0) //if we're a timed puddle, then count down the timer
		{
			reduceTimer -= 1
		}
	}
}



class ZombieSpawner(loc_ : Pt, spawnRate_ : Int)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), -1, 1, "NA", 0, "zombie spawner")
{
	val spawnRate = spawnRate_
	var timeToNextSpawn = spawnRate
	blocksMovement = false
	important = true

	override def draw(ctx: dom.CanvasRenderingContext2D) =
	{
		//Don't draw anything, this is a proxy for zombies coming in off the edge of the map
		//ctx.fillStyle = s"rgb(30, 150, 30)"
		//ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		//if it's time:
		if(timeToNextSpawn == 0)
		{
			//Spawn a zombie if there's room:
			val newZed = new Zombie(new Pt(-100, -100))
			if(newZed.canMoveTo(loc, g)) //there's room!
			{
				g.addActor(newZed)
				newZed.changeLoc(loc) //loc.x, loc.y)
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
extends Actor(new Pt(-1, -1), new Pt(0, 0), -1, act_.speed, "NA", 0, "delayed " + act_.name)
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

class ProjectileActor(act_ : Actor, line_ : SimpleLine, rate_ : Int)
extends Actor(line_.start.cloone, new Pt(0, 0), -1, 0, "NA", 0, "projectile " + act_.name)
{
	var line = line_
	var act = act_
	var rate = rate_

	blocksMovement = false

	override def draw(ctx: dom.CanvasRenderingContext2D)
	{
		//draw handled in movement
	}

	override def aiMove(g : Game)
	{
		dom.console.log("Spit: " + loc)
		val step = line.unitStep
		dom.console.log("  Step: " + step)

		var done = false

		val start = loc.cloone


		for(i <- 1 to rate)
		{
			if(! done)
			{
				//are we there yet?
				if(abs(loc.x - line.end.x) < 1 && abs(loc.y - line.end.y) < 1)
				{
					//we're there
					g.addActor(act)
					g.removeActor(this)
					done = true
				}
				else
				{
					//move
					changeLocRel(step.x, step.y)
				}
				
			}
		}

		val lineToDraw = SimpleLine(start, loc, line.color)
		g.linesToDraw += lineToDraw
	}
}

















//Terrain stuff
class Wall(loc_ : Pt, size_ : Pt) extends Obj(loc_, size_)
{
	blocksLos = true
	alwaysVisible = true
	override def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = "blue"
		ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	def splitWithDoorAt(i : Int, doorSize : Int) : (Wall, Wall) = 
	{
		//horizontal
		if(size.x >= size.y)
		{
			val w1 = new Wall(loc, new Pt(i, size.y))
			val w2 = new Wall(new Pt(loc.x + i + doorSize, loc.y), new Pt(size.x - i - doorSize, size.y))
			return (w1, w2)
		}
		else //vertical
		{
			val w1 = new Wall(loc, new Pt(size.x, i))
			val w2 = new Wall(new Pt(loc.x, loc.y + i + doorSize), new Pt(size.x, size.y - i - doorSize))
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
	val player = new BaseHuman(new Pt(GV.GAMEX / 2, GV.GAMEY/2), 100)
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
		//draw floor LOS by color.... :(
		val losArr = ofDim[Boolean](GV.GAMEX, GV.GAMEY)
		//now, for each wall, shoot rays in the usual way
		val dumb = new Obj(new Pt(0, 0), new Pt(1, 1))
		//bottom wall
		for(x<- 0 to GV.GAMEX - 1)
		{
			//walk outwards from player
			val l = SimpleLine(player.loc.cloone, new Pt(x, GV.GAMEY), "black")

			val c = l.start.cloone
			val step = l.unitStep

			var visible = true
			while(visible && (abs(c.x - l.end.x) >= 1 || abs(c.y - l.end.y) >= 1))
			{
				c.x += step.x
				c.y += step.y

				//now mark it visible
				losArr(c.x.toInt)(c.y.toInt) = true

				//now check if it blocks
				for(o <- objs)
				{
					if(o.blocksLos && o.inside(c.x, c.y))
					{
						visible = false
					}
				}
			}
		}
		for(x <- 0 to GV.GAMEX - 1)
		{
			for(y <- 0 to GV.GAMEY - 1)
			{
				if(! losArr(x)(y))
				{
					//draw it dark grey
					ctx.fillStyle = s"rgb(100, 100, 100)"
					ctx.fillRect(x, y, 1, 1)
				}
			}
		}
		//draw all low-priority
		objs map(o => if(o.lowPriority) o.draw(ctx))

		//draw lines
		while(! linesToDraw.isEmpty)
		{
			val line = linesToDraw.remove(0)

			val s = line.start //(sx, sy) = line.start
			val e = line.end //(ex, ey) = line.end

			//now draw it
			ctx.beginPath()
			ctx.moveTo(s.x, s.y)
			ctx.fillStyle = line.color
			ctx.lineTo(e.x, e.y)
			ctx.stroke
		}


		//Draw all the objects
		for(o <- objs)
		{
			if(! o.lowPriority && (o.alwaysVisible ||
								   player.hasLosTo(o, this)))
			{

				o.draw(ctx)
			}
		}
	}

	/*def genFarms2() =
	{
		//make a starter room
		val starter = new Wall(new Pt(100, 100), 100, 100)

		objs += starter

		for(i <- 1 to 10)
		{
			//pick which wall we're going to add it to
			//val newRoom = new Wall(0, 0, )
		}
	}*/

	/*def genFarms() =
	{
		def moveRoom(walls : scala.collection.mutable.Set[Wall], x : Int, y : Int) =
		{
			for(w <- walls)
			{
				w.loc.x += x
				w.loc.y += y
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
		val rightOWall = new Wall(100 + houseWidth, 100, 5, leftOWall.size.y + 5)
		val botOWall = new Wall(100, 100 + houseHeight, topOWall.size.x, 5)

		val house = scala.collection.mutable.Set[Obj](leftOWall, topOWall, rightOWall, botOWall)

		objs ++= house

		for(i <- 1 to 10)
		{
			//make a new room
			val leftWall = new Wall(0, 0, 5,
				r.nextInt(maxRoomSize - minRoomSize) + minRoomSize)
			
			val topWall = new Wall(0, 0, r.nextInt(maxRoomSize - minRoomSize) + minRoomSize, 5)

			val rightWall = new Wall(topWall.size.x, 0, 5, leftWall.size.y + 5)
			val botWall = new Wall(0, leftWall.size.y, topWall.size.x, 5)

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
			if(matchWall.size.x == 5) //vertical
			{
				if(leftWall.size.y >= matchWall.size.y)
				{
					valid = false
				}
				else
				{
					y = matchWall.loc.y + r.nextInt(matchWall.size.y)// - leftWall.size.y)
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
						x = matchWall.loc.x
					}
					else //match right wall
					{
						//our right with house left
						if(matchWall == leftOWall)
						{
							valid = false
						}

						x = matchWall.loc.x - topWall.size.x
					}
				}
			}
			else //horizontal
			{
				if(topWall.size.x >= matchWall.size.x)
				{
					valid = false
				}
				else
				{
					x = matchWall.loc.x + r.nextInt(matchWall.size.x)// - topWall.size.x)
					if(r.nextInt(2) == 0) //match top wall
					{
						//our top with the house's bottom
						if(matchWall == botOWall)
						{
							valid = false
						}


						roomWalls remove topWall
						moveRoom(roomWalls, 0, 5)
						y = matchWall.loc.y
					}
					else //match bottom wall
					{
						if(matchWall == topOWall)
						{
							valid = false
						}


						y = matchWall.loc.y - leftWall.size.y
					}
				}
			}

			//final checks: if any wall is outside the house
			if(rightWall.loc.x >  rightOWall.loc.x ||
				botWall.loc.y > botOWall.loc.y)
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
	}*/

	def genMap() =
	{
		dom.console.log("Generating map with difficulty " + difficulty)
		objs.clear()
	
		acts.clear()

		if(difficulty != 0) player.changeLoc(new Pt(player.loc.x, GV.GAMEY - 50))
		addActor(player)

		//Bounding walls
		val topWallLeft = new Wall(new Pt(0, 0), new Pt(GV.GAMEX/2 - 50, 5))
		val topWallRight = new Wall(new Pt(GV.GAMEX/2 + 50, 0), new Pt(GV.GAMEX/2 - 50, 5))
		val leftWall = new Wall(new Pt(0, 0), new Pt(5, GV.GAMEY))
		val rightWall = new Wall(new Pt(GV.GAMEX - 5, 0), new Pt(5, GV.GAMEY))
		val botWallLeft = new Wall(new Pt(0, GV.GAMEY - 5), new Pt(GV.GAMEX / 2 - 50, 5))
		val botWallRight = new Wall(new Pt(GV.GAMEX/2 + 50, GV.GAMEY - 5), new Pt(GV.GAMEX/2 - 50, 5))

		addObj(topWallLeft)
		addObj(topWallRight)
		addObj(leftWall)
		addObj(rightWall)
		addObj(botWallLeft)
		addObj(botWallRight)

		val topTopBound = new Wall(new Pt(GV.GAMEX / 2 - 50, -20), new Pt(100, 5))
		val topLeftBound = new Wall(new Pt(GV.GAMEX / 2 - 55, -20), new Pt(5, 20))
		val topRightBound = new Wall(new Pt(GV.GAMEX / 2 + 50, -20), new Pt(5, 20))

		val botBotBound = new Wall(new Pt(GV.GAMEX / 2 - 50, GV.GAMEY + 20), new Pt(100, 5))
		val botLeftBound = new Wall(new Pt(GV.GAMEX / 2 - 55, GV.GAMEY), new Pt(5, 20))
		val botRightBound = new Wall(new Pt(GV.GAMEX / 2 + 50, GV.GAMEY), new Pt(5, 20))

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
			val randWall = new Wall(new Pt(x, y), new Pt(width, height))
			addObj(randWall)
		}

		//Add random zombies
		for(i <- 1 to difficulty * 3)
		{
			val x = r.nextInt(GV.GAMEX - GV.NORMUNITSIZE)
			val y = r.nextInt(GV.GAMEY - GV.NORMUNITSIZE - GV.GAMEY/4)
			//Random wall
			val randDude = new Zombie(new Pt(x, y))
			if(! collision(randDude))
			{
				addActor(randDude)
			}
		}

		//add random specials
		for(i <- 1 to difficulty)
		{
			val t = r.nextInt(2)
			val x = r.nextInt(GV.GAMEX - GV.NORMUNITSIZE)
			val y = r.nextInt(GV.GAMEY - GV.NORMUNITSIZE - GV.GAMEY/4)

			var randDude : Actor = null

			if(t == 0) //charger
				randDude = new Charger(new Pt(x, y))
			if(t == 1) //spitter
				randDude = new Spitter(new Pt(x, y))


			if(! collision(randDude))
			{
				addActor(randDude)
			}
		}


		//Now, increase the difficulty
		difficulty += 1
	}
}