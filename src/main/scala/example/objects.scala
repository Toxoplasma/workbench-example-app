package objects

import org.scalajs.dom
import scala.math.{abs, signum, sqrt, pow, min, max, round}
import Array._

import game._
import globalvars._
import enemies._







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
		// return hasCenterLosTo(o.center, o, g) ||
		// 	   hasCenterLosTo(o.upperLeft, o, g) ||
		// 	   hasCenterLosTo(o.upperRight, o, g) ||
		// 	   hasCenterLosTo(o.lowerLeft, o, g) ||
		// 	   hasCenterLosTo(o.lowerRight, o, g)

		return hasCenterLosTo(o.center, o, g)
	}

	def hasClearLosTo(o : Obj, g : Game) : Boolean =
	{
		// return hasClearCenterLosTo(o.center, o, g) ||
		// 	   hasClearCenterLosTo(o.upperLeft, o, g) ||
		// 	   hasClearCenterLosTo(o.upperRight, o, g) ||
		// 	   hasClearCenterLosTo(o.lowerLeft, o, g) ||
		// 	   hasClearCenterLosTo(o.lowerRight, o, g)

		return hasClearCenterLosTo(o.center, o, g)
	}

	def draw(g : Game) =
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
	var momentumFactor = GV.NORMMOMENTUMFACTOR

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

	override def draw(g : Game) =
	{
		g.ctx.fillStyle = "red"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
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

	def handleMomentum(g : Game) = 
	{
		//Knockback
		val changeX = momentum.x * momentumFactor
		val changeY = momentum.y * momentumFactor
		val change = new Pt(changeX, changeY)

		for(i <- 1 to change.pythagLength.toInt)
		{
			val d = change.unitStep
			moveLoc(d.x, d.y, g)
		}

		//moveLoc(changeX, changeY, g)
		momentum = new Pt(changeX, changeY)
	}

	def takeDamage(source : Actor, damage : Int, pushFactor : Double, g : Game) =
	{
		dom.console.log(source.name + " does " + damage + " damage to " + name)
		hp -= damage

		push(source, damage * pushFactor, g)

		//Are we dead?
		if(hp <= 0) //we're dead
		{
			g.removeActor(this)

			//if we have anything to do when we die then do it
			onDeath(g)

			g.score += points
		}
	}

	def push(source : Actor, amount : Double, g : Game) =
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

	def moveToNewMap(g : Game) =
	{
		//meh
	}

	def canTakeItem(item : Actor) : Boolean=
	{
		false
	}

	def onDeath(g : Game) =
	{
		//nothing
	}
}



class AmmoPack(loc_ : Pt, amt_ : Int)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), -1, 0, "NA", 0, "ammo pack")
{
	val amount = amt_

	blocksMovement = false

	override def draw(g : Game) =
	{
		g.ctx.fillStyle = s"rgb(100, 100, 100)"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
		g.ctx.fillStyle = s"rgb(0, 200, 255)"
		g.ctx.fillRect(loc.x + 2, loc.y + 2, size.x - 4, size.y - 4)
	}

	override def aiMove(g : Game) =
	{
		//check if the anyone valid collides with us
		for(a <- g.acts)
		{
			if(collides(a) && a.canTakeItem(this))
			{
				a match {
					case h : BaseHuman => 
						h.gun.ammo += amount
					case _ => //nothing
				}

				//and remove us
				g.removeActor(this)
			}
		}
	}
}

class HealthPack(loc_ : Pt, amt_ : Int)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), -1, 0, "NA", 0, "ammo pack")
{
	val amount = amt_

	blocksMovement = false

	override def draw(g : Game) =
	{
		g.ctx.fillStyle = s"rgb(100, 100, 100)"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
		g.ctx.fillStyle = s"rgb(255, 200, 0)"
		g.ctx.fillRect(loc.x + 2, loc.y + 2, size.x - 4, size.y - 4)
	}

	override def aiMove(g : Game) =
	{
		//check if the anyone valid collides with us
		for(a <- g.acts)
		{
			if(collides(a) && a.canTakeItem(this))
			{
				//the player gets some ammo!
				a.hp = min(g.player.hp + amount, g.player.maxHp)

				//and remove us
				g.removeActor(this)
			}
		}
	}
}

class GroundGun(loc_ : Pt, gun_ : Gun, displayName_ : String)
extends Actor(loc_, new Pt(20, 20), -1, 0, "NA", 0, "ground gun")
{
	val gun = gun_
	val displayName = displayName_

	blocksMovement = false

	override def draw(g : Game) =
	{
		//draw from the game thing
		val img = g.images(displayName)
		g.ctx.drawImage(img, loc.x, loc.y)
	}

	override def aiMove(g : Game) =
	{
		//check if the anyone valid collides with us
		for(a <- g.acts)
		{
			if(collides(a) && a.canTakeItem(this))
			{
				//the player picks up the gun!
				gun.owner = g.player
				g.player.gun = gun

				//and remove us
				g.removeActor(this)
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

	override def draw(g : Game) = //TODO: switch this to take in game rather than ctx
	{
		g.ctx.fillStyle = s"rgb(0, 255, 0)"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
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

	override def draw(g : Game) =
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

			dom.console.log("Spawning: " + g.acts.size + " total actors")
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

	blocksMovement = false

	override def draw(g : Game) =
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
				//unable to spawn, bump our timer so we don't spam it
				time += g.r.nextInt(1000)
				//dom.console.log(act.name + " unable to spawn!")
			}
		}
		else
		{
			time -= 1
		}

	}

	override def moveToNewMap(g : Game) =
	{

	}
}

class ProjectileActor(act_ : Actor, line_ : SimpleLine, rate_ : Int)
extends Actor(line_.start.cloone, new Pt(0, 0), -1, 0, "NA", 0, "projectile " + act_.name)
{
	var line = line_
	var act = act_
	var rate = rate_

	blocksMovement = false

	override def draw(g : Game)
	{
		//draw handled in movement
	}

	override def aiMove(g : Game)
	{
		val step = line.unitStep

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
	override def draw(g : Game) =
	{
		g.ctx.fillStyle = "blue"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
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
