package objects

import org.scalajs.dom
import scala.math.{abs, signum, sqrt, pow, min, max, round, acos}
import Array._

import game._
import globalvars._
import enemies._
import allies._






class Obj(loc_ : Pt, size_ : Pt)
{
	var loc = loc_
	val size = size_

	var blocksMovement = true
	var blocksLos = false
	var alwaysVisible = false

	var lowPriority = false
	var highPriority = false

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
			hp_ : Double, speed_ : Double,
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


	var lastLoc = new Pt(-1, -1)

	var effects = scala.collection.mutable.Set[Effect]()
	var equips = scala.collection.mutable.HashMap[String, Equipment]()

	//flags
	//drawing related
	var important = true

	//gameplay related
	var flammable = true
	var canPutSelfOut = false
	var acidProof = false

	var damageReduce = 0

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

	def roundLoc() =
	{
		loc.x = round(loc.x)
		loc.y = round(loc.y)
	}

	def moveLocWithoutRound(dx : Double, dy : Double, g : Game) : Boolean =
	{
		var moved = false

		val tempLoc = loc.cloone

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

		if(moved)
		{
			lastLoc = tempLoc
		}
		
		return moved
	}

	def moveLoc(dx : Double, dy : Double, g : Game) : Boolean =
	{
		if(moveLocWithoutRound(dx, dy, g))
		{
			roundLoc()
			true
		}
		else
		{
			false
		}
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

			lastLoc += change //don't want to change the way you're facing
		}

		//moveLoc(changeX, changeY, g)
		momentum = new Pt(changeX, changeY)
	}

	def takeDamage(source : Actor, damage : Double, pushFactor : Double, g : Game, irresistable : Boolean = false) =
	{
		//dom.console.log(source.name + " does " + damage + " damage to " + name)
		var adjustedDamage = damage
		if(! irresistable)
		{
			adjustedDamage = max(1, adjustedDamage - damageReduce)
		}
		hp -= adjustedDamage

		push(source, adjustedDamage * pushFactor, g)

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

	def getCondActorInLOS(scoreFunc : Actor => Double, g : Game) : Actor =
	{
		var bestAct : Actor = null
		var bestScore : Double = Double.NegativeInfinity
		for(a <- g.acts)
		{
			val score = scoreFunc(a)
			if(a.faction != "NA" && score > bestScore && hasLosTo(a, g))
			{
				bestAct = a
				bestScore = score
			}
		}

		return bestAct
	}

	def getClosestValidActorInLOS(valid : Actor => Boolean, g : Game) : Actor =
	{
		def validScore(a : Actor) =
		{
			if(valid(a))
				-1 * distanceTo(a)
			else
				Double.NegativeInfinity
		}

		getCondActorInLOS(validScore, g)
	}

	def getClosestEnemyInLOS(g : Game) : Actor = 
	{
		getClosestValidActorInLOS(isEnemy, g)
	}

	def inFov(o : Obj) : Boolean =
	{
		val facing = loc - lastLoc
		val dir = o.loc - loc

		//now, is the angle within fov?
		//cosine rule?
		val cosa = (facing.x * dir.x + facing.y * dir.y) / (facing.pythagLength * dir.pythagLength)

		val a = acos(cosa)
		//dom.console.log(a)

		return abs(a) < GV.MAX_FOV
	}

	def getClosestEnemyInView(g : Game) : Actor =
	{
		getClosestValidActorInLOS(a => inFov(a) && isEnemy(a), g)
	}

	def getBestEnemyInRange(r : Int, g : Game) : Actor =
	{
		def score(a : Actor) =
		{
			//if it's an enemy that we can see, then score it
			if(distanceTo(a) < r && inFov(a) && isEnemy(a))
			{
				a.points * 100.0/ pow(distanceTo(a), 2)
			}
			else //if not it's not the best
			{
				Double.NegativeInfinity
			}
		}

		getCondActorInLOS(score, g)
	}

	def getBestEnemyInView(g : Game) : Actor =
	{
		getBestEnemyInRange(GV.GAMEX + GV.GAMEY, g)
	}

	//a is my enemy
	def isEnemy(a : Actor) : Boolean =
	{
		a.faction != faction
	}

	def moveToNewMap(time : Int, g : Game) =
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

	def isDead() =
	{
		hp <= 0
	}

	def runEffects(g : Game) = 
	{
		for(f <- effects)
		{
			f.effect(this, g)
		}
	}

	def angleToSpriteAngle(change : Pt) : Int =
	{
		var spriteAngle = 0

		if(change.x > 0 && change.y == 0)
			spriteAngle = 90
		else if(change.x < 0 && change.y == 0)
			spriteAngle = 270
		else if(change.x == 0 && change.y > 0)
			spriteAngle = 180
		else if(change.x == 0 && change.y < 0)
			spriteAngle = 0
		
		else if(change.x > 0 && change.y > 0) //bot right
			spriteAngle = 135
		else if(change.x < 0 && change.y > 0) //bot left
			spriteAngle = 225
		else if(change.x < 0 && change.y < 0) //top left
			spriteAngle = 315
		else if(change.x > 0 && change.y < 0) //top right
			spriteAngle = 45

		spriteAngle
	}

	

	def gainHealth(n : Int) =
	{
		hp = min(hp + n, maxHp)
	}
}


class Item(loc_ : Pt)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), -1, 0, "NA", 0, "item")
{
	blocksMovement = false

	def pickup(owner : Actor, g : Game)
	{
		//yeah
	}

	override def aiMove(g : Game) =
	{
		//check if the anyone valid collides with us
		for(a <- g.acts)
		{
			if(collides(a) && a.canTakeItem(this))
			{
				pickup(a, g)

				//and remove us
				g.removeActor(this)
			}
		}
	}
}


class AmmoPack(loc_ : Pt, amt_ : Int)
extends Item(loc_)
{
	val amount = amt_

	override def draw(g : Game) =
	{
		val img = g.images("item_ammobox")
		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
	}

	override def pickup(owner: Actor, g : Game)
	{
		//if it's a human give 'em some ammo
		owner match 
		{
			case h : BaseHuman => h.addAmmo(amount)
				//h.gun.ammo += amount
			case _ => //nothing
		}
	}
}

class MediumAmmoPack(loc_ : Pt)
extends AmmoPack(loc_, GV.AMMOPACK_AMOUNT)

class HealthPack(loc_ : Pt, amt_ : Int)
extends Item(loc_)
{
	val amount = amt_

	override def draw(g : Game) =
	{
		val img = g.images("item_healthkit")
		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
	}

	override def pickup(owner : Actor, g : Game) =
	{
		//the player gets some health!
		val n = owner.maxHp * amount / 100
		owner.hp = min(owner.hp + n, owner.maxHp)
	}
}

class MediumHealthPack(loc_ : Pt)
extends HealthPack(loc_, GV.HEALTHPACK_AMOUNT)


class Equipment(slot_ : String, name_ : String, displayName_ : String, onEquip_ : Player => Any)
{
	val slot = slot_

	var onEquip = onEquip_

	val name = name_
	val displayName = displayName_

	def equip(a : Player)
	{
		onEquip(a)
	}
}



class GroundEquip(loc_ : Pt, equip_ : Equipment, displayName_ : String)
extends Item(loc_)
{
	val equip = equip_
	val displayName = displayName_

	override def draw(g : Game)
	{
		val img = g.images(displayName)
		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
	}

	override def pickup(owner : Actor, g : Game)
	{
		owner match {
			case p : Player => p.equip(equip, g)
			case _ => //no one else can pick up equipment
		}	
	}
}


// class GroundGun(loc_ : Pt, gun_ : Gun, displayName_ : String)
// extends Item(loc_)
// {
// 	val gun = gun_
// 	val displayName = displayName_

// 	override def draw(g : Game) =
// 	{
// 		//draw from the game thing
// 		val img = g.images(displayName)

// 		//draw the location scaled to the normal size
// 		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE*2, GV.NORMUNITSIZE*2)
// 	}

// 	override def pickup(owner : Actor, g : Game)
// 	{
// 		gun.owner = owner
// 		owner match 
// 		{
// 			case h : BaseHuman => 
// 				h.gun = gun
// 			case _ => //nothing
// 		}
// 	}
// }


class UsableItem(owner_ : Actor, name_ : String, displayName_ : String)
{
	var owner = owner_
	val name = name_
	val displayName = displayName_

	def use(g : Game) = {}
}

class GroundUsableItem(loc_ : Pt, item_ : UsableItem, displayName_ : String)
extends Item(loc_)
{
	val item = item_

	//flags
	important = false

	override def draw(g : Game) =
	{
		//draw from the game thing
		val img = g.images(item.displayName)
		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
	}

	override def pickup(owner : Actor, g : Game)
	{
		owner match {
			case p : Player => 
				item.owner = p
				p.addUsableItem(item)
			case _ => //no one else can use items 
		}
	}
}

class ThrowableItem(owner_ : Actor, minDist_ : Int, name_ : String, picture_ : String)
extends UsableItem(owner_, name_, picture_)
{
	val dist = GV.PLAYER_THROWDISTANCE
	val speed = GV.PLAYER_THROWSPEED
	val minDist = minDist_

	def hasTarget(g : Game) : Boolean =
	{
		val target = owner.getBestEnemyInView(g)
		target != null
	}

	def getThrowPosition(g : Game) : Pt =
	{
		val target = owner.getBestEnemyInView(g)

		if(target != null && owner.distanceTo(target) > minDist)
		{
			dom.console.log("Target")
			//throw it at the target
			return target.loc
		}
		else
		{
			dom.console.log("straight")
			return getUntargetedThrowPosition(g)
		}
	}

	def getUntargetedThrowPosition(g : Game) : Pt = 
	{
		val d = owner.loc - owner.lastLoc
		val diff = d * dist

		return owner.loc + diff
	}
}


class UsableLandMine(owner_ : Actor)
extends UsableItem(owner_, "land mine", "item_landmine")
{
	override def use(g : Game) =
	{
		//drop a land mine at the owner's location
		val mine = new LandMine(owner.loc.cloone, owner.faction)

		g.addActor(mine)
	}
}

class UsableZombieController(owner_ : Actor)
extends UsableItem(owner_, "wifi router", "item_zombiecontroller")
{
	override def use(g : Game) =
	{
		//drop it at the owner's location
		val mine = new ZombieController(owner.loc.cloone)

		g.addActor(mine)
	}
}

class UsableGrenade(owner_ : Actor)
extends ThrowableItem(owner_, GV.GRENADE_RADIUS * 3/2, "grenade", "item_grenade")
{
	override def use(g : Game)
	{
		val dest = getThrowPosition(g)

		//make a spitter spit there
		val spit = new SimpleBomb(dest, GV.GRENADE_RADIUS, GV.GRENADE_DAMAGE)
		val spitLine = SimpleLine(owner.loc.cloone, dest, "black")
		
		val proj = new ProjectileActor(spit, spitLine, speed)
		g.addActor(proj)
	}
}

class UsablePipeBomb(owner_ : Actor)
//extends UsableItem(owner_, "pipe bomb", "item_pipebomb")
extends ThrowableItem(owner_, 0, "pipe bomb", "item_pipebomb")
{
	override def use(g : Game) =
	{
		val dest = getThrowPosition(g)

		//make a spitter spit there
		val spit = new PipeBomb(dest)
		val spitLine = SimpleLine(owner.loc.cloone, dest, "black")
		
		val proj = new ProjectileActor(spit, spitLine, speed)
		g.addActor(proj)
	}
}

class UsableSpitterAcid(owner_ : Actor)
extends ThrowableItem(owner_, GV.SPITTER_SPITRADIUS, "spitter acid", "item_acid")
{
	override def use(g : Game) =
	{
		//compute where it should land
		val dest = getThrowPosition(g)

		//make a spitter spit there
		val spit = new CausticAcid(dest, GV.SPITTER_SPITRADIUS, GV.SPITTER_SPITREDUCERATE)
		val spitLine = SimpleLine(owner.loc.cloone, dest, "black")
		
		val proj = new ProjectileActor(spit, spitLine, speed)
		g.addActor(proj)
	}
}

class UsableMolotov(owner_ : Actor)
extends ThrowableItem(owner_, GV.MOLOTOV_RADIUS, "molotov", "item_molotov")
{
	override def use(g : Game) =
	{
		//compute where it should land
		val dest = getThrowPosition(g)

		//make a spitter spit there
		val spit = new FirePatch(dest, GV.MOLOTOV_RADIUS, GV.MOLOTOV_DURATION)
		val spitLine = SimpleLine(owner.loc.cloone, dest, "black")
		
		val proj = new ProjectileActor(spit, spitLine, speed)
		g.addActor(proj)
	}
}

class UsableGunTurret(owner_ : Actor)
extends ThrowableItem(owner_, 0, "turret", "turret big")
{
	override def use(g : Game)
	{
		val dest = getUntargetedThrowPosition(g)

		//make a gun turret
		//val gun = new Gun(GV.TURRET_FIRETIME, GV.TURRET_DAMAGE, GV.TURRET_RANGE, GV.TURRET_APS, 0, null, "turret gun", "ak47")
		val turret = new GunTurret(dest, GV.TURRET_HP, "NA", "human")
		//gun.owner = turret
		val spitLine = SimpleLine(owner.loc.cloone, dest, "black")
		
		val proj = new ProjectileActor(turret, spitLine, speed)
		g.addActor(proj)
	}
}


class PipeBomb(loc_ : Pt)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE),
				100000, 0, "human", 0, "pipe bomb")
{
	var timer = GV.PIPEBOMB_TIME

	momentumFactor = 0

	override def draw(g : Game) =
	{
		//draw from the game thing
		val img = g.images("item_pipebomb")
		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
	}

	override def aiMove(g : Game) =
	{
		//tick down, when ticked down, spawn a simple bomb and explode
		//TODO: HERE
		if(timer <= 0)
		{
			//explode
			g.removeActor(this)
			val bomb = new SimpleBomb(loc, GV.PIPEBOMB_RADIUS, GV.PIPEBOMB_DAMAGE)
			g.addActor(bomb)
		}
		else
		{
			timer -= 1
		}
	}
}

//all it does is explode
class SimpleBomb(loc_ : Pt, radius_ : Int, damage_ : Int)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE),
				-1, 0, "NA", 0, "bomb")
{
	val radius = radius_
	val damage = damage_
	//should never be drawn


	override def aiMove(g : Game) = 
	{
		//we explode!
		for(a <- g.acts)
		{
			if(a.faction != "NA" && distanceTo(a) < radius) //they take damage
			{
				a.takeDamage(this, damage, 1, g)
			}
		}

		g.removeActor(this)
	}
}

//TODO: make this return them to zombie team when it runs out of time
class ZombieController(loc_ : Pt)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE),
				-1, 0, "NA", 0, "zombie controller")
{
	val radius = GV.ZOMBIE_CONTROLLER_RADIUS

	var timer = GV.ZOMBIE_CONTROLLER_TIMER

	blocksMovement = false
	lowPriority = true

	override def draw(g : Game) =
	{
		//draw from the game thing
		val img = g.images("item_zombiecontroller")
		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
	}

	override def aiMove(g : Game) =
	{
		for(a <- g.acts)
		{
			if(a.faction == "zombie" && distanceTo(a) < radius)
			a match {
				case z : Zombie => 
					z.faction = "human" 
					z.color = s"rgb(150, 30, 150)"
				case _ =>
			}	
		}

		if(timer <= 0)
		{
			//we're done
			g.removeActor(this)
		}
		else
		{
		timer -= 1
		}
	}
}

//TODO: make this just wrap another actor (i.e. fire, or simple bomb) so it's multi-purpose
class LandMine(loc_ : Pt, ignoreFaction_ : String)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE),
				-1, 0, "NA", 0, "land mine")
{
	val radius = GV.LANDMINE_RADIUS
	val damage = GV.LANDMINE_DAMAGE
	val ignoreFaction = ignoreFaction_

	var enabledTimer = GV.LANDMINE_DELAY

	blocksMovement = false
	lowPriority = true

	override def draw(g : Game) =
	{
		if(enabledTimer <= 0)
		{
			g.ctx.fillStyle = s"rgb(50, 50, 50)"
			g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
			g.ctx.fillStyle = s"rgb(200, 50, 50)"
			g.ctx.fillRect(loc.x + 2, loc.y + 2, size.x - 4, size.y - 4)
		}
		else
		{
			g.ctx.fillStyle = s"rgb(50, 50, 50)"
			g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
			g.ctx.fillStyle = s"rgb(50, 50, 50)"
			g.ctx.fillRect(loc.x + 2, loc.y + 2, size.x - 4, size.y - 4)
		}
	}

	override def aiMove(g : Game) = 
	{
		if(enabledTimer <= 0)
		{
			for(a <- g.acts)
			{
				//if it's a real actor and we collide with it
				if(a.faction != "NA" && a.faction != ignoreFaction && collides(a)) 
				{

					//explode
					g.removeActor(this)
					val bomb = new SimpleBomb(loc, GV.LANDMINE_RADIUS, GV.LANDMINE_DAMAGE)
					g.addActor(bomb)
				}
			}
		}
		else
		{
			enabledTimer -= 1
		}
	}
}




class CausticAcid(loc_ : Pt, radius_ : Int, reduceRate_ : Int)
extends Actor(new Pt(loc_.x - radius_, loc_.y - radius_), new Pt(radius_ * 2, radius_ * 2),
				-1, 0, "NA", 0, "caustic acid")
{
	var radius = radius_

	var reduceRate = reduceRate_
	var reduceTimer = reduceRate

	val minRadius = GV.NORMUNITSIZE

	blocksMovement = false
	lowPriority = true

	override def draw(g : Game) =
	{
		g.ctx.fillStyle = s"rgb(0, 255, 0)"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		//first, check if anyone gets damaged
		for(a <- g.acts)
		{
			if(a.faction != "NA" && ! a.acidProof && 
				collides(a) && hasLosTo(a, g)) //if it's a real actor and we collide with it
			{
				a.takeDamage(this, GV.SPITTER_SPITDAMAGE, 0, g, true)
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

class FirePatch(loc_ : Pt, radius_ : Int, timer_ : Int)
extends Actor(new Pt(loc_.x - radius_, loc_.y - radius_), new Pt(radius_ * 2, radius_ * 2),
				-1, 0, "NA", 0, "fire")
{
	var radius = radius_

	var timer = timer_

	val actsOnFire = scala.collection.mutable.HashMap[Actor, Int]()

	blocksMovement = false
	highPriority = true

	override def draw(g : Game) =
	{
		def drawFire(loc : Pt, area : Pt)
		{
			for(i <- 1 to (area.x * area.y / 10).toInt)
			{
				//draw a red dot?
				val x = g.r.nextInt(area.x.toInt) + loc.x
				val y = g.r.nextInt(area.y.toInt) + loc.y
				g.ctx.fillStyle = s"rgb(" + (g.r.nextInt(100) + 156) + ", 0, 0)"
				g.ctx.fillRect(x, y, 1, 1)
			}
		}

		if(timer > 0)
			drawFire(loc, size)

		//g.ctx.fillStyle = s"rgb(200, 50, 50)"
		//g.ctx.fillRect(loc.x, loc.y, size.x, size.y)

		//draw fire on all the dudes on fire
		for((a, t) <- actsOnFire)
		{
			drawFire(a.loc, a.size)
		}
	}

	override def aiMove(g : Game) =
	{
		//first, check if anyone gets set on fire
		if(timer > 0)
		{
			for(a <- g.acts)
			{
				//if it's a real actor and we collide with it and it's flammable
				if(a.faction != "NA" && collides(a) && hasLosTo(a, g) && a.flammable) 
				{
					actsOnFire += (a -> 0)
				}
			}

			timer -= 1
		}

		//now process fire
		for((a, t) <- actsOnFire)
		{
			a.takeDamage(this, GV.FIRE_DAMAGE, 0, g, true)

			

		}

		for((a, t) <- actsOnFire)
		{
			//did they put the fire out?
			if(a.canPutSelfOut)
			{
				if(t >= GV.FIRE_EXTINGUISHTIME)
				{
					actsOnFire remove a
				}
				else
					actsOnFire(a) += 1

			}
			//are they dead?
			if(a.isDead)
			{
				actsOnFire remove a
			}
		}

		if(timer <= 0 && actsOnFire.isEmpty) //all done!
		{
			//remove ourselves, we're done
			actsOnFire.clear
			g.removeActor(this)
		}
	}
}


//TODO: make this take in a position and area so it could work sideways or whatever
class ZombieSpawner(spawnRate_ : Int)
extends Actor(new Pt(6, GV.GAMEY), new Pt(GV.GAMEX - 12, GV.NORMUNITSIZE), -1, GV.ZOMBIESPAWNER_SPEED, "NA", 0, "zombie spawner")
{
	val spawnRate = spawnRate_
	var timeToNextSpawn = spawnRate
	blocksMovement = false
	//important = true

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
			val destx = g.r.nextInt(size.x.toInt)
			val dest = new Pt(destx, loc.y)
			//Spawn a zombie if there's room:
			val newZed = new Zombie(new Pt(-100, -100))
			if(newZed.canMoveTo(dest, g)) //there's room!
			{
				g.addActor(newZed)
				newZed.changeLoc(dest) //loc.x, loc.y)
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

	override def moveToNewMap(time : Int, g : Game) =
	{

	}
}

class ProjectileActor(act_ : Actor, line_ : SimpleLine, rate_ : Int)
extends Actor(line_.start.cloone, new Pt(0, 0), -1, 0, "NA", 0, "projectile " + act_.name)
{
	var line = line_
	var act = act_
	var rate = rate_

	var forceSpawn = true
	var passThrough = false

	blocksMovement = true

	override def draw(g : Game)
	{
		//draw handled in movement
	}

	override def aiMove(g : Game)
	{
		val step = line.unitStep

		var done = false

		val start = loc.cloone

		var stepCount = 0
		while(!done && stepCount < rate*2 && distanceTo(start) < rate)
		{
			if(passThrough) // it ignores collisions
			{
				changeLocRel(step.x, step.y)
			}
			else
			{
				//try to move. If it fails, we're done
				done = ! moveLocWithoutRound(step.x, step.y, g)
			}

			//are we there yet?
			if((loc - line.start).pythagLength > line.length)
				done = true
		}

		//if we're done, spawn the actor
		if(done)
		{
			act.changeLoc(loc - (act.size*.5))

			g.removeActor(this)

			if(forceSpawn || ! g.collision(act))
			{
				g.addActor(act)
			}
			
		}
		

		val lineToDraw = SimpleLine(start, loc, line.color)
		g.linesToDraw += lineToDraw
	}
}


class DummyItem(loc_ : Pt)
extends Actor(loc_, new Pt(0, 0), -1, 0, "NA", 0, "dummy")














//Terrain stuff
class Wall(loc_ : Pt, size_ : Pt) extends Obj(loc_, size_)
{
	blocksLos = true
	alwaysVisible = true

	var color = "blue"
	override def draw(g : Game) =
	{
		g.ctx.fillStyle = color
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

class Barrier(loc_ : Pt, size_ : Pt) extends Wall(loc_, size_)
{
	blocksLos = false
	alwaysVisible = true

	color = "brown"
}




class Effect(effect_ : (Actor, Game) => Any, draw_ : Game => Any)
{
	val effect = effect_
	val draw = draw_
}



class EqGeneric(slot_ : String, name_ : String, displayName_ : String)
extends Equipment(slot_, name_, displayName_, a => ())



class EqLightArmor()
extends Equipment("body", "light armor", "eq light armor big", a => ())
{
	def apply(a : Player)
	{
		a.damageReduce = 5
		a.maxUsableItems -= 5
	}

	onEquip = apply
}

class EqHeavyArmor()
extends Equipment("body", "heavy armor", "eq heavy armor big", a => ())
{
	def apply(a : Player)
	{
		a.damageReduce = 9
		a.maxUsableItems -= 10
	}

	onEquip = apply
}
	


class EqSpitterBoots()
extends Equipment("legs", "rain boots", "eq acid boots big", a => a.acidProof = true)

class EqSpeedBoots()
extends Equipment("legs", "running shoes", "item running shoes big", a => a.speed += 1)




class EqFlameCape()
extends Equipment("cloak", "flame cape", "eq fireproof cape big", a => a.flammable = false)


class Gun(firingSpeed_ : Int, damage_ : Int, range_ : Int, ammoPerShot_ : Int, aimTime_ : Int, name_ : String, displayName_ : String)
extends Equipment("gun", name_, displayName_, a => ())
{
	val firingSpeed = firingSpeed_
	val damage = damage_
	val range = range_
	val ammoPerShot = ammoPerShot_
	val aimTimeMin = aimTime_

	var owner : Actor = null
	var ownerLastLoc = new Pt(0, 0)

	var aimTime = 0
	var shotCountdown = 0

	var ammo = GV.BASE_AMMO

	onEquip = apply

	def canShoot() =
	{
		shotCountdown == 0 && ammo >= ammoPerShot && aimTime >= aimTimeMin
	}

	def shoot(target : Actor, g : Game)
	{
		//do we have enough ammo?
		if(ammo >= ammoPerShot)
		{
			ammo -= ammoPerShot
		
			//SHOOOOT ITTTT
			target.takeDamage(owner, damage, 1, g)
			shotCountdown = firingSpeed

			//add it to the graphics
			g.linesToDraw += SimpleLine(owner.center(), target.center(), "black")
		}
	}

	def tick()
	{
		if(shotCountdown > 0) shotCountdown -= 1

		if(ownerLastLoc == owner.loc) //they didn't move
			aimTime += 1
		else
			aimTime = 0

		ownerLastLoc = owner.loc.cloone
	}

	def apply(a : Player)
	{
		dom.console.log("In apply")
		owner = a
	}

	
}



class EqGun(name_ : String, displayName_ : String)
extends Equipment("gun", name_, displayName_, a => ())

class EqNoGun()
extends Gun(0, 0, 0, 10000, 0, "no gun", "ak47")

class EqPistol()
extends Gun(GV.PISTOL_FIRETIME, GV.PISTOL_DAMAGE, GV.PISTOL_RANGE, GV.PISTOL_APS, GV.PISTOL_AIMTIME, "pistol", "ak47")

class EqSMG()
extends Gun(GV.SMG_FIRETIME, GV.SMG_DAMAGE, GV.SMG_RANGE, GV.SMG_APS, GV.SMG_AIMTIME, "smg", "ak47")

class EqMG()
extends Gun(GV.MG_FIRETIME, GV.MG_DAMAGE, GV.MG_RANGE, GV.MG_APS, GV.MG_AIMTIME, "machine gun", "ak47")


class EqSniperRifle()
extends Gun(GV.SNIPER_FIRETIME, GV.SNIPER_DAMAGE, GV.SNIPER_RANGE, GV.SNIPER_APS, GV.SNIPER_AIMTIME, "sniper rifle", "ak47")

class EqAk47()
extends Gun(GV.AK47_FIRETIME, GV.AK47_DAMAGE, GV.AK47_RANGE, GV.AK47_APS, GV.AK47_AIMTIME, "ak47", "ak47")

