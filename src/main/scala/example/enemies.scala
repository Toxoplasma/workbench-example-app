package enemies

import scala.math.{min, max, signum, round}
import org.scalajs.dom

import game._
import globalvars._
import objects._



class Player(loc_ : Pt)
extends BaseHuman(loc_, GV.PLAYER_HEALTH)
{
	bigDisplayname = "char_human_1_big"
	//var usable : UsableItem = null
	var usableItems = new scala.collection.mutable.Stack[UsableItem]()

	override def canTakeItem(item : Actor) : Boolean =
	{
		true
	}

	override def draw(g : Game) =
	{
		val change = lastLoc - loc
		var img = g.images("char_human_1 " + angleToSpriteAngle(change))

		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)

		//g.ctx.fillStyle = "red"
		//g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) = 
	{
		//shoot the shootys
		super.aiMove(g)
	}

	def useItem(g : Game) =
	{
		if(usableItems.length > 0)
		{
			val usable = usableItems.pop
			usable.use(g)
		}
	}

	def addUsableItem(item : UsableItem)
	{
		usableItems.push(item)

		dom.console.log(usableItems.length)
	}
}


class BaseHuman(loc_ : Pt, hp_ : Int)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), hp_, 2, "human", 0, "human")
{
	var timeToNextShot = 0
	var shotCooldown = 20

	var bigDisplayname = ""

	var gun = new Gun(GV.PISTOL_FIRETIME, GV.PISTOL_DAMAGE, GV.PISTOL_RANGE, GV.PISTOL_APS, this)

	canPutSelfOut = true

	override def aiMove(g : Game) =
	{
		gun.tick()

		//Now, check if we can SHOOT STUFF
		if(gun.canShoot)
		{
			//val closestAct = getClosestEnemyInLOS(g)
			val closestAct = getClosestEnemyInView(g)
			if(closestAct != null)
			{
				val distance =  distanceTo(closestAct)

				//if(distance <= gun.range && inFov(closestAct))
				if(distance <= gun.range && inFov(closestAct))
				{
					gun.shoot(closestAct, g)
				}
			}
		}
	}
}






class Human(loc_ : Pt)
extends BaseHuman(loc_, GV.HUMAN_HEALTH)
{
	bigDisplayname = "char_human_3_big"

	var displayName = "char_human_3"

	var dest = new Pt(-1, -1)

	var hasSeenPlayer = false
	var isLowAmmo = false
	var isLowHealth = false

	//random dudes have less ammo
	gun.ammo = GV.HUMAN_AMMO

	def onSeePlayer(g : Game) =
	{
		val phrases = List("Good to see you!",
							"We'll bang ok",
							"Help me!",
							"Save me!",
							"Take the lead!",
							"I'll follow you!",
							"Freakin' ZOMBIES man!",
							"Let's get out of here!",
							"Run for your life!")

		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)

		hasSeenPlayer = true
	}

	def onLowAmmo(g : Game) =
	{
		val phrases = List("I'm almost out of ammo!",
							"I'm running low on bullets!",
							"We'd better find more ammo soon!")

		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)

		isLowAmmo = true
	}

	def onLowHealth(g : Game) =
	{
		val phrases = List("I'm gonna die!",
							"I'm dying bro!",
							"I'm not gonna make it!",
							"Go on without me!")

		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)

		isLowHealth = true
	}

	override def onDeath(g : Game) = 
	{
		//drop an ammo pack with remaining ammo
		if(gun.ammo > 0)
		{
			val pack = new AmmoPack(loc, gun.ammo)
			g.addActor(pack)
		}
	}

	override def canTakeItem(item : Actor) : Boolean =
	{
		false
	}

	override def draw(g : Game) =
	{
		val change = lastLoc - loc
		var img = g.images(displayName + " " + angleToSpriteAngle(change))

		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
	}

	override def aiMove(g : Game) =
	{
		//Bot humans do everything player humans do except for movement
		super.aiMove(g)

		//handle text and related things
		if(! hasSeenPlayer && hasLosTo(g.player, g))
			onSeePlayer(g)

		if(! isLowAmmo && gun.ammo <= 10)
			onLowAmmo(g)

		if(! isLowHealth && hp <= 20)
			onLowHealth(g)


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

	override def moveToNewMap(time : Int, g : Game)
	{
		dest = new Pt(-1, -1)
	}
}


class Gun(firingSpeed_ : Int, damage_ : Int, range_ : Int, ammoPerShot_ : Int, owner_ : Actor)
{
	val firingSpeed = firingSpeed_
	val damage = damage_
	val range = range_
	val ammoPerShot = ammoPerShot_

	var owner = owner_

	var shotCountdown = 0

	var ammo = GV.BASE_AMMO

	def canShoot() =
	{
		shotCountdown == 0 && ammo >= ammoPerShot
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

	var attackTimer = 0

	var color = s"rgb(30, 150, 30)"

	important = false

	override def draw(g : Game) =
	{
		g.ctx.fillStyle = color
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		if(g.r.nextInt(400) == 0)
			g.addHeadText(this, "byeah", 10)


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

		//see if we bite the target or what
		if(attackTimer <= 0 && target != null && 
			target.collides(new Pt(loc.x + direction.x, loc.y + direction.y), new Pt(size.x, size.y)))
		{
			target.takeDamage(this, GV.ZOMBIE_DAMAGE, 1, g)
			attackTimer = GV.ZOMBIE_ATTACKCOOLDOWN
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
		
		if(attackTimer > 0)
		{
			attackTimer -= 1
		}
		
	}
}



//TODO: make them slowly heal like the tank rider
//TODO: Make them have a punch cooldown like the zombie
class Tank(loc_ : Pt)
extends Actor(loc_, new Pt(GV.HUGEUNITSIZE, GV.HUGEUNITSIZE),
	GV.TANK_HEALTH, 1, 
	"zombie", 50, "tank")
{
	var target : Actor = null

	var direction = new Pt(0, 0)

	val chanceToChangeDirection = 400

	momentumFactor = GV.HUGEMOMENTUMFACTOR

	override def draw(g : Game) =
	{
		g.ctx.fillStyle = s"rgb(30, 150, 30)"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		if(g.r.nextInt(400) == 0)
			g.addHeadText(this, "ROOOAAAR", 10)


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

		if(target != null && 
			target.collides(new Pt(loc.x + direction.x, loc.y + direction.y), new Pt(size.x, size.y)))
		{
			//Hell yeah we are, bit him!
			target.takeDamage(this, GV.TANK_DAMAGE, GV.TANK_PUSH, g)
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
	var chargeCooldown = GV.CHARGER_CHARGECOOLDOWN
	var chargeTimer = 0
	var chargeRange = GV.CHARGER_CHARGERANGE

	val chargeSpeed = GV.CHARGER_CHARGESPEED

	momentumFactor = GV.BIGMOMENTUMFACTOR

	override def draw(g : Game) =
	{
		g.ctx.fillStyle = s"rgb(100, 150, 100)"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	def stateCharging() =
	{
		state = "charging"
		dom.console.log("Charging")
		chargeTimer = chargeCooldown

		//change our momentum factor while we're charging
		momentumFactor = GV.CHARGER_CHARGINGMOMENTUMFACTOR
	}

	def stateMoving() =
	{
		state = "moving" //we hit someone
		chargeLine = null

		//change our momentum back
		momentumFactor = GV.BIGMOMENTUMFACTOR
	}

	override def aiMove(g : Game) =
	{
		//we don't do knockback while charging

		//tick charge timer
		chargeTimer = max(0, chargeTimer - 1)

		if(state == "moving")
		{
			//Get closest in-LOS enemy
			val closestAct = getClosestEnemyInLOS(g)
			if(closestAct != null) //possible charge
			{
				chargeLine = SimpleLine(loc.cloone, closestAct.loc.cloone, "black")
			}


			if(chargeLine != null && chargeLine.length <= chargeRange*.75 && chargeTimer <= 0) //CHARGE!
			{
				stateCharging()
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
				stateMoving()
			}
			else
			{
				for(i <- 1 to chargeSpeed)
				{
					if(state == "charging") //if we hit someone we don't want to keep taking steps
					{
						//take a step along the line
						val d = chargeLine.unitStep
						
						//try to move in that direction
						val moveSuccess = moveLocWithoutRound(d.x, d.y, g)

						if(! moveSuccess) //we hit a wall
						{
							stateMoving()
						}
						
						//did we crash into an actor?
						for(a <- g.acts)
						{
							if(a != this &&  a.faction != "NA" &&
								a.collides(loc + d, size))
							{
								//we did!
								//knock 'em up
								a.takeDamage(this, 10, 4, g)
								stateMoving()
							}
						}
					}
				}
			}
		}
	}

	override def moveToNewMap(time : Int, g: Game)
	{
		//tick our timer down a bunch
		//how long it will take us to get there is loc.y/speed
		chargeTimer  = max(0, chargeTimer - time)
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
	val spitRate = GV.SPITTER_SPITRATE
	val spitRadius = GV.SPITTER_SPITRADIUS
	val spitReduceRate = GV.SPITTER_SPITREDUCERATE

	override def draw(g : Game) =
	{
		g.ctx.fillStyle = s"rgb(0, 255, 0)"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		//we don't do knockback while charging

		//tick charge timer
		spitTimer = max(0, spitTimer - 1)


		//Get closest in-LOS enemy
		if(spitTimer <= 0)
		{
			val target = getClosestEnemyInLOS(g)
			if(target != null && distanceTo(target) < GV.SPITTER_SPITRANGE)
			{
				//spit at them!

				//draw a line
				val spitLine = SimpleLine(loc.cloone, target.loc.cloone, "black")
				//g.linesToDraw += spitLine

				//make a spit
				val spit = new CausticAcid(target.loc.cloone, spitRadius, spitReduceRate)
				//g.addActor(spit)

				val proj = new ProjectileActor(spit, spitLine, 6)
				proj.passThrough = true
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

	override def moveToNewMap(time : Int, g: Game) =
	{
		//tick our timer down a bunch
		//how long it will take us to get there is loc.y/speed
		spitTimer  = max(0, spitTimer - time) 
	}

	override def onDeath(g : Game) = 
	{
		//drop a spitter spit
		val spit = new CausticAcid(loc, spitRadius * 2/3, spitReduceRate)
		g.addActor(spit)
	}
}



//TODO: make them slowly heal like the tank rider
class CannonTank(loc_ : Pt)
extends Actor(loc_, new Pt(GV.HUGEUNITSIZE, GV.HUGEUNITSIZE),
	GV.TANK_HEALTH, 1, 
	"zombie", 50, "tank")
{
	var target : Actor = null

	var direction = new Pt(0, 0)

	val chanceToChangeDirection = 400

	var spitTimer = 0
	val spitRate = GV.CANNON_SPITRATE

	momentumFactor = GV.HUGEMOMENTUMFACTOR

	override def draw(g : Game) =
	{
		g.ctx.fillStyle = s"rgb(30, 150, 250)"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	override def aiMove(g : Game) =
	{
		//tick spit timer
		spitTimer = max(0, spitTimer - 1)

		//Get closest in-LOS enemy
		if(spitTimer <= 0)
		{
			val target = getClosestEnemyInLOS(g)
			if(target != null)
			{
				//spit at them!

				val spitLoc = loc + (target.loc - loc)/2

				for(i <- 1 to GV.CANNON_SPITNUM)
				{
					val targetLoc = spitLoc.cloone
					targetLoc.x += g.r.nextInt(GV.NORMUNITSIZE*10) - GV.NORMUNITSIZE*5
					targetLoc.y += g.r.nextInt(GV.NORMUNITSIZE*10) - GV.NORMUNITSIZE*5
					//spit a zombie at a random point there
					//draw a line
					val spitLine = SimpleLine(loc.cloone, targetLoc, "black")
					val zed = new Zombie(targetLoc)

					val proj = new ProjectileActor(zed, spitLine, 6)
					proj.forceSpawn = false

					g.addActor(proj)
				}

				//we spit!
				spitTimer = spitRate
			}
		}


		if(direction.is_zero)
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
	}
}




class MolotovThrower(loc_ : Pt)
extends BaseHuman(loc_, GV.PLAYER_HEALTH)
{
	bigDisplayname = "char_human_2_big"
	name = "Scatterbrain Jane"

	var dest = new Pt(-1, -1)

	var hasSeenPlayer = false
	var isLowAmmo = false
	var isLowHealth = false

	var molotovTimer = 0
	val molotovTime = GV.JANE_MOLOTOVRATE

	flammable = false

	def onSeePlayer(g : Game) =
	{
		val phrases = GV.JANE_SEE_PHRASES 
		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)

		hasSeenPlayer = true
	}

	def onLowAmmo(g : Game) =
	{
		val phrases = List("I'm almost out of ammo!",
							"I'm running low on bullets!",
							"We'd better find more ammo soon!")

		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)

		isLowAmmo = true
	}

	def onLowHealth(g : Game) =
	{
		val phrases = List("I'm gonna die!",
							"I'm dying bro!",
							"I'm not gonna make it!",
							"Go on without me!")

		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)

		isLowHealth = true
	}

	override def onDeath(g : Game) = 
	{
		//drop an ammo pack with remaining ammo
		if(gun.ammo > 0)
		{
			val pack = new AmmoPack(loc, gun.ammo)
			g.addActor(pack)
		}
	}

	override def canTakeItem(item : Actor) : Boolean =
	{
		item match 
		{
			case ammo : AmmoPack =>
				if(isLowAmmo)
				{
					isLowAmmo = false
					return true
				}
			case health : HealthPack =>
				if(hp < maxHp/2)
				{	
					isLowHealth = false
					return true
				}
			case _ =>
				return false
		}

		return false
	}

	override def draw(g : Game) =
	{
		val change = lastLoc - loc
		var img = g.images("char_human_2 " + angleToSpriteAngle(change))

		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
	}


	def throwMolotov(g : Game) : Boolean =
	{
		val throwable = new UsableMolotov(this)
		if(throwable.hasTarget(g))
		{
			throwable.use(g)
			return true
		}
		else
		{
			return false
		}
	}

	override def aiMove(g : Game) =
	{
		//Bot humans do everything player humans do except for movement
		super.aiMove(g)

		//handle text and related things
		if(! hasSeenPlayer && hasLosTo(g.player, g))
			onSeePlayer(g)

		if(! isLowAmmo && gun.ammo <= 10)
			onLowAmmo(g)

		if(! isLowHealth && hp <= 20)
			onLowHealth(g)


		//Now decide where to go!
		if(dest.is_neg_one)
		{
			//pick a new dest!
			val destX = g.player.loc.x + g.r.nextInt(GV.NORMUNITSIZE * 10) - GV.NORMUNITSIZE*5
			val destY = g.player.loc.y + g.r.nextInt(GV.NORMUNITSIZE * 10) - GV.NORMUNITSIZE*5
			dest = new Pt(destX, destY)
		}

		//now, is our destination good?

		//is it too far from the player?
		if(g.player.distanceTo(dest) > 200)
			dest = new Pt(-1, -1)

		for(a <- g.acts)
		{
			//are we too close to an enemy?
			if(a.faction == "NA")
			{
				a match 
				{
					case acid : CausticAcid => 
						if(acid.collides(this)) //are we in acid right now?
						{
							dest = loc + (loc - acid.loc)
						}
						else if( acid.collides(loc + (dest - loc).unitStep, size)) //we're gonna step in acid
						{
							dest = new Pt(-1, -1) //pick a better destination
						}
					case _ => //don't care about it
				}
			}
			else if(a.faction != faction)
			{
				//are we too close?
				if(distanceTo(a) < 30 && hasLosTo(a, g))
				{
					//run away
					dest = loc + (loc - a.loc)
				}
			}
		}

		//Now cheap pathfinding as usual
		var dx = min(dest.x - loc.x, 2)
		dx = max(dx, -2)
		
		var dy = min(dest.y - loc.y, 2)
		dy = max(dy, -2)

		val moved = moveLoc(dx, dy, g)
		
		//are we there yet?
		if(!moved || distanceTo(dest) <= 2)
		{
			dest = new Pt(-1, -1)
		}

		if(molotovTimer <= 0)
		{
			//throw a molotov
			if(throwMolotov(g))
			{
				molotovTimer = molotovTime
			}
		}
		else
		{
			molotovTimer -= 1
		}
	}

	override def moveToNewMap(time : Int, g : Game)
	{
		dest = new Pt(-1, -1)
	}
}


class TankRider(loc_ : Pt)
extends Actor(loc_, new Pt(GV.HUGEUNITSIZE, GV.HUGEUNITSIZE),
	GV.TANK_RIDER_HP, 2, 
	"human", 0, "The Colonel")
{
	var bigDisplayname = "tank rider big"
	
	var dest = new Pt(-1, -1)

	var target : Actor = null

	var hasSeenPlayer = false
	var isLowAmmo = false
	var isLowHealth = false

	var direction = new Pt(0, 0)

	val chanceToChangeDirection = 400

	momentumFactor = GV.HUGEMOMENTUMFACTOR

	var healTimer = 0

	canPutSelfOut = true

	def onSeePlayer(g : Game) =
	{
		val phrases = GV.TANK_RIDER_SEE_PHRASES

		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)

		hasSeenPlayer = true
	}

	def onLowAmmo(g : Game) =
	{
		val phrases = List("I'm almost out of ammo!",
							"I'm running low on bullets!",
							"We'd better find more ammo soon!")

		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)

		isLowAmmo = true
	}

	def onLowHealth(g : Game) =
	{
		val phrases = List("Buckaroo's going down!",
							"My little friend's hurtin'!",
							"I'm not gonna make it!",
							"Go on without me!")

		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)

		isLowHealth = true
	}

	override def canTakeItem(item : Actor) : Boolean =
	{
		item match 
		{
			case ammo : AmmoPack =>
				if(isLowAmmo)
				{
					isLowAmmo = false
					return true
				}
			case health : HealthPack =>
				if(isLowHealth)
				{	
					isLowHealth = false
					return true
				}
			case _ =>
				return false
		}

		return false
	}

	override def draw(g : Game) =
	{
		val change = lastLoc - loc
		var img = g.images("tank rider " + angleToSpriteAngle(change))

		g.ctx.drawImage(img, loc.x, loc.y, GV.HUGEUNITSIZE, GV.HUGEUNITSIZE)
	}

	override def aiMove(g : Game) =
	{
		dom.console.log("" + dest)

		if(g.r.nextInt(400) == 0)
			g.addHeadText(this, "Yeehaw!", 10)


		//Get closest in-LOS enemy
		target = getClosestEnemyInLOS(g)

		//if it's really close to us or kinda close to the player, it's a valid target
		if(target != null && (distanceTo(target) < 30 || g.player.distanceTo(target) < 150))
		{
			dest = target.loc
		}
		else if(direction.is_zero)
		{
			//we're stuck, wander in a different direction
			//direction = new Pt(g.r.nextInt(3) - 1, g.r.nextInt(3) - 1)
			//pick a new dest near the player
			val destX = g.player.loc.x + g.r.nextInt(GV.HUGEUNITSIZE * 10) - GV.HUGEUNITSIZE*5
			val destY = g.player.loc.y + g.r.nextInt(GV.HUGEUNITSIZE * 10) - GV.HUGEUNITSIZE*5
			dest = new Pt(destX, destY)
		}

		//compute where we're about to step
		val dx = signum(dest.x - loc.x)
		val dy = signum(dest.y - loc.y)

		//direction = (dest - loc).unitStep
		direction = new Pt(dx, dy)

		//is there someone there to punch?
		if(target != null && target.collides(new Pt(loc.x + direction.x, loc.y + direction.y), new Pt(size.x, size.y)))
		{
			//Hell yeah we are, bit him!
			target.takeDamage(this, GV.TANK_RIDER_DAMAGE, GV.TANK_PUSH, g)
		}

		val moved = moveLoc(direction.x, direction.y, g)

		//try to move there, see if it works
		if(! moved || distanceTo(dest) <= 2)
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

		//process healing
		if(healTimer <= 0)
		{
			gainHealth(1)
			healTimer = GV.TANK_HEALRATE
		}
		else
		{
			healTimer -= 1
		}
		
		
	}

	override def moveToNewMap(time : Int, g : Game)
	{
		dest = new Pt(-1, -1)
	}

	override def onDeath(g : Game) =
	{
		//he hops off the tank
		val hooman = new Human(loc)
		hooman.displayName = "char_human_2"
		hooman.name = "The Colonel"
		g.addActor(hooman)
	}
}



class BarDude(loc_ : Pt, seePhrases_ : List[String], joinPhrases_ : List[String], 
	char_image_ : String, act_ : Actor)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), 50, 0, "human", 0, act_.name)
{
	var char_image = char_image_
	var player_close = false

	val seePhrases = seePhrases_
	val joinPhrases = joinPhrases_
	val act = act_

	var state = "waiting"
	var moveCounter = 100

	override def draw(g : Game)
	{

		if(state == "waiting")
		{
			var img = g.images(char_image + " 0")
			g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
		}
		else if(state == "moving")
		{
			var img = g.images(char_image + " 90")
			g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
		}

	}

	override def aiMove(g : Game)
	{
		//as long as they haven't picked anyone yet
		if(state == "waiting" && ! g.saloon_hasPickedAlly)
		{
			//close enough to say hi
			if(! player_close && distanceTo(g.player) < 100)
			{
				player_close = true
				
				//pick a random phrase and say it
				val phrase = seePhrases(g.r.nextInt(seePhrases.length))
				g.addHeadText(this, phrase, 150)
			}

			//so close they picked us!
			if(distanceTo(g.player) < 30)
			{
				g.saloon_hasPickedAlly = true
				state = "moving"
			}
		}
		else if(state == "moving")
		{
			//move left a bit out of the booth
			if(moveCounter > 0)
			{
				if(moveLoc(-1, 0, g))
				{
					moveCounter -= 1
				}
			}
			else
			{
				act.changeLoc(loc)
				g.removeActor(this)
				g.addActor(act)

				val phrase = joinPhrases(g.r.nextInt(joinPhrases.length))
				g.addHeadText(act, phrase, 150)
			}
		}
	}
}






//stupid dudes
class Bartender(loc_ : Pt)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), 50, 0, "human", 0, "bartender")
{
	var hasSeenPlayer = false

	var speechIndex = 0
	var speechRate = 150
	var speechTimer = 0

	val phrases = List("Well hello there!",
						"Come on in and have a drink.",
						"I bet one of the regulars would help you out,",
						"If you ask nicely, that is.")

	override def draw(g : Game)
	{
		var img = g.images("char_human_2 270")

		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
	}

	def onSeePlayer(g : Game) =
	{
		val phrases = List("Good to see you!",
							"We'll bang ok",
							"Help me!",
							"Save me!",
							"Take the lead!",
							"I'll follow you!",
							"Freakin' ZOMBIES man!",
							"Let's get out of here!",
							"Run for your life!")

		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)

		hasSeenPlayer = true
	}

	override def aiMove(g : Game)
	{
		//handle text and related things
		if(! hasSeenPlayer && hasLosTo(g.player, g))
			hasSeenPlayer = true

		if(hasSeenPlayer && speechIndex < phrases.size)
		{
			if(speechTimer <= 0)
			{
				speechTimer = speechRate
				g.addHeadText(this, phrases(speechIndex), speechRate)
				speechIndex += 1
			}
			else
			{
				speechTimer -= 1
			}
		}
	}
}