package enemies

import scala.math.{min, max, signum, round}
import org.scalajs.dom

import game._
import globalvars._
import objects._



class Player(loc_ : Pt)
extends BaseHuman(loc_, GV.PLAYER_HEALTH)
{
	//var usable : UsableItem = null
	var usableItems = new scala.collection.mutable.Stack[UsableItem]()

	override def canTakeItem(item : Actor) : Boolean =
	{
		true
	}

	override def draw(g : Game) =
	{
		//draw from the game thing
		val img = g.images("char_human_1")
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

	var gun = new Gun(GV.PISTOL_FIRETIME, GV.PISTOL_DAMAGE, GV.PISTOL_RANGE, GV.PISTOL_APS, this)

	override def aiMove(g : Game) =
	{
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






class Human(loc_ : Pt)
extends BaseHuman(loc_, GV.HUMAN_HEALTH)
{
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
		g.ctx.fillStyle = "pink"
		g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
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

	override def moveToNewMap(g : Game)
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

	override def draw(g : Game) =
	{
		g.ctx.fillStyle = s"rgb(30, 150, 30)"
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
		if(target != null && target.collides(new Pt(loc.x + direction.x, loc.y + direction.y), new Pt(size.x, size.y)))
		{
			//Hell yeah we are, bit him!
			//g.player.hp -= 10 //ZOMBIEDAMAGE
			target.takeDamage(this, 10, 1, g)
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
			target.takeDamage(this, GV.TANK_DAMAGE, 1.5, g)
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


			if(chargeLine != null && chargeLine.length <= chargeRange && chargeTimer <= 0) //CHARGE!
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
						val moveSuccess = moveLoc(d.x, d.y, g)

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

	override def moveToNewMap(g: Game)
	{
		//tick our timer down a bunch
		//how long it will take us to get there is loc.y/speed
		chargeTimer  = max(0, chargeTimer - (loc.y / speed).toInt)
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

	override def moveToNewMap(g: Game) =
	{
		//tick our timer down a bunch
		//how long it will take us to get there is loc.y/speed
		spitTimer  = max(0, spitTimer - (loc.y / speed).toInt) 
	}

	override def onDeath(g : Game) = 
	{
		//drop a spitter spit
		val spit = new CausticAcid(loc, spitRadius * 2/3, spitReduceRate)
		g.addActor(spit)
	}
}


