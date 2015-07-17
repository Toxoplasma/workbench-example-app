package enemies

import scala.math.{min, max, signum, round}
import org.scalajs.dom

import game._
import globalvars._
import objects._











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
			val target = getClosestValidActorInLOS(a => isEnemy(a) && ! a.acidProof, g)
			//val target = getClosestEnemyInLOS(g)
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



