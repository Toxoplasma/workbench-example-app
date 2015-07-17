package allies

import scala.math.{min, max, signum, round}
import org.scalajs.dom

import game._
import globalvars._
import objects._


class Player(loc_ : Pt)
extends BaseHuman(loc_, GV.PLAYER_HEALTH)
{
	bigDisplayname = "char human 1 big"
	//var usable : UsableItem = null
	var usableItems = new scala.collection.mutable.Stack[UsableItem]()
	var maxUsableItems = GV.PLAYER_MAX_USABLES

	equips += ("legs" -> new EqGeneric("legs", "flip flops", "eq flip flops"))
	equips += ("body" -> new EqGeneric("body", "sweatshirt", "item wet towel"))
	equips += ("cloak" -> new EqGeneric("cloak", "towel", "item wet towel"))
	//equips += ("guns" -> new EqGeneric("cloak", "towel", "item wet towel big"))

	override def canTakeItem(item : Actor) : Boolean =
	{
		item match 
		{
			case ammo : AmmoPack => true
			case health : HealthPack =>
				if(hp < maxHp) true
				else false
			case usable : GroundUsableItem =>
				if(usableItems.length < maxUsableItems) true
				else false

			case _ => true
		}
	}

	override def draw(g : Game) =
	{
		val change = lastLoc - loc
		var img = g.images("char human 1 " + angleToSpriteAngle(change))

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

	//everything any equipment could alter
	def resetAttribs() =
	{
		flammable = true
		canPutSelfOut = true //human specific
		maxUsableItems = 15
		acidProof = false
		damageReduce = 0
		speed = 2
	}

	def equip(e : Equipment, g : Game) =
	{
		dom.console.log("Equipping " + e.name)
		//toss out whatever is in e
		val oldE = equips(e.slot)
		val goldE = new GroundEquip(g.player.loc, oldE)

		var dirX = g.r.nextInt(3) - 1
		var dirY = g.r.nextInt(3) - 1

		//ensure they're not zero
		while(dirX == 0 && dirY == 0)
		{
			dirX = g.r.nextInt(3) - 1
			dirY = g.r.nextInt(3) - 1
		}

		val tossLine = new SimpleLine(g.player.loc.cloone, 
			new Pt(g.player.loc.x + dirX * 50, g.player.loc.y + dirY * 50), "black")

		val tossAct = new ProjectileActor(goldE, tossLine, 5)
		g.addActor(tossAct)

		//now, replace the old equipment with the new
		equips(e.slot) = e

		resetAttribs() //reset everything
		for((_, i) <- equips) //now reequip everything
		{
			i match {
				case g : EqGun => //we don't reequip guns because they actually keep track of important things
				case _ =>  i.equip(this) //re-run the thing
			}
		}
	}
}


class BaseHuman(loc_ : Pt, hp_ : Int)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), hp_, 2, "human", 0, "human")
{
	var bigDisplayname = ""

	val gun : Gun = new EqPistol()
	gun.owner = this
	equips("gun") = gun

	// var gun = new Gun(GV.PISTOL_FIRETIME, GV.PISTOL_DAMAGE, GV.PISTOL_RANGE, GV.PISTOL_APS, GV.PISTOL_AIMTIME, "pistol", "ak47", this)
	// equips += ("gun" -> gun)

	canPutSelfOut = true


	def getGun() : Gun =
	{
		var gun : Gun = null
		equips("gun") match {
			case g : Gun => gun = g
			case _ => dom.console.log("ERROR: non-gun found in equips('gun')")
		}

		gun
	}

	def addAmmo(amount : Int)
	{
		getGun.ammo += amount
	}



	override def aiMove(g : Game) =
	{
		val gun = getGun

		gun.tick()

		//Now, check if we can SHOOT STUFF
		if(gun.canShoot)
		{
			//val closestAct = getClosestEnemyInLOS(g)
			//val closestAct = getClosestEnemyInView(g)
			val closestAct = getBestEnemyInRange(gun.range, g)
			if(closestAct != null)
			{
				gun.shoot(closestAct, g)
				// val distance =  distanceTo(closestAct)

				// //if(distance <= gun.range && inFov(closestAct))
				// if(distance <= gun.range && inFov(closestAct))
				// {
				// 	gun.shoot(closestAct, g)
				// }
			}
		}
	}
}






class Human(loc_ : Pt)
extends BaseHuman(loc_, GV.HUMAN_HEALTH)
{
	bigDisplayname = "char human 3 big"

	var displayName = "char human 3"

	var dest = new Pt(-1, -1)

	var seePhrases = GV.HUMAN_SEE_PHRASES
	var hpPhrases = GV.HUMAN_LOWHP_PHRASES
	var ammoPhrases = GV.HUMAN_LOWAMMO_PHRASES

	var hasSeenPlayer = false
	var isLowAmmo = false
	var isLowHealth = false

	//random dudes have less ammo
	gun.ammo = GV.HUMAN_AMMO

	def sayRandomPhrase(phrases : List[String], g : Game)
	{
		//pick a random phrase and say it
		val phrase = phrases(g.r.nextInt(phrases.length))
		g.addHeadText(this, phrase, 100)
	}

	def onSeePlayer(g : Game) =
	{
		sayRandomPhrase(seePhrases, g)
		hasSeenPlayer = true
	}

	def onLowAmmo(g : Game) =
	{
		sayRandomPhrase(ammoPhrases, g)
		isLowAmmo = true
	}

	def onLowHealth(g : Game) =
	{
		sayRandomPhrase(hpPhrases, g)
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

	def shouldTakeItem(item : Actor) : Boolean = //used by more important children
	{
		item match 
		{
			case ammo : AmmoPack =>
				if(getGun.ammo < GV.BASE_AMMO/2)
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
		var img = g.images(displayName + " " + angleToSpriteAngle(change))

		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)
	}

	

	override def aiMove(g : Game) =
	{
		//Bot humans do everything player humans do except for movement
		super.aiMove(g)

		val gun = getGun
		

		//handle text and related things
		if(! hasSeenPlayer && hasLosTo(g.player, g))
			onSeePlayer(g)

		if(! isLowAmmo && gun.ammo <= 10)
			onLowAmmo(g)

		if(! isLowHealth && hp <= 20)
			onLowHealth(g)


		//Now decide where to go if we need to
		if(dest.is_neg_one)
		{
			//pick a new dest!
			val destX = g.player.loc.x + g.r.nextInt(GV.NORMUNITSIZE * 10) - GV.NORMUNITSIZE*5
			val destY = g.player.loc.y + g.r.nextInt(GV.NORMUNITSIZE * 10) - GV.NORMUNITSIZE*5
			dest = new Pt(destX, destY)
		}


		//now, is our destination good?

		//should we stop and shoot something?
		if(gun.aimTimeMin > 0) //we actually need to stop to shoot
		{
			val closestAct = getClosestEnemyInView(g) //a good target?
			if(closestAct != null)
			{
				dest = loc
			}
		}

		//is it too far from the player?
		if(g.player.distanceTo(dest) > 200)
			dest = new Pt(-1, -1)

		//are we too close to a hazard or enemy?
		for(a <- g.acts)
		{
			//are we too close to a hazard?
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
			//are we too close to an enemy?
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
	}

	override def moveToNewMap(time : Int, g : Game)
	{
		dest = new Pt(-1, -1)
	}
}





class MolotovThrower(loc_ : Pt)
extends Human(loc_)
{
	bigDisplayname = "char human 2 big"
	displayName = "char human 2"
	name = "Scatterbrain Jane"
	maxHp = GV.PLAYER_HEALTH
	hp = GV.PLAYER_HEALTH

	seePhrases = GV.JANE_SEE_PHRASES

	var molotovTimer = 0
	val molotovTime = GV.JANE_MOLOTOVRATE

	flammable = false

	override def canTakeItem(item : Actor) : Boolean = shouldTakeItem(item)


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
		//She acts like a regular bot + throws molotov
		super.aiMove(g)

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
}

class Sapper(loc_ : Pt)
extends Human(loc_)
{
	bigDisplayname = "char human 2 big"
	displayName = "char human 2"
	name = "Fiddler"
	maxHp = GV.PLAYER_HEALTH
	hp = GV.PLAYER_HEALTH

	//gun = new Gun(0, 0, 0, 10000, 0, this)

	val newgun : Gun = new EqNoGun()
	newgun.owner = this
	equips("gun") = newgun

	seePhrases = GV.SAPPER_SEE_PHRASES

	var grenadeTimer = 0
	val grenadeTime = GV.SAPPER_GRENADETIME
	var mineTimer = 0
	val mineTime = GV.SAPPER_MINETIME

	override def canTakeItem(item : Actor) : Boolean = shouldTakeItem(item)


	def throwGrenade(g : Game) : Boolean =
	{
		val throwable = new UsableGrenade(this)
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

	def layMine(g : Game) : Boolean =
	{
		val usable = new UsableLandMine(this)
		usable.use(g)
		return true
	}

	override def aiMove(g : Game) =
	{
		//She acts like a regular bot + throws molotov
		super.aiMove(g)

		if(grenadeTimer <= 0)
		{
			//throw a molotov
			if(throwGrenade(g))
			{
				grenadeTimer = grenadeTime
			}
		}
		else
		{
			grenadeTimer -= 1
		}

		if(mineTimer <= 0)
		{
			//throw a molotov
			if(layMine(g))
			{
				mineTimer = mineTime
			}
		}
		else
		{
			mineTimer -= 1
		}
	}
}

class Sniper(loc_ : Pt)
extends Human(loc_)
{
	bigDisplayname = "char suou big"
	displayName = "char suou"
	name = "Pavlichenko"
	maxHp = GV.PLAYER_HEALTH
	hp = GV.PLAYER_HEALTH

	val newgun : Gun = new EqSniperRifle()
	newgun.owner = this
	equips("gun") = newgun
	//gun = new Gun(GV.SNIPER_FIRETIME, GV.SNIPER_DAMAGE, GV.SNIPER_RANGE, GV.SNIPER_APS, GV.SNIPER_AIMTIME, this)

	seePhrases = GV.SNIPER_SEE_PHRASES 

	override def draw(g : Game) =
	{
		val change = lastLoc - loc
		var img = g.images(displayName + " " + angleToSpriteAngle(change))

		g.ctx.drawImage(img, loc.x - 5, loc.y - 5)
	}

	override def canTakeItem(item : Actor) : Boolean = shouldTakeItem(item)

	// override def aiMove(g : Game) =
	// {
	// 	//nothing special about him except the gun
	// 	super.aiMove(g)
	// }
}


class Medic(loc_ : Pt)
extends Human(loc_)
{
	bigDisplayname = "char human 3 big"
	displayName = "char human 3"
	name = "Medic dude"
	maxHp = GV.PLAYER_HEALTH
	hp = GV.PLAYER_HEALTH

	seePhrases = GV.MEDIC_SEE_PHRASES

	var hpTimer = 0
	val hpTime = GV.MEDIC_MEDRATE

	override def canTakeItem(item : Actor) : Boolean = shouldTakeItem(item)


	def throwHp(g : Game) : Boolean =
	{
		val targetLoc = g.player.loc.cloone
		targetLoc.x += g.r.nextInt(GV.NORMUNITSIZE*10) - GV.NORMUNITSIZE*5
		targetLoc.y += g.r.nextInt(GV.NORMUNITSIZE*10) - GV.NORMUNITSIZE*5
		
		val hp = new MediumHealthPack(targetLoc)
		val throwLine = SimpleLine(loc.cloone, targetLoc, "black")
		
		val proj = new ProjectileActor(hp, throwLine, 6)

		g.addActor(proj)

		true
	}

	override def aiMove(g : Game) =
	{
		//She acts like a regular bot + throws molotov
		super.aiMove(g)

		if(hpTimer <= 0)
		{
			//throw a molotov
			if(throwHp(g))
			{
				hpTimer = hpTime
			}
		}
		else
		{
			hpTimer -= 1
		}
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
		hooman.displayName = "char human 2"
		hooman.name = "The Colonel"
		g.addActor(hooman)
	}
}




//todo: add a build time to this
class GunTurret(loc_ : Pt, hp_ : Double, faction_ : String, ignoreFaction_ : String)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), hp_, 0, faction_, 0, "gun turret")
{
	val gun = new EqSMG()
	gun.owner = this
	equips("gun") = gun


	//var gun = gun_
	var ignoreFaction = ignoreFaction_

	var lastTarget : Pt = new Pt(0, 0)

	var displayName = "turret"

	override def draw(g : Game) =
	{
		val change = loc - lastTarget
		var img = g.images("turret " + angleToSpriteAngle(change))

		g.ctx.drawImage(img, loc.x, loc.y, GV.NORMUNITSIZE, GV.NORMUNITSIZE)



		// g.ctx.fillStyle = s"rgb(100, 100, 100)"
		// g.ctx.fillRect(loc.x, loc.y, size.x, size.y)
	}

	def targetIsValid(a : Actor) : Boolean =
	{
		faction != a.faction && ignoreFaction != a.faction
	}

	override def aiMove(g : Game) =
	{
		gun.tick()

		//Now, check if we can SHOOT STUFF
		if(gun.canShoot)
		{
			val closestAct = getClosestValidActorInLOS(targetIsValid, g)
			if(closestAct != null)
			{
				val distance =  distanceTo(closestAct)

				if(distance <= gun.range)
				{
					lastTarget = closestAct.loc
					gun.shoot(closestAct, g)
				}
			}
		}
	}
}




class BarDude(loc_ : Pt, seePhrases_ : List[String],
	char_image_ : String, sitAngle_ : String, act_ : Actor)
extends Actor(loc_, new Pt(GV.NORMUNITSIZE, GV.NORMUNITSIZE), 50, 0, "human", 0, act_.name)
{
	var char_image = char_image_
	val sitAngle = sitAngle_
	var player_close = false

	val seePhrases = seePhrases_
	//val joinPhrases = joinPhrases_
	val act = act_

	var state = "waiting"
	var moveCounter = 60

	override def draw(g : Game)
	{

		if(state == "waiting")
		{
			var img = g.images(char_image + " " + sitAngle)
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

				//val phrase = joinPhrases(g.r.nextInt(joinPhrases.length))
				//g.addHeadText(act, phrase, 150)
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
		var img = g.images("char human 2 270")

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