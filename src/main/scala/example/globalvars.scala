package globalvars

import scala.math.{sqrt, pow, Pi}

object GV
{
	val GAMEX = 600
	val GAMEY = 600

	val FULLX = 900
	val FULLY = 600

	val OFFMAPCUTOFF = 600

	val NORMUNITSIZE = 10
	val BIGUNITSIZE = 14
	val HUGEUNITSIZE = 20

	val NORMMOMENTUMFACTOR = 0.6
	val BIGMOMENTUMFACTOR = 0.4
	val HUGEMOMENTUMFACTOR = 0.1

	val MAX_FOV = 80 * Pi/180


	val PLAYER_HEALTH = 100
	val PLAYER_THROWDISTANCE = 50
	val PLAYER_THROWSPEED = 6
	val PLAYER_MAX_USABLES = 15

	val HUMAN_HEALTH = 50
	val HUMAN_AMMO = 500

	val BASE_AMMO = 1000 //player's starting ammo. 100 pistol shots

	val ITEM_CHANCE = 2 //1 in this
	val EQUIP_CHANCE = 10 //1 in 10 items is an equip

	val AMMO_CHANCE = 5
	val HEALTH_CHANCE = 5
	val LANDMINE_CHANCE = 5
	val SPITTERACID_CHANCE = 5
	val PIPEBOMB_CHANCE = 5
	val GRENADE_CHANCE = 5
	val MOLOTOV_CHANCE = 5
	val TURRET_CHANCE = 5
	val CONTROLLER_CHANCE = 5


	val PISTOL_DAMAGE = 10
	val PISTOL_RANGE = 100
	val PISTOL_APS = 10
	val PISTOL_FIRETIME = 20
	val PISTOL_AIMTIME = 0

	val AK47_DAMAGE = 8
	val AK47_RANGE = 200
	val AK47_APS = 4
	val AK47_FIRETIME = 7
	val AK47_AIMTIME = 1

	val MG_DAMAGE = 15
	val MG_RANGE = 300
	val MG_APS = 6
	val MG_FIRETIME = 6
	val MG_AIMTIME = 25

	val SNIPER_DAMAGE = 50
	val SNIPER_RANGE = 600
	val SNIPER_APS = 20
	val SNIPER_FIRETIME = 50
	val SNIPER_AIMTIME = 50

	val SMG_FIRETIME = 6
	val SMG_DAMAGE = 4
	val SMG_RANGE = 150
	val SMG_APS = 4
	val SMG_AIMTIME = 0

	val MEDIC_MEDRATE = 500


	val AMMOPACK_AMOUNT = 500

	val HEALTHPACK_AMOUNT = 25

	val LANDMINE_RADIUS = 50
	val LANDMINE_DAMAGE = 70
	val LANDMINE_DELAY = 30

	val PIPEBOMB_TIME = 200
	val PIPEBOMB_DAMAGE = 50
	val PIPEBOMB_RADIUS = 50

	val GRENADE_DAMAGE = 100
	val GRENADE_RADIUS = 50

	val MOLOTOV_RADIUS = 40
	val MOLOTOV_DURATION = 5
	
	val TURRET_HP = 100

	val ZOMBIE_CONTROLLER_RADIUS = 100
	val ZOMBIE_CONTROLLER_TIMER = 200


	val FIRE_DAMAGE = 0.5
	val FIRE_EXTINGUISHTIME = 40
	val BURN_TIME = 400

	val ZOMBIE_ATTACKCOOLDOWN = 20
	val ZOMBIE_DAMAGE = 10
	
	val ZOMBIESPAWNER_SPAWNRATE = 30
	val ZOMBIESPAWNER_SPEED = .3

	val CHARGER_CHANCE = 5
	val SPITTER_CHANCE = 5
	val ICESPITTER_CHANCE = 50
	val TANK_CHANCE = 1
	val CANNON_CHANCE = 1

	val CHARGER_CHARGECOOLDOWN = 300
	val CHARGER_CHARGERANGE = 200
	val CHARGER_CHARGESPEED = 6
	val CHARGER_CHARGINGMOMENTUMFACTOR = 0.2

	val SPITTER_SPITRATE = 400
	val SPITTER_SPITRADIUS = 40
	val SPITTER_SPITREDUCERATE = 4 
	val SPITTER_SPITRANGE = 300
	val SPITTER_SPITDAMAGE = 0.7
	val SPITTER_SPITRESIST = 10

	val TANK_HEALTH = 400
	val TANK_DAMAGE = 25
	val TANK_HEALRATE = 5
	val TANK_PUSH = 1.5

	val CANNON_HEALTH = 500
	val CANNON_DAMAGE = 10
	val CANNON_HEALRATE = 5
	val CANNON_SPITRATE = 200 //100 for challenging boss?
	val CANNON_SPITNUM = 5 //20 for challenging boss?


	val SAPPER_GRENADETIME = 200
	val SAPPER_MINETIME = 250

	val JANE_MOLOTOVRATE = 200

	val TANK_RIDER_HP = TANK_HEALTH * 5 / 4
	val TANK_RIDER_DAMAGE = TANK_DAMAGE * 5 / 4
	


	val SALOON_LEVEL_MIN = 5
	val SALOON_LEVEL_RANGE = 6
	val SALOON_NPCNUM = 3

	val JANE_BAR_PHRASES = List("You need something burned?",
								"I just wanna watch the world burn",
								"I bring the heat")
	val JANE_SEE_PHRASES = List("Wassup!",
							"We'll bang ok",
							"Let's kill some dudes!",
							"Awww yiss!",
							"I'll bring the fire",
							"I like to set things on fire",
							"Watch, or you might get burned!",
							"They don't stand a chance!")

	val TANK_RIDER_BAR_PHRASES = List("Me and Bessy will kick some butt!",
								"Let's ride!")
	val TANK_RIDER_SEE_PHRASES = List("Yeehaw!",
							"We'll bang ok",
							"Giddyup!",
							"Whoa there Buckaroo!",
							"Woooooaaaaa!",
							"Yippee!",
							"Yippekaya!",
							"Ride 'em cowboy!")

	val SNIPER_SEE_PHRASES = List("I'll take them down",
									"They won't see it coming")
	val SNIPER_BAR_PHRASES = List("I shoot guys from far away",
								"Snipey snipey")

	val SAPPER_SEE_PHRASES = List("Zooooombs",
									"write more lines please?")
	val SAPPER_BAR_PHRASES = List("Need something blown up?",
								"Let's blow it sky high!")

	val MEDIC_SEE_PHRASES = List("Have a med kit",
									"Who needs healing?")
	val MEDIC_BAR_PHRASES = List("I can keep you alive",
								"You need a medic?")

	val HUMAN_SEE_PHRASES = List("Good to see you!",
							"We'll bang ok",
							"Help me!",
							"Save me!",
							"Take the lead!",
							"I'll follow you!",
							"Freakin' ZOMBIES man!",
							"Let's get out of here!",
							"Run for your life!")
	val HUMAN_LOWHP_PHRASES =List("I'm gonna die!",
							"I'm dying bro!",
							"I'm not gonna make it!",
							"Tell my kids I love them!")
	val HUMAN_LOWAMMO_PHRASES = List("I'm almost out of ammo!",
							"I'm running low on bullets!",
							"We'd better find more ammo soon!")

	
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

	def *(n : Double) : Pt =
	{
		return new Pt(x * n, y * n)
	}

	def /(n : Double) : Pt =
	{
		return new Pt(x / n, y / n)
	}

	def +(p : Pt) : Pt =
	{
		return new Pt(x + p.x, y + p.y)
	}

	def -(p : Pt) : Pt = return new Pt(x - p.x, y - p.y)

	def ==(p : Pt) : Boolean = x == p.x && y == p.y

	override def toString(): String = "(" + x + ", " + y + ")"

	def pythagLength() = sqrt(x * x + y * y)

	def unitStep() = new Pt(x / pythagLength, y / pythagLength)
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


class HeadText(text_ : String, time_ : Int)
{
	var text = text_
	var time = time_

	def tick() =
	{
		time -= 1
	}
}

