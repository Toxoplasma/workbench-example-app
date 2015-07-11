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

	val JANE_MOLOTOVRATE = 300

	val HUMAN_HEALTH = 50
	val HUMAN_AMMO = 500

	val BASE_AMMO = 1000 //player's starting ammo. 100 pistol shots

	val ITEM_CHANCE = 2 //1 in this

	val AMMO_CHANCE = 5
	val HEALTH_CHANCE = 5
	val LANDMINE_CHANCE = 5
	val SPITTERACID_CHANCE = 5
	val PIPEBOMB_CHANCE = 5
	val GRENADE_CHANCE = 5
	val MOLOTOV_CHANCE = 5
	val TURRET_CHANCE = 5


	val PISTOL_DAMAGE = 10
	val PISTOL_RANGE = 100
	val PISTOL_APS = 10
	val PISTOL_FIRETIME = 20

	val AK47_DAMAGE = 7
	val AK47_RANGE = 200
	val AK47_APS = 4
	val AK47_FIRETIME = 7

	val SNIPER_DAMAGE = 50
	val SNIPER_RANGE = 600
	val SNIPER_APS = 20
	val SNIPER_FIRETIME = 50


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


	val TURRET_FIRETIME = 6
	val TURRET_DAMAGE = 4
	val TURRET_RANGE = 150
	val TURRET_APS = 4
	val TURRET_HP = 100


	val FIRE_DAMAGE = 0.5

	val ZOMBIE_ATTACKCOOLDOWN = 10
	val ZOMBIE_DAMAGE = 10

	val CHARGER_CHANCE = 5
	val SPITTER_CHANCE = 5
	val TANK_CHANCE = 1

	val CHARGER_CHARGECOOLDOWN = 300
	val CHARGER_CHARGERANGE = 200
	val CHARGER_CHARGESPEED = 6
	val CHARGER_CHARGINGMOMENTUMFACTOR = 0.2

	val SPITTER_SPITRATE = 400
	val SPITTER_SPITRADIUS = 40
	val SPITTER_SPITREDUCERATE = 4 
	val SPITTER_SPITRANGE = 300
	val SPITTER_SPITDAMAGE = 0.7

	val TANK_HEALTH = 400
	val TANK_DAMAGE = 25
	val TANK_HEALRATE = 5

	//load some pictures

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

	def +(p : Pt) : Pt =
	{
		return new Pt(x + p.x, y + p.y)
	}

	def -(p : Pt) : Pt = return new Pt(x - p.x, y - p.y)

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

