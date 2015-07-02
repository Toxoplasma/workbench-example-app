package example
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random

import scala.math.{abs}

case class Point(x: Int, y: Int)
{
	def +(p: Point) = Point(x + p.x, y + p.y)
	def /(d: Int) = Point(x / d, y / d)
}

class Obj(locX_ : Int, locY_ : Int, sizeX_ : Int, sizeY_ : Int)
{
	val sizeX = sizeX_
	val sizeY = sizeY_
	var locX = locX_
	var locY = locY_

	val blocksMovement = true

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
		// val ul = (locX, locY)
		// val ur = (locX + sizeX, locY)
		// val ll = (locX, locY + sizeY)
		// val lr = (locX + sizeX, locY + sizeY)

		// val oul = (loX, loY)
		// val our = (loX + sX, loY)
		// val oll = (loX, loY + sY)
		// val olr = (loX + sX, loY + sY)

		// def insideOther(p : (Int, Int)) : Boolean = 
		// {
		// 	if(p._1 > oul._1 && p._1 < olr._1 && p._2 > oul._2 && p._2 < olr._2) true
		// 	else false
		// }

		// return insideOther(ul) || insideOther(ur) || insideOther(ll) || insideOther(lr)
	}

	def collides(other : Obj) : Boolean =
	{
		collides(other.locX, other.locY, other.sizeX, other.sizeY)
	}

	def draw(ctx : dom.CanvasRenderingContext2D) =
	{
	}
}

class Actor(locX_ : Int, locY_ : Int, sizeX_ : Int, sizeY_ : Int) extends Obj(locX_, locY_, sizeX_, sizeY_)
{

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
				return false
			}
		}
		return true
	}

	def moveLoc(dx : Int, dy : Int, g : Game) : Boolean =
	{
		//Check collisions
		if(canMoveTo(locX + dx, locY + dy, g))
		{
			locX += dx
			locY += dy
			return true
		}
		else
		{
			return false
		}
	}

	override def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = "red"
		ctx.fillRect(locX, locY, sizeX, sizeY)
	}
}

class Wall(locX_ : Int, locY_ : Int, sizeX_ : Int, sizeY_ : Int) extends Obj(locX_, locY_, sizeX_, sizeY_)
{
	override def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		ctx.fillStyle = "blue"
		ctx.fillRect(locX, locY, sizeX, sizeY)
	}
}

class Game(mSizeX : Int, mSizeY : Int)
{
	val mapSizeX = mSizeX
	val mapSizeY = mSizeY

	//Generate the map
	val objs = scala.collection.mutable.Set[Obj]()

	def addObj(newObj : Obj) =
	{
		objs += newObj
	}

	def drawAll(ctx : dom.CanvasRenderingContext2D) =
	{
		objs map(_.draw(ctx))
	}
}

@JSExport
object ScalaJSExample 
{
	def handlePlayerMovement(player : Actor, keys : collection.mutable.Set[Int], g : Game) =
	{
		if (keys(38)) player.moveLoc(0, -2, g)
	    if (keys(37)) player.moveLoc(-2, 0, g)
	    if (keys(39)) player.moveLoc(2, 0, g)
	    if (keys(40)) player.moveLoc(0, 2, g)
	}



	@JSExport
	def main(canvas: html.Canvas): Unit = 
	{
		dom.console.log("butts")
		val keysDown = collection.mutable.Set[Int]()
		val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

		//Make the game
		val g = new Game(600, 600)

		//Make the player
		val player = new Actor(50, 50, 20, 20)
		g.addObj(player)

		//Make the map
		val wall = new Wall(100, 100, 50, 200)
		g.addObj(wall)


		def clear() = 
		{
			ctx.fillStyle = "black"
			ctx.fillRect(0, 0, 600, 600)
		}

		def run()
		{
			//handle player movement
			handlePlayerMovement(player, keysDown, g)

			//Clear the screen
			clear()

			//Draw the map
			g.drawAll(ctx)
		}

		clear()


		// dom.onkeypress = {(e: dom.KeyboardEvent) =>
	 //      if (e.keyCode.toInt == 32) bullets = player +: bullets
	 //    }
	    dom.onkeydown = {(e: dom.KeyboardEvent) =>
	      keysDown.add(e.keyCode.toInt)
	    }
	    
	    dom.onkeyup = {(e: dom.KeyboardEvent) =>
	      keysDown.remove(e.keyCode.toInt)
	    }


		dom.setInterval(() => run, 20)



		// var count = 0
		// var p = Point(0, 0)
		// // val corners = Seq(Point(255, 255), Point(0, 255), Point(128, 0))
		// val corners = Seq(Point(600, 600), Point(0, 600), Point(300, 0))

		// def clear() = 
		// {
		// 	ctx.fillStyle = "black"
		// 	ctx.fillRect(0, 0, 600, 600)
		// }

		// def run = for (i <- 0 until 10)
		// {
		// 	if (count % 3000 == 0) clear()

		// 	count += 1
		// 	p = (p + corners(Random.nextInt(3))) / 2

		// 	// val height = 512.0 / (255 + p.y)
		// 	// val r = (p.x * height).toInt
		// 	// val g = ((255-p.x) * height).toInt
		// 	// val b = p.y
		// 	val r = 255
		// 	val g = 0
		// 	val b = 0
		// 	ctx.fillStyle = s"rgb($r, $g, $b)"

		// 	ctx.fillRect(p.x, p.y, 1, 1)
		// }

		// clear()

		// dom.setInterval(() => run, 50)
	}
}
