package example
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random



case class Point(x: Int, y: Int)
{
	def +(p: Point) = Point(x + p.x, y + p.y)
	def /(d: Int) = Point(x / d, y / d)
}

class Obj(sizeX_ : Int, sizeY_ : Int)
{
	val sizeX = sizeX_
	val sizeY = sizeY_

	def draw(ctx : dom.CanvasRenderingContext2D) =
	{
		1
	}
}

class Wall(sizeX_ : Int, sizeY_ : Int, locX_ : Int, locY_ : Int) extends Obj(sizeX_, sizeY_)
{
	val locX = locX_
	val locY = locY_

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
	@JSExport
	def main(canvas: html.Canvas): Unit = 
	{
		val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

		//Make the game
		val g = new Game(600, 600)

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
			//Draw the map
			g.drawAll(ctx)
		}

		clear()

		dom.setInterval(() => run, 50)




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
