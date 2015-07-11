package game

import org.scalajs.dom

import globalvars._
import objects._
import enemies._


class Game(mSizeX : Int, mSizeY : Int, ctx_ : dom.CanvasRenderingContext2D)
{
	var difficulty = 0
	var score = 0

	val r = new scala.util.Random()

	val mapSizeX = mSizeX
	val mapSizeY = mSizeY
	val ctx = ctx_

	//graphics-related
	val linesToDraw = scala.collection.mutable.Buffer[SimpleLine]()
	val images = scala.collection.mutable.HashMap[String, dom.raw.HTMLImageElement]()
	loadAllImages()

	//all the things
	val objs = scala.collection.mutable.Set[Obj]()
	
	//actors & things with ais
	val acts = scala.collection.mutable.Set[Actor]()

	//actors that are coming later onto the stage, or delayed effects, or whatever
	val delayedActs = scala.collection.mutable.Set[DelayedActor]()

	//text above people's heads :D
	val headTexts = scala.collection.mutable.HashMap[Obj, HeadText]()

	//make the player
	val player = new Player(new Pt(GV.GAMEX / 2, GV.GAMEY/2))
	player.name = "Tim Jones"
	addActor(player)

	def addHeadText(o : Obj, text : String, time : Int)
	{
		val ht = new HeadText(text, time)
		headTexts += (o -> ht)
	}

	def collision(ob : Obj) : Boolean =
	{
		for(o <- objs)
		{
			if(o != ob && o.blocksMovement && o.collides(ob))
			{
				//dom.console.log(ob.getClass + " collision with " + o.getClass)
				return true
			}
		}
		return false
	}

	def withinBounds(p : Pt) : Boolean =
	{
		if(p.x < 0 || p.y < 0 || p.x > GV.GAMEX || p.y > GV.GAMEY) return false
		else return true
	}

	//are both x and y outside bounds? useful for drawing shadows
	def totallyOutsideBounds(p : Pt) : Boolean =
	{
		if((p.x >= 0 && p.x <= GV.GAMEX) ||
		   (p.y >= 0 && p.y <= GV.GAMEY)) return false
		else return true
	}

	def addObj(newObj : Obj) =
	{
		objs += newObj
	}

	def addActor(newAct : Actor) = 
	{
		acts += newAct
		objs += newAct
	}

	def addDelayed(newDel : DelayedActor) =
	{
		delayedActs += newDel
	}
	def removeDelayed(del : DelayedActor) =
	{
		delayedActs remove del
	}

	def removeActor(act : Actor) =
	{
		acts remove act
		objs remove act

		headTexts remove act
	}


	def runAllAIs() =
	{
		//run momentum
		acts map(_.handleMomentum(this))

		//run effects
		acts map(_.runEffects(this))

		//run all ais
		acts map(_.aiMove(this))

		//tick all delayed dudes
		delayedActs map(_.aiMove(this))

		//tick all head texts
		for((o, ht) <- headTexts)
		{
			if(ht.time <= 0)
				headTexts remove o
			else
				ht.tick()
		}
	}


	//DRAWING FUNCTIONS

	def loadAllImages() =
	{
		var img = loadImage("images/ak47.png")
		images += ("ak47" -> img)

		img = loadImage("images/item_landmine.png")
		images += ("item_landmine" -> img)

		img = loadImage("images/item health kit.png")
		images += ("item_healthkit" -> img)

		img = loadImage("images/item ammo box.png")
		images += ("item_ammobox" -> img)

		img = loadImage("images/item molotov.png")
		images += ("item_molotov" -> img)

		img = loadImage("images/item pipe bomb.png")
		images += ("item_pipebomb" -> img)

		img = loadImage("images/item acid.png")
		images += ("item_acid" -> img)

		img = loadImage("images/char 1 big.png")
		images += ("item_gunturret" -> img)


		//img = loadImage("/Users/ravi/Documents/prog/dogedots/target/scala-2.11/character 1.png")
		img = loadImage("images/char 1 0.png")
		images += ("char_human_1 0" -> img)
		img = loadImage("images/char 1 90.png")
		images += ("char_human_1 90" -> img)
		img = loadImage("images/char 1 180.png")
		images += ("char_human_1 180" -> img)
		img = loadImage("images/char 1 270.png")
		images += ("char_human_1 270" -> img)
		img = loadImage("images/char 1 45.png")
		images += ("char_human_1 45" -> img)
		img = loadImage("images/char 1 135.png")
		images += ("char_human_1 135" -> img)
		img = loadImage("images/char 1 225.png")
		images += ("char_human_1 225" -> img)
		img = loadImage("images/char 1 315.png")
		images += ("char_human_1 315" -> img)

		img = loadImage("images/char 1 big.png")
		images += ("char_human_1_big" -> img)

		img = loadImage("images/char 2 0.png")
		images += ("char_human_2 0" -> img)
		img = loadImage("images/char 2 90.png")
		images += ("char_human_2 90" -> img)
		img = loadImage("images/char 2 180.png")
		images += ("char_human_2 180" -> img)
		img = loadImage("images/char 2 270.png")
		images += ("char_human_2 270" -> img)
		img = loadImage("images/char 2 45.png")
		images += ("char_human_2 45" -> img)
		img = loadImage("images/char 2 135.png")
		images += ("char_human_2 135" -> img)
		img = loadImage("images/char 2 225.png")
		images += ("char_human_2 225" -> img)
		img = loadImage("images/char 2 315.png")
		images += ("char_human_2 315" -> img)

		img = loadImage("images/char 2 big.png")
		images += ("char_human_2_big" -> img)

		img = loadImage("images/char 3 0.png")
		images += ("char_human_3 0" -> img)
		img = loadImage("images/char 3 90.png")
		images += ("char_human_3 90" -> img)
		img = loadImage("images/char 3 180.png")
		images += ("char_human_3 180" -> img)
		img = loadImage("images/char 3 270.png")
		images += ("char_human_3 270" -> img)
		img = loadImage("images/char 3 45.png")
		images += ("char_human_3 45" -> img)
		img = loadImage("images/char 3 135.png")
		images += ("char_human_3 135" -> img)
		img = loadImage("images/char 3 225.png")
		images += ("char_human_3 225" -> img)
		img = loadImage("images/char 3 315.png")
		images += ("char_human_3 315" -> img)

		img = loadImage("images/char 3 big.png")
		images += ("char_human_3_big" -> img)

		img = loadImage("images/tank rider 0.png")
		images += ("tank rider 0" -> img)
		img = loadImage("images/tank rider 90.png")
		images += ("tank rider 90" -> img)
		img = loadImage("images/tank rider 180.png")
		images += ("tank rider 180" -> img)
		img = loadImage("images/tank rider 270.png")
		images += ("tank rider 270" -> img)
		img = loadImage("images/tank rider 45.png")
		images += ("tank rider 45" -> img)
		img = loadImage("images/tank rider 135.png")
		images += ("tank rider 135" -> img)
		img = loadImage("images/tank rider 225.png")
		images += ("tank rider 225" -> img)
		img = loadImage("images/tank rider 315.png")
		images += ("tank rider 315" -> img)

		img = loadImage("images/tank rider big.png")
		images += ("tank rider big" -> img)

	}

	def loadImage(path : String) =
	{
		val img: dom.raw.HTMLImageElement = 
		{
			val _image = dom.document.createElement("img").asInstanceOf[dom.raw.HTMLImageElement]
			_image.src  = path
			_image
		}

		img
		//ctx.drawImage(testimage, 0, 0)
	}


	def extendLineOffMap(s : Pt, slope : Pt) =
	{
		var e = s.cloone
		if(slope.x == 0 || slope.y == 0)
		{
			while(withinBounds(e))
			{
				e += slope
			}
		}
		else
		{
			while(! totallyOutsideBounds(e))
			{
				e += slope
			}
		}

		e
	}

	//push two points so the line between them is off the map
	def extendLinesOffMap(p1_ : Pt, s1 : Pt, p2_ : Pt, s2 : Pt) =
	{
		//make a dummy
		val dum = new Obj(new Pt(0, 0), new Pt(GV.GAMEX, GV.GAMEY))

		var p1 = p1_.cloone
		var p2 = p2_.cloone

		var lineBetween = new SimpleLine(p1, p2, "black")

		while(dum.collidesLine(lineBetween))
		{
			//bump both by their slopes
			p1 += s1
			p2 += s2

			lineBetween = new SimpleLine(p1, p2, "black")
		}

		//now they're good!
		(p1, p2)
	}
	
	//Computes the line between the two points, and draws a cast shadow from that line
	def drawLineShadow(p1 : Pt, p2 : Pt) =
	{
		val (extp1, extp2) = extendLinesOffMap(p1, p1 - player.center, p2, p2 - player.center)

		//now draw the line
		ctx.beginPath
		ctx.moveTo(p1.x, p1.y)
		ctx.lineTo(extp1.x, extp1.y)
		ctx.lineTo(extp2.x, extp2.y)
		ctx.lineTo(p2.x, p2.y)
		ctx.fillStyle = s"rgb(100, 100, 100)"
		ctx.fill()
	}

	//draw the cast shadows for LOS for all los-blocking objects
	def drawShadows() =
	{
		//draw shadows
		for(o <- objs)
		{
			if(o != player && o.blocksLos)
			{
				var p1 : Pt = o.upperLeft
				var p2 : Pt = o.upperRight

				//compute which octant we're in
				if(player.center.x > o.loc.x && 
					player.center.x <= o.loc.x + o.size.x && 
					player.center.y <= o.loc.y) //straight above
				{
					p1 = o.upperLeft
					p2 = o.upperRight
				}
				else if(player.center.x > o.loc.x && 
						player.center.x <= o.loc.x + o.size.x && 
						player.center.y > o.loc.y) //straight below
				{
					p1 = o.lowerLeft
					p2 = o.lowerRight
				}
				else if(player.center.x > o.loc.x + o.size.x && 
						player.center.y > o.loc.y && 
						player.center.y <= o.loc.y + o.size.y) //straight right
				{
					p1 = o.upperRight
					p2 = o.lowerRight
				}
				else if(player.center.x < o.loc.x && 
						player.center.y > o.loc.y && 
						player.center.y <= o.loc.y + o.size.y) //straight left
				{
					p1 = o.upperLeft
					p2 = o.lowerLeft
				}
				else if(player.center.x > o.loc.x + o.size.x && 
						player.center.y <= o.loc.y) //above right
				{
					p1 = o.upperLeft
					p2 = o.lowerRight
				}
				else if(player.center.x > o.loc.x + o.size.x && 
						player.center.y > o.loc.y + o.size.y) //lower right
				{
					p1 = o.upperRight
					p2 = o.lowerLeft
				}
				else if(player.center.x <= o.loc.x && 
						player.center.y > o.loc.y + o.size.y) //lower left
				{
					p1 = o.upperLeft
					p2 = o.lowerRight
				}
				else if(player.center.x <= o.loc.x && 
						player.center.y <= o.loc.y) //above left
				{
					p1 = o.lowerLeft
					p2 = o.upperRight
				}

				drawLineShadow(p1, p2)
			}
		}
	}

	def drawHeadTexts()
	{
		ctx.fillStyle = "black"
		ctx.font = "12px sans-serif"
				
		for((o, ht) <- headTexts)
		{
			if(player.hasLosTo(o, this))
				ctx.fillText(ht.text, o.loc.x - ht.text.length*2, o.loc.y - 6)
		}
	}

	def drawAllCond(cond : (Obj) => Boolean) =
	{
		//Draw all the objects
		for(o <- objs)
		{
			if(cond(o))
				o.draw(this)
		}
	}

	//draw all the things!!!
	def drawAll() =
	{

		//draw all low-priority
		//drawAllCond(o => o.lowPriority && player.hasLosTo(o, this))
		drawAllCond(o => o.lowPriority)

		//draw lines
		while(! linesToDraw.isEmpty)
		{
			val line = linesToDraw.remove(0)

			val s = line.start
			val e = line.end

			//if either end is visible
			//if(player.hasCenterLosTo(s, null, this) || player.hasCenterLosTo(e, null, this))
			if(true)
			{
				//now draw it
				ctx.beginPath()
				ctx.moveTo(s.x, s.y)
				ctx.fillStyle = line.color
				ctx.lineTo(e.x, e.y)
				ctx.stroke
			}
		}

		//drawAllCond(o => ! o.lowPriority && ! o.alwaysVisible && player.hasLosTo(o, this))
		drawAllCond(o => ! o.lowPriority && !o.highPriority && ! o.alwaysVisible)
		drawAllCond(o => o.alwaysVisible)

		drawAllCond(o => o.highPriority)

		drawShadows()
		
		//draw all the walls overtop the shadows
		val wally = new Wall(new Pt(0, 0), new Pt(0, 0))
		drawAllCond(o => o.getClass == wally.getClass)
		
		drawHeadTexts()
	}




	//Map generation



	def genFarmhouse(iloc : Pt, isize : Pt) =
	{
		val overlap = 5
		val minSize = 22
		val maxSize = 100

		//make a starter room
		val sWidth = r.nextInt(maxSize - minSize) + minSize
		val sHeight = r.nextInt(maxSize - minSize) + minSize
		//val starter = new Wall(new Pt(100, 100), new Pt(100, 100))
		val starter = new Wall(
			new Pt(r.nextInt(isize.x.toInt - sWidth) + iloc.x, r.nextInt(isize.y.toInt - sHeight) + iloc.y), 
			new Pt(sWidth, sHeight))

		if(! collision(starter))
		{
			val rooms = scala.collection.mutable.Buffer((starter, "none")) //room, which wall it should have a door in

			var last = starter
			var lastRelation = ""

			for(i <- 1 to 8)
			{
				val height = r.nextInt(maxSize - minSize) + minSize
				val width = r.nextInt(maxSize - minSize) + minSize

				var newRoom : Wall = null
				var relation = "none" //which of our walls is related to the last room, i.e. which should have a door
				
				//pick which wall we're going to add it to
				val whichWall = r.nextInt(4)

				if(whichWall == 0 && lastRelation != "east")
				{   //east wall
					newRoom = new Wall(new Pt(last.loc.x + last.size.x - overlap, last.loc.y),
										   new Pt(width, height))
					relation = "west"
				}
				else if(whichWall == 1 && lastRelation != "south")
				{   //south wall
					newRoom = new Wall(new Pt(last.loc.x, last.loc.y + last.size.y - overlap),
										   new Pt(width, height))
					relation = "north"
				}
				else if(whichWall == 2 && lastRelation != "west")
				{   //west wall
					newRoom = new Wall(new Pt(last.loc.x - width + overlap, last.loc.y),
										   new Pt(width, height))
					relation = "east"
				}
				else if(whichWall == 3 && lastRelation != "north")
				{   //north wall
					newRoom = new Wall(new Pt(last.loc.x, last.loc.y - height + overlap),
										   new Pt(width, height))
					relation = "south"
				}

				//check if the new room is off the map
				if(newRoom != null && (
					newRoom.loc.x < 0 || newRoom.loc.y < 0 || 
					newRoom.loc.x + newRoom.size.x > GV.GAMEX ||
					newRoom.loc.y + newRoom.size.y > GV.GAMEY ))
				{
					newRoom = null
				}

				//check the rooms collisions with other objects already on the map (i.e. other houses)
				else if(newRoom != null && collision(newRoom))
				{
					newRoom = null
				}

				//check the new room collisions with the other rooms to avoid anything too weird
				for((r, _) <- rooms)
				{
					if(newRoom != null && 
						r != last && 
						r.collides(newRoom)) //we always collide with last
						newRoom = null
				}


				//add it
				//addObj(newRoom)
				//if we actually added it
				if(newRoom != null)
				{
					rooms += new Tuple2(newRoom, relation)
					last = newRoom
					lastRelation = relation
				}
			}

			//now devolve them into four walls per room

			for(i <- 0 to rooms.length - 1)
			{
				val (room, relation) = rooms(i)

				//first put in something! either a zombie or an item.
				val x = r.nextInt(room.size.x.toInt - 2*overlap - GV.NORMUNITSIZE) + overlap + room.loc.x
				val y = r.nextInt(room.size.y.toInt - 2*overlap - GV.NORMUNITSIZE) + overlap + room.loc.y
				if(r.nextInt(GV.ITEM_CHANCE) == 0) //item
				{
					var newItem = new DummyItem(new Pt(x, y)) //: Actor = null
					//dom.console.log("dummy")

					addActor(newItem)
				}
				else //zomb
				{
					val newZomb = new Zombie(new Pt(x, y))
					addActor(newZomb)
				}

				var nrelation : String = ""
				if(i != rooms.length - 1) nrelation = rooms(i+1)._2

				val north = new Wall(room.upperLeft, 
					new Pt(room.size.x, overlap))
				val west = new Wall(room.upperLeft, 
					new Pt(overlap, room.size.y))
				val south = new Wall(new Pt(room.loc.x, room.loc.y + room.size.y - overlap), 
					new Pt(room.size.x, overlap))
				val east = new Wall(new Pt(room.loc.x + room.size.x - overlap, room.loc.y), 
					new Pt(overlap, room.size.y))

				val walls = scala.collection.mutable.Set(north, east, south, west)

				def carveWall(w : Wall) =
				{
					val (w1, w2) = w.splitWithDoorAt(0, 20)
						walls remove w
						walls add w1
						walls add w2
				}

				var dir = ""
				//are we the first or last one? then we might need to carve a door to the outside
				if(i == 0 || i == rooms.length - 1) //definite door
				{
					val relations = scala.collection.mutable.Set("north", "east", "south", "west")
					relations remove relation
					if(nrelation == "west") relations remove "east"
					if(nrelation == "north") relations remove "south"
					if(nrelation == "south") relations remove "north"
					if(nrelation == "east") relations remove "west"
					//relations remove nrelation

					dir = relations.toList(r.nextInt(2))
				}

				// dom.console.log(i)
				// dom.console.log("  dir " + dir)
				// dom.console.log("  rel " + relation)
				// dom.console.log("  nrel " + nrelation)
				//carve into four
				if(relation == "west" || dir == "west") carveWall(west)
				if(relation == "north" || dir == "north") carveWall(north)
				if(relation == "south" || dir == "south") carveWall(south)
				if(relation == "east" || dir == "east") carveWall(east)

				if(nrelation == "west") carveWall(east)
				if(nrelation == "north") carveWall(south)
				if(nrelation == "south") carveWall(north)
				if(nrelation == "east") carveWall(west)
				
			

				walls map(addObj(_))
			}
		}
	}

	def spawnMainWalls() =
	{
		//Bounding walls
		//val topWallLeft = new Wall(new Pt(0, 0), new Pt(GV.GAMEX/2 - 50, 5))
		//val topWallRight = new Wall(new Pt(GV.GAMEX/2 + 50, 0), new Pt(GV.GAMEX/2 - 50, 5))
		val leftWall = new Wall(new Pt(0, 0), new Pt(5, GV.GAMEY))
		val rightWall = new Wall(new Pt(GV.GAMEX - 5, 0), new Pt(5, GV.GAMEY))
		//val botWallLeft = new Wall(new Pt(0, GV.GAMEY - 5), new Pt(GV.GAMEX / 2 - 50, 5))
		//val botWallRight = new Wall(new Pt(GV.GAMEX/2 + 50, GV.GAMEY - 5), new Pt(GV.GAMEX/2 - 50, 5))

		//addObj(topWallLeft)
		//addObj(topWallRight)
		addObj(leftWall)
		addObj(rightWall)
		//addObj(botWallLeft)
		//addObj(botWallRight)

		/*val topTopBound = new Wall(new Pt(GV.GAMEX / 2 - 50, -20), new Pt(100, 5))
		val topLeftBound = new Wall(new Pt(GV.GAMEX / 2 - 55, -20), new Pt(5, 20))
		val topRightBound = new Wall(new Pt(GV.GAMEX / 2 + 50, -20), new Pt(5, 20))

		val botBotBound = new Wall(new Pt(GV.GAMEX / 2 - 50, GV.GAMEY + 20), new Pt(100, 5))
		val botLeftBound = new Wall(new Pt(GV.GAMEX / 2 - 55, GV.GAMEY), new Pt(5, 20))
		val botRightBound = new Wall(new Pt(GV.GAMEX / 2 + 50, GV.GAMEY), new Pt(5, 20))*/

		val topTopBound = new Wall(new Pt(0, -6), new Pt(GV.GAMEX, 4))
		val topLeftBound = new Wall(new Pt(0, -6), new Pt(5, 6))
		val topRightBound = new Wall(new Pt(GV.GAMEX -5, -6), new Pt(5, 6))

		val botBotBound = new Wall(new Pt(0, GV.GAMEY + 20), new Pt(GV.GAMEX, 5))
		val botLeftBound = new Wall(new Pt(0, GV.GAMEY), new Pt(5, 20))
		val botRightBound = new Wall(new Pt(GV.GAMEX - 5, GV.GAMEY), new Pt(5, 20))

		addObj(topTopBound)
		addObj(topLeftBound)
		addObj(topRightBound)
		addObj(botBotBound)
		addObj(botLeftBound)
		addObj(botRightBound)
	}

	def spawnItemFromDummy(d : DummyItem) =
	{
		var randItem : Item = null

		val ammopack = new MediumAmmoPack(d.loc)
		val healthpack = new MediumHealthPack(d.loc)
		val mine = new GroundUsableItem(d.loc, new UsableLandMine(null), "item_landmine")
		val spit = new GroundUsableItem(d.loc, new UsableSpitterAcid(null), "item_acid")
		val pipe = new GroundUsableItem(d.loc, new UsablePipeBomb(null), "item_pipebomb")
		val grenade = new GroundUsableItem(d.loc, new UsableGrenade(null), "item_pipebomb")
		val molotov = new GroundUsableItem(d.loc, new UsableMolotov(null), "item_molotov")
		val turret = new GroundUsableItem(d.loc, new UsableGunTurret(null), "item_gunturret")

		//now put them in a weighted list so to speak
		val ammopackL = Seq.fill(GV.AMMO_CHANCE){ammopack}
		val healthpackL = Seq.fill(GV.HEALTH_CHANCE){healthpack}
		val mineL = Seq.fill(GV.LANDMINE_CHANCE){mine}
		val spitL = Seq.fill(GV.SPITTERACID_CHANCE){spit}
		val pipeL = Seq.fill(GV.PIPEBOMB_CHANCE){pipe}
		val grenadeL = Seq.fill(GV.GRENADE_CHANCE){grenade}
		val molotovL = Seq.fill(GV.MOLOTOV_CHANCE){molotov}
		val turretL = Seq.fill(GV.TURRET_CHANCE){turret}

		val choices = ammopackL ++ healthpackL ++ mineL ++ spitL ++ pipeL ++ 
			grenadeL ++ molotovL ++ turretL

		val choice = choices(r.nextInt(choices.length))

		//removeActor(d)
		addActor(choice)
	}

	def isDummy(d : Obj) : Boolean =
	{
		d match {
			case blah : DummyItem => true
			case _ => false
		}
		
	}

	def spawnItemsFromDummies() =
	{
		var tot = 0
		//val dummies = acts filter(isDummy)
		val dummies = acts collect {case d: DummyItem => d}
		//replace them all with real items
		for(d <- dummies)
		{
			d match
			{
				case dummy : DummyItem => spawnItemFromDummy(dummy); tot += 1
				case _ => //welp
			}
		}

		//remove all the dummy items
		for(d <- dummies)
		{
			d match
			{
				case dummy : DummyItem => removeActor(dummy)
				case _ => //welp
			}
		}

		//dom.console.log("Removed " + tot + " dummies")
	}

	def spawnSpecial() =
	{
		val x = r.nextInt(GV.GAMEX)
		val y = r.nextInt(GV.GAMEY - GV.GAMEY/4)

		var randDude : Actor = null

		val charger = new Charger(new Pt(x, y))
		val spitter = new Spitter(new Pt(x, y))
		val tank = new Tank(new Pt(x, y))

		//now put them in a weighted list so to speak
		val chargerL = Seq.fill(GV.CHARGER_CHANCE){charger}
		val spitterL = Seq.fill(GV.SPITTER_CHANCE){spitter}
		val tankL = Seq.fill(GV.TANK_CHANCE){tank}

		val choices = chargerL ++ spitterL ++ tankL

		val choice = choices(r.nextInt(choices.length))

		if(! collision(choice))
		{
			addActor(choice)
		}
	}

	def spawnZombie() = 
	{
		val x = r.nextInt(GV.GAMEX - GV.NORMUNITSIZE)
		val y = r.nextInt(GV.GAMEY - GV.NORMUNITSIZE - GV.GAMEY/4)
		//Random wall
		val randDude = new Zombie(new Pt(x, y))
		if(! collision(randDude))
		{
			addActor(randDude)
		}
	}

	def spawnFriendly() =
	{
		val x = r.nextInt(GV.GAMEX - GV.NORMUNITSIZE)
		val y = r.nextInt(GV.GAMEY - GV.NORMUNITSIZE)

		var randDude = new Human(new Pt(x, y))

		if(! collision(randDude))
		{
			addActor(randDude)
		}
	}

	def genMap() =
	{
		dom.console.log("Generating map with difficulty " + difficulty)
		objs.clear()
	
		acts.clear()

		//if this isn't the first map move the player to the bottom
		if(difficulty != 0) player.changeLoc(new Pt(player.loc.x, GV.GAMEY - GV.NORMUNITSIZE))
		addActor(player)

		spawnMainWalls()

		genFarmhouse(new Pt(0, 0), new Pt(GV.GAMEX/2, GV.GAMEY/2))
		genFarmhouse(new Pt(GV.GAMEX/2, 0), new Pt(GV.GAMEX/2, GV.GAMEY/2))
		genFarmhouse(new Pt(0, GV.GAMEY/2), new Pt(GV.GAMEX/2, GV.GAMEY/2))
		genFarmhouse(new Pt(GV.GAMEX/2, GV.GAMEY/2), new Pt(GV.GAMEX/2, GV.GAMEY/2))

		// for(i <- 1 to 10)
		// {
		// 	genFarmhouse()
		// }

		//convert all dummy items to actual items
		spawnItemsFromDummies()

		//Add random zombies
		for(i <- 1 to difficulty * 3)
		{
			spawnZombie()
		}

		//add random specials
		for(i <- 1 to difficulty)
		{
			spawnSpecial()
		}

		//add allies!
		for(i <- 1 to 1)
		{
			spawnFriendly()
		}

		//Now, increase the difficulty
		difficulty += 1

		// for(a <- acts)
		// {
		// 	dom.console.log("clas: " + a.getClass)
		// }
	}
}