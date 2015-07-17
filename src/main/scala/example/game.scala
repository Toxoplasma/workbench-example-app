package game

import org.scalajs.dom

import globalvars._
import objects._
import enemies._
import allies._


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

	//animatons/effects
	val anims = scala.collection.mutable.Set[Anim]()

	//make the player
	val player = new Player(new Pt(GV.GAMEX / 2, GV.GAMEY/2))
	player.name = "Tim Jones"
	addActor(player)

	//quest junk
	//saloon
	val saloon_level = r.nextInt(GV.SALOON_LEVEL_RANGE) + GV.SALOON_LEVEL_MIN
	// val saloon_level = 1 //r.nextInt(GV.SALOON_LEVEL_RANGE) + GV.SALOON_LEVEL_MIN
	var saloon_hasPickedAlly = false

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

	def addAnim(anim : Anim) =
	{
		anims += anim
	}

	def removeAnim(anim : Anim) =
	{
		anims remove anim
	}


	def runAllAIs() =
	{
		//run all ais
		acts map(_.aiMove(this))

		//run momentum
		acts map(_.handleMomentum(this))

		//run effects
		acts map(_.runEffects(this))

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

	def loadAndSave(name : String) =
	{
		val img = loadImage("images/" + name + ".png")
		images += (name -> img)
	}

	def loadAndSaveAs(filename : String, name : String) =
	{
		val img = loadImage("images/" + filename + ".png")
		images += (name -> img)
	}

	def loadImagePair(name : String) = 
	{
		loadAndSave(name)
		loadAndSave(name + " big")
	}

	def loadAllImages() =
	{
		
		loadAndSave("item landmine")
		loadAndSave("item health kit")
		loadAndSave("item ammo box")
		loadAndSave("item molotov")
		loadAndSave("item pipe bomb")
		loadAndSave("item acid")
		loadAndSave("item zombiecontroller")
		loadImagePair("item wet towel")
		//loadImagePair("item grenade")
		//loadAndSave("item gunturret")
		loadAndSaveAs("item gunturret big", "item gunturret")
		loadAndSaveAs("item grenade big", "item grenade")

		loadImagePair("eq ak47")
		loadImagePair("eq uzi")
		loadImagePair("eq flip flops")
		loadImagePair("eq running shoes")
		loadImagePair("eq light armor")
		loadImagePair("eq heavy armor")
		loadImagePair("eq fireproof cape")
		loadImagePair("eq acid boots")

		loadAndSave("turret 0")
		loadAndSave("turret 90")
		loadAndSave("turret 180")
		loadAndSave("turret 270")
		loadAndSave("turret 45")
		loadAndSave("turret 135")
		loadAndSave("turret 225")
		loadAndSave("turret 315")

		loadAndSave("char human 1 0")
		loadAndSave("char human 1 90")
		loadAndSave("char human 1 180")
		loadAndSave("char human 1 270")
		loadAndSave("char human 1 45")
		loadAndSave("char human 1 135")
		loadAndSave("char human 1 225")
		loadAndSave("char human 1 315")
		loadAndSave("char human 1 big")

		loadAndSave("char human 2 0")
		loadAndSave("char human 2 90")
		loadAndSave("char human 2 180")
		loadAndSave("char human 2 270")
		loadAndSave("char human 2 45")
		loadAndSave("char human 2 135")
		loadAndSave("char human 2 225")
		loadAndSave("char human 2 315")

		loadAndSave("char human 2 big")

		loadAndSave("char human 3 0")
		loadAndSave("char human 3 90")
		loadAndSave("char human 3 180")
		loadAndSave("char human 3 270")
		loadAndSave("char human 3 45")
		loadAndSave("char human 3 135")
		loadAndSave("char human 3 225")
		loadAndSave("char human 3 315")

		loadAndSave("char human 3 big")

		loadAndSave("tank rider 0")
		loadAndSave("tank rider 90")
		loadAndSave("tank rider 180")
		loadAndSave("tank rider 270")
		loadAndSave("tank rider 45")
		loadAndSave("tank rider 135")
		loadAndSave("tank rider 225")
		loadAndSave("tank rider 315")

		loadAndSave("tank rider big")

		loadAndSave("char suou 0")
		loadAndSave("char suou 90")
		loadAndSave("char suou 180")
		loadAndSave("char suou 270")
		loadAndSave("char suou 45")
		loadAndSave("char suou 135")
		loadAndSave("char suou 225")
		loadAndSave("char suou 315")

		loadAndSave("char suou big")

		loadAndSave("explosion 50 1")
		loadAndSave("explosion 50 2")
		loadAndSave("explosion 50 3")
		loadAndSave("explosion 50 4")
		loadAndSave("explosion 50 5")

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
			{
				o.draw(this)
				o match {
					case a : Actor => a.drawEffects(this)
					case _ => //no effects for other people
				}
				
			}
		}
	}

	def drawAnims()
	{
		anims map(_.draw(this))
	}

	def drawEffects()
	{
		acts map(_.drawEffects(this))
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

		//draw effects
		//drawEffects()

		drawShadows()
		
		//draw all the walls overtop the shadows
		val wally = new Wall(new Pt(0, 0), new Pt(0, 0))
		drawAllCond(o => o.getClass == wally.getClass)
		
		drawHeadTexts()

		//draw animations
		drawAnims()
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
		var itemAct : Actor = null
		if(r.nextInt(GV.EQUIP_CHANCE) == 0)
		{
			//equipment
			val spitterboots = 	new GroundEquip(d.loc, new EqSpitterBoots())
			val flamecape = 	new GroundEquip(d.loc, new EqFlameCape())
			val lightarmor =	new GroundEquip(d.loc, new EqLightArmor())
			val heavyarmor =	new GroundEquip(d.loc, new EqHeavyArmor())
			val speedboots =	new GroundEquip(d.loc, new EqSpeedBoots())
			val sniper = 		new GroundEquip(d.loc, new EqSniperRifle())	
			val ak = 			new GroundEquip(d.loc, new EqAk47())	
			val smg = 			new GroundEquip(d.loc, new EqSMG())	
			val mg = 			new GroundEquip(d.loc, new EqMG())	

			val choices = List(spitterboots, speedboots,
				flamecape, 
				lightarmor, heavyarmor, 
				sniper, ak, smg, mg)

			itemAct = choices(r.nextInt(choices.length))

			dom.console.log("Adding item " + itemAct.name)
		}
		else //consumables
		{
			val ammopack = new MediumAmmoPack(d.loc)
			val healthpack = new MediumHealthPack(d.loc)
			val mine = new GroundUsableItem(d.loc, new UsableLandMine(null))
			val spit = new GroundUsableItem(d.loc, new UsableSpitterAcid(null))
			val pipe = new GroundUsableItem(d.loc, new UsablePipeBomb(null))
			val grenade = new GroundUsableItem(d.loc, new UsableGrenade(null))
			val molotov = new GroundUsableItem(d.loc, new UsableMolotov(null))
			val turret = new GroundUsableItem(d.loc, new UsableGunTurret(null))
			val controller = new GroundUsableItem(d.loc, new UsableZombieController(null))

			//now put them in a weighted list so to speak
			val ammopackL = Seq.fill(GV.AMMO_CHANCE){ammopack}
			val healthpackL = Seq.fill(GV.HEALTH_CHANCE){healthpack}
			val mineL = Seq.fill(GV.LANDMINE_CHANCE){mine}
			val spitL = Seq.fill(GV.SPITTERACID_CHANCE){spit}
			val pipeL = Seq.fill(GV.PIPEBOMB_CHANCE){pipe}
			val grenadeL = Seq.fill(GV.GRENADE_CHANCE){grenade}
			val molotovL = Seq.fill(GV.MOLOTOV_CHANCE){molotov}
			val turretL = Seq.fill(GV.TURRET_CHANCE){turret}
			val controllerL = Seq.fill(GV.CONTROLLER_CHANCE){controller}

			val choices = ammopackL ++ healthpackL ++ mineL ++ spitL ++ pipeL ++ 
				grenadeL ++ molotovL ++ turretL ++ controllerL

			itemAct = choices(r.nextInt(choices.length))			
		}
		
		addActor(itemAct)
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
		val spitter = new AcidSpitter(new Pt(x, y))
		val icespitter = new IceSpitter(new Pt(x, y))
		val tank = new Tank(new Pt(x, y))
		val cannon = new CannonTank(new Pt(x, y))

		//now put them in a weighted list so to speak
		val chargerL = Seq.fill(GV.CHARGER_CHANCE){charger}
		val spitterL = Seq.fill(GV.SPITTER_CHANCE){spitter}
		val icespitterL = Seq.fill(GV.ICESPITTER_CHANCE){icespitter}
		val tankL = Seq.fill(GV.TANK_CHANCE){tank}
		val cannonL = Seq.fill(GV.CANNON_CHANCE){cannon}

		val choices = chargerL ++ spitterL ++ tankL ++ cannonL ++ icespitterL

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

	def loadNewMap() : Boolean =
	{
		dom.console.log("Generating new map")

		//score boost!
		score += difficulty * 10

		//clear head texts
		headTexts.clear()

		//save actors because genMap deletes them
		val oldActs = acts.clone

		//Load new map
		genMap()

		dom.console.log("Copying " + oldActs.size + " actors")

		//Update the delays on all our delayed guys
		for(delAct <- delayedActs)
		{
			delAct.time += (GV.GAMEY / delAct.speed).toInt

			// if(delAct.time > GV.OFFMAPCUTOFF)
			// {
			// 	g.removeDelayed(delAct)
			// }
		}

		//Add all the actors as delays
		for(a <- oldActs)
		{
			a match
			{
				case proj : ProjectileActor => //Nothing, don't add them
				case _ =>
					if(a.important && a != player)
					{
						val timed = (a.loc.y / a.speed)
						if(! timed.isInfinity)
						{
							val time : Int = timed.toInt
							dom.console.log("Delaying " + a.name + " with time " + time)

							a.moveToNewMap(time, this)

							//Move them to valid spots
							a.loc.y = GV.GAMEY //all the way down
							//a.loc.x = max(a.loc.x, GV.GAMEX / 2 - 50) //minimum right they can be
							//a.loc.x = min(a.loc.x, GV.GAMEX/2 + 50 - a.size.x)


							val delayedAct = new DelayedActor(a, time)

							addDelayed(delayedAct)
						}
					}
			}
		}

		true
	}

	def genMap() : Boolean =
	{
		dom.console.log("Generating map with difficulty " + difficulty)
		objs.clear()
	
		acts.clear()

		//if this isn't the first map move the player to the bottom
		if(difficulty != 0) player.changeLoc(new Pt(player.loc.x, GV.GAMEY - GV.NORMUNITSIZE))
		addActor(player)

		spawnMainWalls()

		if(difficulty == saloon_level)
		{
			genSaloon()
			difficulty += 1
			return true
		}

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

		return true
	}






	def genSaloon()
	{
		val border = 100
		//some main walls
		val top = new Wall(new Pt(border, border), new Pt(GV.GAMEX - border*2, 5))
		val left = new Wall(new Pt(border, border), new Pt(5, GV.GAMEY - border*2))
		val bot = new Wall(new Pt(border, GV.GAMEY - border), new Pt(GV.GAMEX - border*2 + 5, 5))
		val right = new Wall(new Pt(GV.GAMEX - border, border), new Pt(5, GV.GAMEY - border*2))

		val doorsize = 40
		val (top1, top2) = top.splitWithDoorAt(GV.GAMEX / 2 - 20 - border, 40)
		val (bot1, bot2) = bot.splitWithDoorAt(GV.GAMEX / 2 - 20 - border, 40)

		addObj(top1)
		addObj(top2)
		addObj(bot1)
		addObj(bot2)
		addObj(left)
		addObj(right)

		//add the bar
		val barRight = new Barrier(new Pt(border+50, border+5), new Pt(7, 300))
		val barBot = new Barrier(new Pt(border+5, border+5+300), new Pt(50, 5))
		addObj(barRight)
		addObj(barBot)

		val tender = new Bartender(new Pt(border+40, border +100))
		addActor(tender)

		//add the booths
		val boothwidth = 50
		val boothspace = 12
		val tableheight = 30

		val totalHeight = boothspace*2 + tableheight + 5

		val seats = scala.collection.mutable.Buffer[(Int, String)]()

		var curY = border + 5
		var x = GV.GAMEX - border - boothwidth
		for(i <- 1 to (GV.GAMEY - border*2)/totalHeight)
		{
			//make a booth
			//top wall inherited from last table
			//space for seat
			seats += Tuple2(curY, "0")
			curY += boothspace
			//table
			val table = new Barrier(new Pt(x, curY), new Pt(boothwidth, tableheight))
			addObj(table)
			curY += tableheight

			//space for seat
			seats += Tuple2(curY, "180")
			curY += boothspace

			//wall
			val wall = new Wall(new Pt(x, curY), new Pt(boothwidth, 5))
			addObj(wall)
			curY += 5
		}

		//add the dudes
		val jane = (new MolotovThrower(new Pt(player.loc.x + 50, player.loc.y)),
					GV.JANE_BAR_PHRASES, "char human 2")
		val tanky = (new TankRider(new Pt(player.loc.x + 50, player.loc.y)),
					GV.TANK_RIDER_BAR_PHRASES, "tank rider")
		dom.console.log("about to spawn sniper")
		val sniper = (new Sniper(new Pt(player.loc.x + 50, player.loc.y)),
					GV.SNIPER_BAR_PHRASES, "char suou")
		dom.console.log("spawned nsniper")
		val medic = (new Medic(new Pt(player.loc.x + 50, player.loc.y)),
					GV.MEDIC_BAR_PHRASES, "char human 3")
		val sapper = (new Sapper(new Pt(player.loc.x + 50, player.loc.y)),
					GV.SAPPER_BAR_PHRASES, "char human 3")
		
		//g.addActor(hooman)
		val seat1 = seats(r.nextInt(seats.length))
		seats -= seat1
		val seat2 = seats(r.nextInt(seats.length))
		seats -= seat2
		val seat3 = seats(r.nextInt(seats.length))
		seats -= seat3

		val chosenSeats = List(seat1, seat2, seat3)
		// val seat4 = seats(r.nextInt(seats.length))
		// seats -= seat4
		// val seat5 = seats(r.nextInt(seats.length))
		// seats -= seat5
		// val dude1 = new BarDude(new Pt(x + 10, seat1._1), GV.JANE_BAR_PHRASES, "char_human_2", seat1._2, jane)
		// val dude2 = new BarDude(new Pt(x + 10, seat2._1), GV.TANK_RIDER_BAR_PHRASES, "tank rider", seat2._2, tanky)
		// val dude3 = new BarDude(new Pt(x + 10, seat3._1), GV.SNIPER_BAR_PHRASES, "char suou", seat3._2, sniper)
		// val dude4 = new BarDude(new Pt(x + 10, seat4._1), GV.SNIPER_BAR_PHRASES, "char_human_3", seat4._2, medic)
		// val dude5 = new BarDude(new Pt(x + 10, seat5._1), GV.SAPPER_BAR_PHRASES, "char_human_3", seat5._2, sapper)

		val dudes = scala.collection.mutable.Set(jane, tanky, sniper, medic, sapper)
		for(i <- 1 to GV.SALOON_NPCNUM)
		{
			val seat = seats(i)
			val dude = dudes.toList(r.nextInt(dudes.size))
			val bardude = new BarDude(new Pt(x + 10, seat._1), dude._2, dude._3, seat._2, dude._1)
			addActor(bardude)

			//remove him from the last so we don't double spawn
			dudes remove dude
		}
		//val dude2 = new BarDude(new Pt(x + 10, seat2._1), GV.TANK_RIDER_BAR_PHRASES, "tank rider", seat2._2, tanky)
		//val dude3 = new BarDude(new Pt(x + 10, seat3._1), GV.SNIPER_BAR_PHRASES, "char suou", seat3._2, sniper)
		

		// addActor(dude1)
		// addActor(dude2)
		// addActor(dude3)
		// addActor(dude4)
		// addActor(dude5)

		//add some health and junk
		val hp1 = new MediumHealthPack(new Pt(GV.GAMEX/2 - 60 - 15, border + 10))
		val hp2 = new MediumHealthPack(new Pt(GV.GAMEX/2 - 45 - 15, border + 10))
		val ammo1 = new MediumAmmoPack(new Pt(GV.GAMEX/2 - 30 - 15, border + 10))
		val ammo2 = new MediumAmmoPack(new Pt(GV.GAMEX/2 - 15 - 15, border + 10))

		addActor(hp1)
		addActor(hp2)
		addActor(ammo1)
		addActor(ammo2)
	}
}