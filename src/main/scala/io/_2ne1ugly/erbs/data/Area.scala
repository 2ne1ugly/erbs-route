package io._2ne1ugly.erbs.data

sealed trait Area {
  val hasHyperloop: Boolean
  val items: Map[Item, Int]
  val connections: Set[Area]
}

object Area {
  import Item._

  case object Dock extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      ShortRod           -> 5,
      LongRifle          -> 3,
      KitchenKnife       -> 4,
      RustySword         -> 4,
      Wetsuit            -> 7,
      Bandage            -> 7,
      Box                -> 7,
      SurveillanceCamera -> 6,
      Snare              -> 6,
      GlassBottle        -> 7,
      TurtleShell        -> 7,
      Rubber             -> 8,
      ScrapMetal         -> 8,
      Lighter            -> 8,
      Battery            -> 7,
      Bread              -> 4,
      Ramen              -> 6,
      Coffee             -> 6
    )
    val connections: Set[Area] = Set(
      Chapel,
      Uptown,
      Factory
    )
  }

  case object Pond extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      Bamboo       -> 7,
      ShortRod     -> 5,
      Hammer       -> 7,
      Pickaxe      -> 7,
      Hatchet      -> 4,
      ShortSpear   -> 5,
      Hat          -> 7,
      Bracelet     -> 7,
      Flower       -> 7,
      Ribbon       -> 6,
      Box          -> 7,
      Snare        -> 6,
      Mousetrap    -> 7,
      TurtleShell  -> 7,
      Gemstone     -> 8,
      OrientalHerb -> 7
    )
    val connections: Set[Area] = Set(
      Avenue,
      Temple,
      Hospital
    )
  }

  case object Beach extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      WaltherPPK         -> 4,
      Hammer             -> 6,
      Pickaxe            -> 7,
      Hatchet            -> 5,
      SteelChain         -> 6,
      BikeHelmet         -> 6,
      Wetsuit            -> 6,
      Binoculars         -> 6,
      SurveillanceCamera -> 5,
      Snare              -> 5,
      PianoWire          -> 6,
      TurtleShell        -> 6,
      StallionMedal      -> 6,
      Gemstone           -> 7,
      Can                -> 4,
      CarbonatedWater    -> 5
    )
    val connections: Set[Area] = Set(
      Hotel,
      Forest,
      Uptown
    )
  }

  case object Uptown extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      FountainPen        -> 6,
      Windbreaker        -> 6,
      Watch              -> 6,
      Bracelet           -> 6,
      RunningShoes       -> 6,
      Flower             -> 7,
      Ribbon             -> 7,
      SurveillanceCamera -> 6,
      PianoWire          -> 7,
      LaserPointer       -> 6,
      Oil                -> 7,
      Lemon              -> 5,
      Chocolate          -> 5,
      CurryPowder        -> 5,
      Whiskey            -> 5,
      Coffee             -> 5,
      CarbonatedWater    -> 5
    )
    val connections: Set[Area] = Set(
      Beach,
      Forest,
      Chapel,
      Dock
    )
  }

  case object Alley extends Area {
    val hasHyperloop: Boolean  = true
    val items: Map[Item, Int]  = Map(
      Hammer        -> 8,
      Scissors      -> 7,
      SteelChain    -> 6,
      Needle        -> 7,
      Wetsuit       -> 7,
      Bracelet      -> 6,
      RunningShoes  -> 7,
      Cross         -> 7,
      Binoculars    -> 7,
      Rubber        -> 7,
      Lighter       -> 8,
      StallionMedal -> 7,
      Glue          -> 7,
      Garlic        -> 6,
      Ramen         -> 6,
      Honey         -> 5
    )
    val connections: Set[Area] = Set(
      ArcheryRange,
      School,
      Avenue,
      Temple
    )
  }

  case object Hotel extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      CottonGloves    -> 4,
      PlayingCards    -> 6,
      WaltherPPK      -> 5,
      Fedorova        -> 4,
      KitchenKnife    -> 4,
      Needle          -> 6,
      Windbreaker     -> 7,
      Watch           -> 6,
      Binoculars      -> 6,
      PianoWire       -> 7,
      ScrapMetal      -> 8,
      Cloth           -> 7,
      IronOre         -> 7,
      Lemon           -> 6,
      Ice             -> 7,
      Whiskey         -> 5,
      CarbonatedWater -> 5
    )
    val connections: Set[Area] = Set(
      ArcheryRange,
      School,
      Forest,
      Beach
    )
  }

  case object Avenue extends Area {
    val hasHyperloop: Boolean  = true
    val items: Map[Item, Int]  = Map(
      PlayingCards       -> 8,
      FountainPen        -> 8,
      Hairband           -> 8,
      Watch              -> 8,
      Slippers           -> 8,
      Tights             -> 8,
      Fan                -> 8,
      SurveillanceCamera -> 6,
      GlassBottle        -> 8,
      Nail               -> 8,
      Battery            -> 8,
      Oil                -> 8,
      Cloth              -> 8,
      Can                -> 6,
      Chocolate          -> 6,
      Honey              -> 6,
      Milk               -> 6
    )
    val connections: Set[Area] = Set(
      Alley,
      School,
      Temple,
      Pond,
      Forest
    )
  }

  case object Hospital extends Area {
    val hasHyperloop: Boolean  = true
    val items: Map[Item, Int]  = Map(
      CottonGloves       -> 4,
      Razor              -> 6,
      Scissors           -> 6,
      Needle             -> 6,
      Bandage            -> 6,
      Slippers           -> 6,
      Tights             -> 6,
      Feather            -> 6,
      SurveillanceCamera -> 6,
      ScrapMetal         -> 6,
      LaserPointer       -> 6,
      Alcohol            -> 4,
      Glue               -> 6,
      Lemon              -> 4,
      AdhesiveBandage    -> 4,
      Ice                -> 6,
      Milk               -> 5
    )
    val connections: Set[Area] = Set(
      Temple,
      Pond,
      Cemetery,
      Factory
    )
  }
  case object Temple   extends Area {
    val hasHyperloop: Boolean  = true
    val items: Map[Item, Int]  = Map(
      Bamboo            -> 7,
      ShortRod          -> 5,
      KitchenKnife      -> 5,
      RustySword        -> 4,
      ShortSpear        -> 4,
      Hairband          -> 7,
      MonksRobe         -> 9,
      FabricArmor       -> 8,
      BuddhistScripture -> 8,
      StallionMedal     -> 7,
      Cloth             -> 7,
      Gemstone          -> 8,
      Paper             -> 7,
      Gunpowder         -> 7,
      Garlic            -> 6,
      OrientalHerb      -> 5
    )
    val connections: Set[Area] = Set(
      Alley,
      Avenue,
      Pond
    )
  }

  case object ArcheryRange extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      Bamboo             -> 6,
      IronBall           -> 5,
      Bow                -> 4,
      Hat                -> 5,
      MonksRobe          -> 5,
      FabricArmor        -> 6,
      RunningShoes       -> 5,
      SurveillanceCamera -> 5,
      Snare              -> 5,
      Nail               -> 6,
      Rubber             -> 5,
      Oil                -> 5,
      Paper              -> 5,
      Gunpowder          -> 6,
      Egg                -> 5,
      Ramen              -> 5,
      Chocolate          -> 4
    )
    val connections: Set[Area] = Set(
      Alley,
      School,
      Hotel
    )
  }

  case object Cemetery extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      BrassKnuckles      -> 5,
      Bamboo             -> 7,
      Pickaxe            -> 7,
      SteelChain         -> 7,
      Hairband           -> 7,
      FabricArmor        -> 7,
      Feather            -> 7,
      Flower             -> 7,
      SurveillanceCamera -> 6,
      Mousetrap          -> 6,
      IronOre            -> 7,
      Gunpowder          -> 7,
      Garlic             -> 5,
      Egg                -> 5,
      Ice                -> 5,
      Coffee             -> 5
    )
    val connections: Set[Area] = Set(
      Hospital,
      Factory,
      Chapel,
      Forest
    )
  }

  case object Forest extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      BrassKnuckles -> 5,
      Bamboo        -> 7,
      IronBall      -> 7,
      ShortCrossbow -> 4,
      LongRifle     -> 4,
      Pickaxe       -> 8,
      ShortSpear    -> 4,
      Tights        -> 7,
      Feather       -> 7,
      Flower        -> 7,
      Fan           -> 8,
      Snare         -> 5,
      Gemstone      -> 8,
      IronOre       -> 7,
      Egg           -> 6,
      OrientalHerb  -> 6,
      Honey         -> 6
    )
    val connections: Set[Area] = Set(
      Hotel,
      School,
      Avenue,
      Chapel,
      Uptown,
      Beach
    )
  }

  case object Factory extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      IronBall      -> 7,
      Chalk         -> 7,
      ShortCrossbow -> 4,
      WaltherPPK    -> 4,
      Fedorova      -> 4,
      Hatchet       -> 4,
      Bandage       -> 6,
      Binoculars    -> 7,
      Nail          -> 7,
      ScrapMetal    -> 9,
      Lighter       -> 8,
      Battery       -> 9,
      Alcohol       -> 6,
      Oil           -> 7,
      Glue          -> 7,
      CurryPowder   -> 5
    )
    val connections: Set[Area] = Set(
      Dock,
      Chapel,
      Cemetery,
      Hospital
    )
  }

  case object Chapel extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      Whip        -> 4,
      Razor       -> 6,
      Chalk       -> 6,
      Bow         -> 4,
      RustySword  -> 4,
      BikeHelmet  -> 6,
      Fan         -> 6,
      Box         -> 6,
      HolyGrail   -> 9,
      Cross       -> 9,
      Snare       -> 5,
      PianoWire   -> 6,
      GlassBottle -> 6,
      Paper       -> 6,
      Bread       -> 4,
      Whiskey     -> 4,
      Milk        -> 3
    )
    val connections: Set[Area] = Set(
      Forest,
      Cemetery,
      Factory,
      Dock,
      Uptown
    )
  }

  case object School extends Area {
    val hasHyperloop: Boolean  = false
    val items: Map[Item, Int]  = Map(
      Whip               -> 4,
      Razor              -> 6,
      Chalk              -> 6,
      Scissors           -> 6,
      FountainPen        -> 7,
      Hat                -> 6,
      BikeHelmet         -> 6,
      Windbreaker        -> 7,
      Bandage            -> 6,
      Slippers           -> 7,
      Ribbon             -> 7,
      SurveillanceCamera -> 6,
      Lighter            -> 7,
      LaserPointer       -> 6,
      Alcohol            -> 5,
      Can                -> 5,
      Bread              -> 6
    )
    val connections: Set[Area] = Set(
      ArcheryRange,
      Alley,
      Avenue,
      Forest,
      Hotel
    )
  }
}
