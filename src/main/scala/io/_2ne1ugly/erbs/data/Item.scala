package io._2ne1ugly.erbs.data

import cats.syntax.all._

sealed trait ItemRarity

object ItemRarity {
  final case object Common    extends ItemRarity
  final case object Uncommon  extends ItemRarity
  final case object Rare      extends ItemRarity
  final case object Epic      extends ItemRarity
  final case object Legendary extends ItemRarity
}

sealed trait Item {
  import Item._

  val bundleCount: Int
  val maxStack: Int
  val recipe: Option[(Item, Item)]

  final lazy val baseRecipe: Map[Item, Int] = recipe match {
    case Some((item1, item2)) => item1.baseRecipe ++ item2.baseRecipe
    case None                 => Map(this -> 1)
  }

  final lazy val isGreedy: Boolean = this match {
    case Meteorite | TreeOfLife | VFBloodSample => true
    case _                                      =>
      this.recipe match {
        case Some((item1, item2)) => item1.isGreedy || item2.isGreedy
        case None                 => false
      }
  }
}

sealed trait WeaponType

object WeaponType {
  final case object Dagger         extends WeaponType
  final case object TwoHandedSword extends WeaponType
  final case object Axe            extends WeaponType
  final case object DualSwords     extends WeaponType
  final case object Pistol         extends WeaponType
  final case object AssaultRifle   extends WeaponType
  final case object SniperRifle    extends WeaponType
  final case object Rapier         extends WeaponType
  final case object Spear          extends WeaponType
  final case object Hammer         extends WeaponType
  final case object Bat            extends WeaponType
  final case object Throw          extends WeaponType
  final case object Shuriken       extends WeaponType
  final case object Bow            extends WeaponType
  final case object Crossbow       extends WeaponType
  final case object Glove          extends WeaponType
  final case object Tonfa          extends WeaponType
  final case object Guitar         extends WeaponType
  final case object Nunchaku       extends WeaponType
  final case object Whip           extends WeaponType
}

sealed trait Weapon { this: Item =>
  final val maxStack: Int    = 1
  final val bundleCount: Int = 1
  val weaponType: WeaponType
}

sealed trait ArmorType

object ArmorType {
  final case object Chest     extends ArmorType
  final case object Head      extends ArmorType
  final case object Arm       extends ArmorType
  final case object Leg       extends ArmorType
  final case object Accessory extends ArmorType
}

sealed trait Armor { this: Item =>
  final val maxStack: Int    = 1
  final val bundleCount: Int = 1
  val armorType: ArmorType
}

sealed trait ConsumableType

object ConsumableType {
  final case object Food     extends ConsumableType
  final case object Beverage extends ConsumableType
}

sealed trait Consumable { this: Item =>
  import Item._

  final val maxStack: Int         = 5
  final lazy val bundleCount: Int = this match {
    case Lemon | Garlic | AdhesiveBandage | Carp | Bread | Meat | Egg | Ramen | OrientalHerb | Chocolate | CurryPowder | HoneyCodSteak |
        CannedCodLiver | Butter | HerbalMedicine | HolyWater | Disinfectant | ChocoPie | AcupunctureNeedle | TandooriChicken | BaconAndGarlicSticks |
        Bun | Hamburger | PotatoSoup | FishFilletWithEgg | EasterEgg | FrenchFries | FirstAidKit | FishCutlet | Baijiu =>
      1

    case Potato | Cod | GarlicBread | CarpBread | Orchid | PotatoBread | CitrusCake | EggBun | ChocoIceCream | CurryBun | BakedPotato | BakedCarp |
        HotRamen | MochaBread | ScrambledEggs | ChocolateChipCookies | ChocoPieBox | OrientalConcoction | HoneyButter | FriedChicken | HealingPotion |
        BoiledEgg | PoundCake | CurryCroquette | ButterFriedPotatoes | StirFriedRamen | ColdNoodles | ZenVitality | GarlicRamen | SpicyFishStew |
        FishAndChips | Honey | Water | Ice | Whiskey | Coffee | CarbonatedWater | Milk | WaterBottle | Soju | IceCoffee | CoffeeLiqueur | Latte |
        HoneyMilk | HoneyWater | IceWater | OnTheRocks | Cowboy | HotHoneyWater | Americano | PurifiedWater | CanOfCola =>
      2

    case LemonIceCream | HoneyGarlicPickle | WhiskeyBonbon | GrilledChileanSeaBass | Curry | BoilingWater | Cola | Highball | KaoliangLiquor =>
      3

    case Steak | Lemonade | Cocktail | ChocolateMilk | HotChocolate | WhiteRussian =>
      4

    case FlowerLiquor | HerbalLiquor | WhiskeyCocktail =>
      5
  }
  val consumableType: ConsumableType
}

sealed trait Special { this: Item =>
  import Item._

  final val maxStack: Int         = 5
  final lazy val bundleCount: Int = this match {

    case PianoWire | Dynamite | JungleGuillotine | ExplosiveTrap | PendulumAxe | RDX | Stingburst | SurveillanceCamera =>
      1

    case Snare | Mousetrap | SpikedPlank | EnhancedMousetrap | BambooTrap | BoobyTrap | Mine | HiddenMaiden | DoubleGuillotine | Claymore =>
      2

    case MithrilString | FireTrap | C4 | RemoteMine | SmartBomb | TelephotoCamera =>
      3

    case ClangClatter =>
      5
  }
}
sealed trait Material { this: Item =>
  import Item._

  final val maxStack: Int         = 5
  final lazy val bundleCount: Int = this match {
    case Nail | Leather | TurtleShell | Rubber | ScrapMetal | Lighter | LaserPointer | StallionMedal | Battery | Alcohol | Oil | Cloth | Gemstone |
        Glue | Paper | Can | Gunpowder | Ruby | WhitePowder | Meteorite | Ash | ElectronicParts | Blueprint | Gold | TreeOfLife | Moonstone | Poison |
        Motor | Mithril | GlassPanel | VFBloodSample | CellPhone | ForceCore =>
      1

    case Stone | GlassBottle | Steel | Oilcloth | DeadBattery | IronSheet | IonBattery =>
      2

    case IronOre | HeatedOil | HeatedStone =>
      3
  }
}

object Item {
  final case object Scissors extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object FountainPen extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object KitchenKnife extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object ArmyKnife extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (KitchenKnife, Branch).some
  }

  final case object RoseKnife extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ArmyKnife, Flower).some
  }

  final case object Carnwennan extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RoseKnife, SaintsRelic).some
  }

  final case object MountSlicer extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RoseKnife, Ash).some
  }

  final case object Vibroblade extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ArmyKnife, Motor).some
  }

  final case object Fragarach extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ArmyKnife, ForceCore).some
  }

  final case object RustySword extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Shamshir extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (RustySword, Lighter).some
  }

  final case object Katana extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RustySword, IronSheet).some
  }

  final case object Masamune extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Katana, Oil).some
  }

  final case object Muramasa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Katana, Gemstone).some
  }

  final case object BastardSword extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RustySword, Steel).some
  }

  final case object JewelSword extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Shamshir, Ruby).some
  }

  final case object ThuanThien extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BastardSword, TurtleShell).some
  }

  final case object PlasmaSword extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BastardSword, LaserPointer).some
  }

  final case object Excalibur extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (JewelSword, HolyGrail).some
  }

  final case object Arondight extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Masamune, Cross).some
  }

  final case object Hovud extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (JewelSword, GlassPieces).some
  }

  final case object Monohoshizao extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Muramasa, Blueprint).some
  }

  final case object Laevateinn extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (TrueSamadhiFire, Shamshir).some
  }

  final case object Dainsleif extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (VFBloodSample, Katana).some
  }

  final case object Pickaxe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Hatchet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object ChainScythe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Pickaxe, SteelChain).some
  }

  final case object BattleAxe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hatchet, Bamboo).some
  }

  final case object LightHatchet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BattleAxe, Feather).some
  }

  final case object ReapersScythe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ChainScythe, ShortRod).some
  }

  final case object GiganticAxe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BattleAxe, Steel).some
  }

  final case object BeamAxe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (GiganticAxe, LaserPointer).some
  }

  final case object SantaMuerte extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ReapersScythe, Gold).some
  }

  final case object Scythe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ReapersScythe, Scythe).some
  }

  final case object Parashu extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (LightHatchet, BuddhaSarira).some
  }

  final case object Harpe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ReapersScythe, WhiteCraneFan).some
  }

  final case object TwinSwords extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (KitchenKnife, RustySword).some
  }

  final case object Florentine extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (TwinSwords, Blueprint).some
  }

  final case object DivineDualSwords extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Florentine, Nail).some
  }

  final case object StarsteelTwinSwords extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (TwinSwords, Moonstone).some
  }

  final case object Dioscuri extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (TwinSwords, IonBattery).some
  }

  final case object LloigorAndZahr extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (TwinSwords, Poison).some
  }

  final case object WaltherPPK extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object MagnumPython extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (WaltherPPK, Oil).some
  }

  final case object BerettaM92F extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (WaltherPPK, Leather).some
  }

  final case object FN57 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BerettaM92F, LaserPointer).some
  }

  final case object DoubleRevolverSP extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (MagnumPython, BerettaM92F).some
  }

  final case object MagnumAnaconda extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (MagnumPython, Blueprint).some
  }

  final case object DevilsMarksman extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (DoubleRevolverSP, Ash).some
  }

  final case object Elegance extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (FN57, FeatherDuster).some
  }

  final case object ElectronBlaster extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BerettaM92F, IonBattery).some
  }

  final case object MagnumBoa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (MagnumAnaconda, Steel).some
  }

  final case object Kelte extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, BerettaM92F).some
  }

  final case object Fedorova extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object STG44 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Fedorova, Gunpowder).some
  }

  final case object AK47 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (STG44, PianoWire).some
  }

  final case object M16A1 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (STG44, Leather).some
  }

  final case object MachineGun extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (STG44, Motor).some
  }

  final case object GatlingGun extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (MachineGun, Oil).some
  }

  final case object AK12 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (AK47, GlassPanel).some
  }

  final case object XCR extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (M16A1, Magazine).some
  }

  final case object LongRifle extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Springfield extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (LongRifle, LaserPointer).some
  }

  final case object HarpoonGun extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Springfield, ShortSpear).some
  }

  final case object GoldenRifle extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Springfield, Gold).some
  }

  final case object Railgun extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Springfield, ElectronicParts).some
  }

  final case object Tac50 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HarpoonGun, Blueprint).some
  }

  final case object Intervention extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HarpoonGun, TelephotoCamera).some
  }

  final case object NTW20 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (GoldenRifle, IronSheet).some
  }

  final case object Polaris extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Railgun, WhitePowder).some
  }

  final case object TheDeadlyRay extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (Moonstone, GoldenRifle).some
  }

  final case object Needle extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object FencingRapier extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Needle, IronOre).some
  }

  final case object ApricotSword extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (FencingRapier, FlowerOfFate).some
  }

  final case object SwordOfJustice extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (FencingRapier, IronOre).some
  }

  final case object DurendalMk2 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ApricotSword, LaserPointer).some
  }

  final case object Volticletto extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ApricotSword, ElectronicParts).some
  }

  final case object MeteorClaymore extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ApricotSword, Meteorite).some
  }

  final case object Joyeuse extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (FencingRapier, Mithril).some
  }

  final case object Mistilteinn extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (SwordOfJustice, Branch).some
  }

  final case object ShortSpear extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object BambooSpear extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ShortSpear, Bamboo).some
  }

  final case object Bident extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BambooSpear, KitchenKnife).some
  }

  final case object Pike extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ShortSpear, Steel).some
  }

  final case object HalberdAxe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Hatchet, Pike).some
  }

  final case object SharpenedSpear extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BambooSpear, Feather).some
  }

  final case object GentianSilverGun extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SharpenedSpear, WhitePowder).some
  }

  final case object EighteenFootSpear extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SharpenedSpear, IonBattery).some
  }

  final case object CosmicBident extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Bident, Moonstone).some
  }

  final case object LanceOfPoseidon extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Bident, Pike).some
  }

  final case object FangtianHuaji extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HalberdAxe, GildedQuillFan).some
  }

  final case object DragonGuandao extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HalberdAxe, IronSheet).some
  }

  final case object BlazingLance extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (TrueSamadhiFire, BambooSpear).some
  }

  final case object SpearOfLonginus extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (VFBloodSample, Pike).some
  }

  final case object Hammer extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Warhammer extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hammer, ShortRod).some
  }

  final case object MorningStar extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Warhammer, IronBall).some
  }

  final case object BlackStagHammer extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Warhammer, Leather).some
  }

  final case object FangMace extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (MorningStar, HeatedStone).some
  }

  final case object HammerOfDagda extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BlackStagHammer, SaintsRelic).some
  }

  final case object HammerOfThor extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BlackStagHammer, IonBattery).some
  }

  final case object EveningStar extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Moonstone, MorningStar).some
  }

  final case object MagicStick extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Warhammer, MoonlightPendant).some
  }

  final case object Branch extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object ShortRod extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object LongRod extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ShortRod, Bamboo).some
  }

  final case object GoblinBat extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LongRod, Nail).some
  }

  final case object Torch extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (FeatherDuster, Oilcloth).some
  }

  final case object Umbrella extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LongRod, Fan).some
  }

  final case object StatueOfSoteria extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Torch, Doll).some
  }

  final case object SpyUmbrella extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Umbrella, Poison).some
  }

  final case object Mallet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (GoblinBat, Motor).some
  }

  final case object MonkeyKingBar extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, LongRod).some
  }

  final case object IronBall extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Baseball extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Grenade extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (IronBall, Gunpowder).some
  }

  final case object MolotovCocktail extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (GlassBottle, Oil).some
  }

  final case object SignedBall extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Baseball, FountainPen).some
  }

  final case object Sling extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SignedBall, Rubber).some
  }

  final case object FlourBomb extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (WhitePowder, MolotovCocktail).some
  }

  final case object BallLightning extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (IronBall, DeadBattery).some
  }

  final case object Flubber extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Rubber, BoilingWater).some
  }

  final case object SpikyBouncyBall extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Flubber, Nail).some
  }

  final case object IncendiaryBomb extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (MolotovCocktail, BallLightning).some
  }

  final case object GrenadeOfAntioch extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HighExplosiveGrenade, Cross).some
  }

  final case object DavidsSling extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Sling, SaintsRelic).some
  }

  final case object SmokeBomb extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (FlourBomb, Cola).some
  }

  final case object HighExplosiveGrenade extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Grenade, RDX).some
  }

  final case object RutheniumMarble extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SpikyBouncyBall, Gold).some
  }

  final case object Razor extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object PlayingCards extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Chalk extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Dart extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Needle, Feather).some
  }

  final case object VintageCards extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (PlayingCards, FountainPen).some
  }

  final case object OnyxDagger extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Razor, Cross).some
  }

  final case object ThrowingStars extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Razor, PianoWire).some
  }

  final case object Charm extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (VintageCards, BuddhistScripture).some
  }

  final case object WillowLeafSpike extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (OnyxDagger, Branch).some
  }

  final case object Chakram extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ThrowingStars, StallionMedal).some
  }

  final case object ApricotFlowerTag extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (WillowLeafSpike, Flower).some
  }

  final case object VenomDart extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Needle, Poison).some
  }

  final case object DharmaChakram extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Chakram, BuddhistScripture).some
  }

  final case object Plumbata extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Dart, Steel).some
  }

  final case object CardsOfTyranny extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (VintageCards, IonBattery).some
  }

  final case object MysticJadeCharm extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Charm, Ash).some
  }

  final case object FuhmaShuriken extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ApricotFlowerTag, Alcohol).some
  }

  final case object WindAndFireWheels extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (DharmaChakram, Bamboo).some
  }

  final case object AzureDagger extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (OnyxDagger, Poison).some
  }

  final case object Flechette extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Plumbata, WhitePowder).some
  }

  final case object FrostVenomDart extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (VenomDart, Ice).some
  }

  final case object DeathRune extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Charm, TreeOfLife).some
  }

  final case object Sudarsana extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, ThrowingStars).some
  }

  final case object PetalTorrent extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (FrostVenomDart, Stingburst).some
  }

  final case object Bow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object WoodenBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Branch, PianoWire).some
  }

  final case object Longbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Bow, Rubber).some
  }

  final case object CompositeBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Longbow, Nail).some
  }

  final case object StrongBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (WoodenBow, Oil).some
  }

  final case object StallionBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (MightyBow, StallionMedal).some
  }

  final case object MightyBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Longbow, Gunpowder).some
  }

  final case object PelletBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (WoodenBow, HeatedStone).some
  }

  final case object Scorchbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Longbow, Lighter).some
  }

  final case object AncientBolt extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (StallionBow, Bamboo).some
  }

  final case object GoldenRatioBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PelletBow, Gold).some
  }

  final case object Twinbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (StrongBow, CompositeBow).some
  }

  final case object ElementalBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Scorchbow, WhiteCraneFan).some
  }

  final case object Failnaught extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (VFBloodSample, StrongBow).some
  }

  final case object ShortCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object LongCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ShortCrossbow, PianoWire).some
  }

  final case object Crossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ShortCrossbow, Bamboo).some
  }

  final case object PowerCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LongCrossbow, Rubber).some
  }

  final case object LaserCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Crossbow, LaserPointer).some
  }

  final case object HeavyCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LongCrossbow, Steel).some
  }

  final case object SteelBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Crossbow, IronSheet).some
  }

  final case object TheLegendOfTheGeneral extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SteelBow, Oilcloth).some
  }

  final case object Ballista extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HeavyCrossbow, ShortSpear).some
  }

  final case object SniperCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (LaserCrossbow, SniperScope).some
  }

  final case object TheGoldenGhost extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PowerCrossbow, RDX).some
  }

  final case object Sharanga extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ForceCore, Crossbow).some
  }

  final case object BrassKnuckles extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object CottonGloves extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object LeatherGloves extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (CottonGloves, Leather).some
  }

  final case object IronKnuckles extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (BrassKnuckles, IronOre).some
  }

  final case object Gauntlet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (CottonGloves, Steel).some
  }

  final case object WingKnuckles extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (IronKnuckles, Feather).some
  }

  final case object BoneGauntlet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Gauntlet, TurtleShell).some
  }

  final case object ShatterShellGauntlet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Gauntlet, Gunpowder).some
  }

  final case object GlassKnuckles extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (IronKnuckles, GlassPieces).some
  }

  final case object PhoenixGloves extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LeatherGloves, Ash).some
  }

  final case object OneInchPunch extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BoneGauntlet, Doll).some
  }

  final case object DivineFist extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ShatterShellGauntlet, Cross).some
  }

  final case object BloodwingKnuckles extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (WingKnuckles, Ruby).some
  }

  final case object FrostPetalHand extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PhoenixGloves, Ice).some
  }

  final case object BuddhasPalm extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PhoenixGloves, BuddhaSarira).some
  }

  final case object BrasilGauntlet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BoneGauntlet, Oilcloth).some
  }

  final case object WhiteClawPunch extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (GlassKnuckles, WhitePowder).some
  }

  final case object ImperialSilkGloves extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mithril, LeatherGloves).some
  }

  final case object Bamboo extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object WoodenTonfa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Bamboo, Branch).some
  }

  final case object PoliceBaton extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (PoliceBaton, StallionMedal).some
  }

  final case object RyukyuTonfa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (WoodenTonfa, WhitePowder).some
  }

  final case object TacticalTonfa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PoliceBaton, Blueprint).some
  }

  final case object MaiSok extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RyukyuTonfa, ShortRod).some
  }

  final case object PlasmaTonfa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RyukyuTonfa, LaserPointer).some
  }

  final case object StarterGuitar extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object GoldenBridge extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (StarterGuitar, Gold).some
  }

  final case object SingleCoilPickup extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (StarterGuitar, DeadBattery).some
  }

  final case object RubySpecial extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SingleCoilPickup, Ruby).some
  }

  final case object HumbuckerPickup extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (GoldenBridge, SniperScope).some
  }

  final case object KingV extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (GoldenBridge, Scissors).some
  }

  final case object Nocaster extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SingleCoilPickup, Blueprint).some
  }

  final case object Superstrat extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SingleCoilPickup, Bamboo).some
  }

  final case object WildHorse extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SingleCoilPickup, Oil).some
  }

  final case object Bohemian extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RubySpecial, PlayingCards).some
  }

  final case object StairwayToHeaven extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HumbuckerPickup, HolyGrail).some
  }

  final case object PurpleHaze extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (KingV, Ash).some
  }

  final case object Satisfaction extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Nocaster, Stone).some
  }

  final case object WonderfulTonight extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Superstrat, Meteorite).some
  }

  final case object TheWall extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Superstrat, WhitePowder).some
  }

  final case object TeenSpirit extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (WildHorse, BuddhaSarira).some
  }

  final case object SteelChain extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Nunchaku extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (SteelChain, ScrapMetal).some
  }

  final case object Sharper extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Nunchaku, Nail).some
  }

  final case object Bleeder extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Nunchaku, Razor).some
  }

  final case object TheSmitingDragon extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Sharper, Ash).some
  }

  final case object VibroNunchaku extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Bleeder, Motor).some
  }

  final case object Whip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object RopeCuffs extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Whip, StallionMedal).some
  }

  final case object Bullwhip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Whip, Razor).some
  }

  final case object WindWhip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RopeCuffs, Fan).some
  }

  final case object ThunderWhip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Bullwhip, Gold).some
  }

  final case object LightningWhip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Bullwhip, DeadBattery).some
  }

  final case object Gleipnir extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (WindWhip, HoneyCodSteak).some
  }

  final case object PlasmaWhip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (LightningWhip, LaserPointer).some
  }

  final case object WhipOfNineBloodyTails extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (VFBloodSample, WindWhip).some
  }

  final case object Hairband extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Hat extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object BikeHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Mask extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hairband, Feather).some
  }

  final case object Circlet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hairband, Branch).some
  }

  final case object Beret extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hat, Scissors).some
  }

  final case object ChainCoif extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hat, SteelChain).some
  }

  final case object SafetyHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (BikeHelmet, Stone).some
  }

  final case object BallisticHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Beret, BikeHelmet).some
  }

  final case object FireHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SafetyHelmet, Water).some
  }

  final case object Tiara extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Circlet, StallionMedal).some
  }

  final case object Crown extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Circlet, Gold).some
  }

  final case object CloseHelm extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ChainCoif, Mask).some
  }

  final case object MotorcycleHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (FireHelmet, Binoculars).some
  }

  final case object CrystalTiara extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mithril, ChainCoif).some
  }

  final case object MithrilHelm extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (CrystalTiara, GlassPieces).some
  }

  final case object TacticalOPSHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BallisticHelmet, ElectronicParts).some
  }

  final case object HelmOfBanneret extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (CloseHelm, Rubber).some
  }

  final case object ChineseOperaMask extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mask, VFBloodSample).some
  }

  final case object ImperialCrown extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Crown, Ruby).some
  }

  final case object ImperialBurgonet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (CloseHelm, Gold).some
  }

  final case object LaurelWreath extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (TrueSamadhiFire, Circlet).some
  }

  final case object Windbreaker extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object MonksRobe extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Wetsuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object FabricArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object LeatherArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (FabricArmor, Leather).some
  }

  final case object LeatherJacket extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Windbreaker, Leather).some
  }

  final case object TurtleDobok extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (MonksRobe, TurtleShell).some
  }

  final case object MilitarySuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Windbreaker, Branch).some
  }

  final case object Bikini extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Wetsuit, Scissors).some
  }

  final case object PatchedRobe extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (MonksRobe, Bandage).some
  }

  final case object Dress extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Cloth, Scissors).some
  }

  final case object DivingSuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Wetsuit, Bandage).some
  }

  final case object RiderJacket extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RiderJacket, SteelChain).some
  }

  final case object SunsetArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LeatherArmor, Ruby).some
  }

  final case object CovertAgentUniform extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Hanbok, StallionMedal).some
  }

  final case object ChainArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LeatherArmor, SteelChain).some
  }

  final case object Suit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (MilitarySuit, Ribbon).some
  }

  final case object Qipao extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Dress, Razor).some
  }

  final case object SheetMetalArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (FabricArmor, Steel).some
  }

  final case object Hanbok extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (PatchedRobe, Flower).some
  }

  final case object BulletproofVest extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (MilitarySuit, IronSheet).some
  }

  final case object OpticalCamouflageSuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (DivingSuit, GlassPanel).some
  }

  final case object EODSuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BulletproofVest, PatchedRobe).some
  }

  final case object ButlersSuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Suit, FeatherDuster).some
  }

  final case object RockersJacket extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RiderJacket, ClangClatter).some
  }

  final case object MithrilArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mithril, LeatherArmor).some
  }

  final case object CrusaderArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SheetMetalArmor, SaintsRelic).some
  }

  final case object AmazonessArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SheetMetalArmor, Bikini).some
  }

  final case object DragonDobok extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Qipao, TurtleDobok).some
  }

  final case object CommandersArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ChainArmor, Gold).some
  }

  final case object BattleSuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BulletproofVest, DivingSuit).some
  }

  final case object Kabana extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (Meteorite, CommandersArmor).some
  }

  final case object QueenOfHearts extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (VFBloodSample, Hanbok).some
  }

  final case object BlazingDress extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (TrueSamadhiFire, Dress).some
  }

  final case object Watch extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Bandage extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Bracelet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object SquadLeaderArmband extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Bandage, Needle).some
  }

  final case object LeatherShield extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (TurtleShell, Leather).some
  }

  final case object Bracer extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Bandage, Leather).some
  }

  final case object Sheath extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Leather, IronSheet).some
  }

  final case object CubeWatch extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Watch, Moonstone).some
  }

  final case object GoldenBracelet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Bracelet, Gold).some
  }

  final case object Bazuband extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Bracer, IronSheet).some
  }

  final case object CrimsonBracelet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Bracelet, Ruby).some
  }

  final case object SteelShield extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LeatherShield, Steel).some
  }

  final case object SwordStopper extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Bazuband, Nail).some
  }

  final case object MithrilShield extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mithril, LeatherShield).some
  }

  final case object VitalSignSensor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Watch, CellPhone).some
  }

  final case object Draupnir extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (GoldenBracelet, Bracer).some
  }

  final case object CreedOfTheKnight extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SteelShield, SquadLeaderArmband).some
  }

  final case object SwordOfShahJahan extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Sheath, Ruby).some
  }

  final case object BraceletOfSkadi extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (GlacialIce, CrimsonBracelet).some
  }

  final case object AutoArms extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (Bracer, ForceCore).some
  }

  final case object Radar extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (VitalSignSensor, GlassPanel).some
  }

  final case object Slippers extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object RunningShoes extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Tights extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object KneePads extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Tights, Leather).some
  }

  final case object ChainLeggings extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Tights, SteelChain).some
  }

  final case object HighHeels extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Slippers, ScrapMetal).some
  }

  final case object Heelys extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (RunningShoes, IronBall).some
  }

  final case object RepairedSlippers extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Slippers, Cloth).some
  }

  final case object Boots extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RunningShoes, Oilcloth).some
  }

  final case object SteelKneePads extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (KneePads, Steel).some
  }

  final case object StraitjacketSneakers extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RepairedSlippers, Ash).some
  }

  final case object MaverickRunner extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Heelys, ElectronicParts).some
  }

  final case object CombatBoots extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Boots, Leather).some
  }

  final case object KillerHeels extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (HighHeels, GlassPieces).some
  }

  final case object FeatherBoots extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (CombatBoots, Feather).some
  }

  final case object MithrilBoots extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mithril, ChainLeggings).some
  }

  final case object Bucephalus extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (KillerHeels, ChainLeggings).some
  }

  final case object WhiteRhinos extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (CombatBoots, Nail).some
  }

  final case object EODBoots extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (Boots, SteelKneePads).some
  }

  final case object RedShoes extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (KillerHeels, VFBloodSample).some
  }

  final case object GlacialShoes extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (RepairedSlippers, GlacialIce).some
  }

  final case object BootsOfHermes extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, Heelys).some
  }

  final case object Feather extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Flower extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Ribbon extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Fan extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object BuddhistScripture extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Box extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object HolyGrail extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Cross extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Binoculars extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object GildedQuillFan extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Fan, Nail).some
  }

  final case object SaintsRelic extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Cross, HolyGrail).some
  }

  final case object FlowerOfFate extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Flower, PlayingCards).some
  }

  final case object GlassPieces extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (GlassBottle, Stone).some
  }

  final case object Doll extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Ribbon, Cloth).some
  }

  final case object SniperScope extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (LaserPointer, Binoculars).some
  }

  final case object BuddhaSarira extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (BuddhistScripture, MonksRobe).some
  }

  final case object Quiver extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Leather, Bamboo).some
  }

  final case object FeatherDuster extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ShortRod, Feather).some
  }

  final case object PowderOfLife extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (TreeOfLife, Stone).some
  }

  final case object Uchiwa extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Doll, Fan).some
  }

  final case object Magazine extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Box, IronSheet).some
  }

  final case object LacedQuiver extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Quiver, FeatherDuster).some
  }

  final case object MoonlightPendant extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Ribbon, Moonstone).some
  }

  final case object SchrodingersBox extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Box, Poison).some
  }

  final case object VeritasLuxMea extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BuddhaSarira, SaintsRelic).some
  }

  final case object WhiteCraneFan extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (GildedQuillFan, Feather).some
  }

  final case object GlacialIce extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PowderOfLife, Ice).some
  }

  final case object TrueSamadhiFire extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PowderOfLife, Lighter).some
  }

  final case object EmeraldTablet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, FlowerOfFate).some
  }

  final case object Potato extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Cod extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Lemon extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Garlic extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object AdhesiveBandage extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Carp extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Bread extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Meat extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Egg extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Ramen extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object OrientalHerb extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Chocolate extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object CurryPowder extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object HoneyCodSteak extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Cod, Honey).some
  }

  final case object CannedCodLiver extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Cod, Can).some
  }

  final case object GarlicBread extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Bread, Garlic).some
  }

  final case object Butter extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Milk, Branch).some
  }

  final case object HerbalMedicine extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (OrientalHerb, TurtleShell).some
  }

  final case object CarpBread extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Bread, Carp).some
  }

  final case object HolyWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Disinfectant extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Alcohol, AdhesiveBandage).some
  }

  final case object ChocoPie extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Bread, Chocolate).some
  }

  final case object AcupunctureNeedle extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Alcohol, Needle).some
  }

  final case object Orchid extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (OrientalHerb, Flower).some
  }

  final case object TandooriChicken extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (CurryPowder, Meat).some
  }

  final case object BaconAndGarlicSticks extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Garlic, Meat).some
  }

  final case object Bun extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Bread, Coffee).some
  }

  final case object Hamburger extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Meat, Bread).some
  }

  final case object PotatoBread extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Potato, Bread).some
  }

  final case object PotatoSoup extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Potato, Milk).some
  }

  final case object FishFilletWithEgg extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Cod, Egg).some
  }

  final case object CitrusCake extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Lemon, Bread).some
  }

  final case object LemonIceCream extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Lemon, Egg).some
  }

  final case object HoneyGarlicPickle extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Garlic, Honey).some
  }

  final case object EggBun extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Egg, Bread).some
  }

  final case object EasterEgg extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Egg, Chocolate).some
  }

  final case object WhiskeyBonbon extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Whiskey, Chocolate).some
  }

  final case object ChocoIceCream extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Ice, Chocolate).some
  }

  final case object CurryBun extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (CurryPowder, Bread).some
  }

  final case object FrenchFries extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Potato, HeatedOil).some
  }

  final case object BakedPotato extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Carp, BoilingWater).some
  }

  final case object BakedCarp extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Carp, HeatedStone).some
  }

  final case object GrilledChileanSeaBass extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Cod, HeatedStone).some
  }

  final case object HotRamen extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Ramen, BoilingWater).some
  }

  final case object MochaBread extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Bread, CoffeeLiqueur).some
  }

  final case object ScrambledEggs extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Egg, HeatedOil).some
  }

  final case object ChocolateChipCookies extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Chocolate, Butter).some
  }

  final case object ChocoPieBox extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (ChocoPie, Box).some
  }

  final case object Curry extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (CurryPowder, BoilingWater).some
  }

  final case object OrientalConcoction extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (OrientalHerb, BoilingWater).some
  }

  final case object HoneyButter extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Butter, Honey).some
  }

  final case object FriedChicken extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Meat, HeatedOil).some
  }

  final case object HealingPotion extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Orchid, GlassBottle).some
  }

  final case object BoiledEgg extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Egg, BoilingWater).some
  }

  final case object PoundCake extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Butter, Bread).some
  }

  final case object CurryCroquette extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (CurryPowder, HeatedOil).some
  }

  final case object Steak extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Meat, HeatedStone).some
  }

  final case object FirstAidKit extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Disinfectant, Bandage).some
  }

  final case object ButterFriedPotatoes extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Potato, Butter).some
  }

  final case object FishCutlet extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Cod, HeatedOil).some
  }

  final case object StirFriedRamen extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Ramen, HeatedOil).some
  }

  final case object ColdNoodles extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Ramen, IceWater).some
  }

  final case object ZenVitality extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (AcupunctureNeedle, HerbalMedicine).some
  }

  final case object GarlicRamen extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (HotRamen, Garlic).some
  }

  final case object SpicyFishStew extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Carp, BoilingWater).some
  }

  final case object FishAndChips extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Epic
    val recipe: Option[(Item, Item)]   = (FishCutlet, FrenchFries).some
  }

  final case object Honey extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Water extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Ice extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Whiskey extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Coffee extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object CarbonatedWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object Milk extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
  }

  final case object BoilingWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Water, Lighter).some
  }

  final case object Lemonade extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (CarbonatedWater, Lemon).some
  }

  final case object WaterBottle extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Water, GlassBottle).some
  }

  final case object Baijiu extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Alcohol, Lighter).some
  }

  final case object Soju extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Alcohol, Water).some
  }

  final case object IceCoffee extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Ice, Coffee).some
  }

  final case object Cocktail extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Whiskey, Lemon).some
  }

  final case object CoffeeLiqueur extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Coffee, Alcohol).some
  }

  final case object Cola extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (CarbonatedWater, Honey).some
  }

  final case object Latte extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Milk, Coffee).some
  }

  final case object HoneyMilk extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Milk, Honey).some
  }

  final case object Highball extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Whiskey, CarbonatedWater).some
  }

  final case object ChocolateMilk extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Chocolate, Milk).some
  }

  final case object HoneyWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Honey, Water).some
  }

  final case object IceWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Ice, Water).some
  }

  final case object OnTheRocks extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Ice, Whiskey).some
  }

  final case object Cowboy extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Milk, Whiskey).some
  }

  final case object KaoliangLiquor extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Baijiu, Lighter).some
  }

  final case object HotHoneyWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (BoilingWater, Honey).some
  }

  final case object FlowerLiquor extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Baijiu, Flower).some
  }

  final case object Americano extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (BoilingWater, Coffee).some
  }

  final case object HerbalLiquor extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Baijiu, OrientalHerb).some
  }

  final case object WhiskeyCocktail extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Cola, Whiskey).some
  }

  final case object PurifiedWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (BoilingWater, Ice).some
  }

  final case object CanOfCola extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Cola, Can).some
  }

  final case object HotChocolate extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (BoilingWater, Chocolate).some
  }

  final case object WhiteRussian extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (CoffeeLiqueur, Milk).some
  }

  final case object Snare extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Mousetrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object PianoWire extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object SpikedPlank extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Mousetrap, Nail).some
  }

  final case object EnhancedMousetrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Mousetrap, IronOre).some
  }

  final case object Dynamite extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (PianoWire, Gunpowder).some
  }

  final case object BambooTrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Snare, Bamboo).some
  }

  final case object BoobyTrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Snare, Glue).some
  }

  final case object ClangClatter extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Can, IronBall).some
  }

  final case object JungleGuillotine extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Mousetrap, KitchenKnife).some
  }

  final case object ExplosiveTrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Mousetrap, Gunpowder).some
  }

  final case object PendulumAxe extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BambooTrap, Hatchet).some
  }

  final case object Mine extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BoobyTrap, Gunpowder).some
  }

  final case object RDX extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Dynamite, ScrapMetal).some
  }

  final case object MithrilString extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Mithril, PianoWire).some
  }

  final case object HiddenMaiden extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SpikedPlank, JungleGuillotine).some
  }

  final case object FireTrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ExplosiveTrap, Oilcloth).some
  }

  final case object C4 extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RDX, WhitePowder).some
  }

  final case object DoubleGuillotine extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (JungleGuillotine, PendulumAxe).some
  }

  final case object Claymore extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mine, ExplosiveTrap).some
  }

  final case object Stingburst extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RDX, SpikedPlank).some
  }

  final case object RemoteMine extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, SpikedPlank).some
  }

  final case object SmartBomb extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (RDX, CellPhone).some
  }

  final case object SurveillanceCamera extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object TelephotoCamera extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = (SurveillanceCamera, Binoculars).some
  }

  final case object Stone extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object GlassBottle extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Nail extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Leather extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object TurtleShell extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Rubber extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object ScrapMetal extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Lighter extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object LaserPointer extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object StallionMedal extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Battery extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Alcohol extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Oil extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Cloth extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Gemstone extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Glue extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Paper extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object IronOre extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Can extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Gunpowder extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
  }

  final case object Steel extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ScrapMetal, IronOre).some
  }

  final case object Oilcloth extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Oil, Bandage).some
  }

  final case object HeatedOil extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Oil, Lighter).some
  }

  final case object Ruby extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hammer, Gemstone).some
  }

  final case object DeadBattery extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Battery, Water).some
  }

  final case object WhitePowder extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Chalk, Stone).some
  }

  final case object HeatedStone extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Stone, Lighter).some
  }

  final case object Meteorite extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = None
  }

  final case object Ash extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Paper, Lighter).some
  }

  final case object ElectronicParts extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Battery, PianoWire).some
  }

  final case object Blueprint extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (FountainPen, Paper).some
  }

  final case object IronSheet extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ScrapMetal, Hammer).some
  }

  final case object Gold extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Pickaxe, Gemstone).some
  }

  final case object TreeOfLife extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = None
  }

  final case object Moonstone extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Meteorite, Stone).some
  }

  final case object Poison extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Ash, Water).some
  }

  final case object Motor extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ElectronicParts, ScrapMetal).some
  }

  final case object Mithril extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = None
  }

  final case object GlassPanel extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (GlassPieces, Glue).some
  }

  final case object IonBattery extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (DeadBattery, CarbonatedWater).some
  }

  final case object VFBloodSample extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = None
  }

  final case object CellPhone extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Blueprint, ElectronicParts).some
  }

  final case object ForceCore extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PowderOfLife, Meteorite).some
  }

//  val allItems: Set[Item] = fetchAll[Item]
}
