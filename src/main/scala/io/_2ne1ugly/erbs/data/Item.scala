package io._2ne1ugly.erbs.data

import cats.syntax.all._
import enumeratum._

sealed trait ItemRarity

object ItemRarity {
  final case object Common    extends ItemRarity
  final case object Uncommon  extends ItemRarity
  final case object Rare      extends ItemRarity
  final case object Epic      extends ItemRarity
  final case object Legendary extends ItemRarity
}

sealed trait Item extends EnumEntry {
  import Item._

  val bundleCount: Int
  val maxStack: Int
  val recipe: Option[(Item, Item)]
  val imgSrc: String

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

object Item extends Enum[Item] {
  final case object Scissors extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Scissors.png"
  }

  final case object FountainPen extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/FountainPen.png"
  }

  final case object KitchenKnife extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/KitchenKnife.png"
  }

  final case object ArmyKnife extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (KitchenKnife, Branch).some
    val imgSrc: String               = "images/items/ArmyKnife.png"
  }

  final case object RoseKnife extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ArmyKnife, Flower).some
    val imgSrc: String               = "images/items/RoseKnife.png"
  }

  final case object Carnwennan extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RoseKnife, SaintsRelic).some
    val imgSrc: String               = "images/items/Carnwennan.png"
  }

  final case object MountSlicer extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RoseKnife, Ash).some
    val imgSrc: String               = "images/items/MountSlicer.png"
  }

  final case object Vibroblade extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ArmyKnife, Motor).some
    val imgSrc: String               = "images/items/Vibroblade.png"
  }

  final case object Fragarach extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Dagger
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ArmyKnife, ForceCore).some
    val imgSrc: String               = "images/items/Fragarach.png"
  }

  final case object RustySword extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/RustySword.png"
  }

  final case object Shamshir extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (RustySword, Lighter).some
    val imgSrc: String               = "images/items/Shamshir.png"
  }

  final case object Katana extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RustySword, IronSheet).some
    val imgSrc: String               = "images/items/Katana.png"
  }

  final case object Masamune extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Katana, Oil).some
    val imgSrc: String               = "images/items/Masamune.png"
  }

  final case object Muramasa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Katana, Gemstone).some
    val imgSrc: String               = "images/items/Muramasa.png"
  }

  final case object BastardSword extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RustySword, Steel).some
    val imgSrc: String               = "images/items/BastardSword.png"
  }

  final case object JewelSword extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Shamshir, Ruby).some
    val imgSrc: String               = "images/items/JewelSword.png"
  }

  final case object ThuanThien extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BastardSword, TurtleShell).some
    val imgSrc: String               = "images/items/ThuanThien.png"
  }

  final case object PlasmaSword extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BastardSword, LaserPointer).some
    val imgSrc: String               = "images/items/PlasmaSword.png"
  }

  final case object Excalibur extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (JewelSword, HolyGrail).some
    val imgSrc: String               = "images/items/Excalibur.png"
  }

  final case object Arondight extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Masamune, Cross).some
    val imgSrc: String               = "images/items/Arondight.png"
  }

  final case object Hovud extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (JewelSword, GlassPieces).some
    val imgSrc: String               = "images/items/Hovud.png"
  }

  final case object Monohoshizao extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Muramasa, Blueprint).some
    val imgSrc: String               = "images/items/Monohoshizao.png"
  }

  final case object Laevateinn extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (TrueSamadhiFire, Shamshir).some
    val imgSrc: String               = "images/items/Laevateinn.png"
  }

  final case object Dainsleif extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.TwoHandedSword
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (VFBloodSample, Katana).some
    val imgSrc: String               = "images/items/Dainsleif.png"
  }

  final case object Pickaxe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Pickaxe.png"
  }

  final case object Hatchet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Hatchet.png"
  }

  final case object ChainScythe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Pickaxe, SteelChain).some
    val imgSrc: String               = "images/items/ChainScythe.png"
  }

  final case object BattleAxe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hatchet, Bamboo).some
    val imgSrc: String               = "images/items/BattleAxe.png"
  }

  final case object LightHatchet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BattleAxe, Feather).some
    val imgSrc: String               = "images/items/LightHatchet.png"
  }

  final case object ReapersScythe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ChainScythe, ShortRod).some
    val imgSrc: String               = "images/items/ReapersScythe.png"
  }

  final case object GiganticAxe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BattleAxe, Steel).some
    val imgSrc: String               = "images/items/GiganticAxe.png"
  }

  final case object BeamAxe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (GiganticAxe, LaserPointer).some
    val imgSrc: String               = "images/items/BeamAxe.png"
  }

  final case object SantaMuerte extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ReapersScythe, Gold).some
    val imgSrc: String               = "images/items/SantaMuerte.png"
  }

  final case object Scythe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ReapersScythe, Scythe).some
    val imgSrc: String               = "images/items/Scythe.png"
  }

  final case object Parashu extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (LightHatchet, BuddhaSarira).some
    val imgSrc: String               = "images/items/Parashu.png"
  }

  final case object Harpe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ReapersScythe, WhiteCraneFan).some
    val imgSrc: String               = "images/items/Harpe.png"
  }

  final case object TwinSwords extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (KitchenKnife, RustySword).some
    val imgSrc: String               = "images/items/TwinSwords.png"
  }

  final case object Florentine extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (TwinSwords, Blueprint).some
    val imgSrc: String               = "images/items/Florentine.png"
  }

  final case object DivineDualSwords extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Florentine, Nail).some
    val imgSrc: String               = "images/items/DivineDualSwords.png"
  }

  final case object StarsteelTwinSwords extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (TwinSwords, Moonstone).some
    val imgSrc: String               = "images/items/StarsteelTwinSwords.png"
  }

  final case object Dioscuri extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (TwinSwords, IonBattery).some
    val imgSrc: String               = "images/items/Dioscuri.png"
  }

  final case object LloigorAndZahr extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Axe
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (TwinSwords, Poison).some
    val imgSrc: String               = "images/items/LloigorAndZahr.png"
  }

  final case object WaltherPPK extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/WaltherPPK.png"
  }

  final case object MagnumPython extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (WaltherPPK, Oil).some
    val imgSrc: String               = "images/items/MagnumPython.png"
  }

  final case object BerettaM92F extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (WaltherPPK, Leather).some
    val imgSrc: String               = "images/items/BerettaM92F.png"
  }

  final case object FN57 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BerettaM92F, LaserPointer).some
    val imgSrc: String               = "images/items/FN57.png"
  }

  final case object DoubleRevolverSP extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (MagnumPython, BerettaM92F).some
    val imgSrc: String               = "images/items/DoubleRevolverSP.png"
  }

  final case object MagnumAnaconda extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (MagnumPython, Blueprint).some
    val imgSrc: String               = "images/items/MagnumAnaconda.png"
  }

  final case object DevilsMarksman extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (DoubleRevolverSP, Ash).some
    val imgSrc: String               = "images/items/DevilsMarksman.png"
  }

  final case object Elegance extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (FN57, FeatherDuster).some
    val imgSrc: String               = "images/items/Elegance.png"
  }

  final case object ElectronBlaster extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BerettaM92F, IonBattery).some
    val imgSrc: String               = "images/items/ElectronBlaster.png"
  }

  final case object MagnumBoa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (MagnumAnaconda, Steel).some
    val imgSrc: String               = "images/items/MagnumBoa.png"
  }

  final case object Kelte extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Pistol
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, BerettaM92F).some
    val imgSrc: String               = "images/items/Kelte.png"
  }

  final case object Fedorova extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Fedorova.png"
  }

  final case object STG44 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Fedorova, Gunpowder).some
    val imgSrc: String               = "images/items/STG44.png"
  }

  final case object AK47 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (STG44, PianoWire).some
    val imgSrc: String               = "images/items/AK47.png"
  }

  final case object M16A1 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (STG44, Leather).some
    val imgSrc: String               = "images/items/M16A1.png"
  }

  final case object MachineGun extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (STG44, Motor).some
    val imgSrc: String               = "images/items/MachineGun.png"
  }

  final case object GatlingGun extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (MachineGun, Oil).some
    val imgSrc: String               = "images/items/GatlingGun.png"
  }

  final case object AK12 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (AK47, GlassPanel).some
    val imgSrc: String               = "images/items/AK12.png"
  }

  final case object XCR extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.AssaultRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (M16A1, Magazine).some
    val imgSrc: String               = "images/items/XCR.png"
  }

  final case object LongRifle extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/LongRifle.png"
  }

  final case object Springfield extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (LongRifle, LaserPointer).some
    val imgSrc: String               = "images/items/Springfield.png"
  }

  final case object HarpoonGun extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Springfield, ShortSpear).some
    val imgSrc: String               = "images/items/HarpoonGun.png"
  }

  final case object GoldenRifle extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Springfield, Gold).some
    val imgSrc: String               = "images/items/GoldenRifle.png"
  }

  final case object Railgun extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Springfield, ElectronicParts).some
    val imgSrc: String               = "images/items/Railgun.png"
  }

  final case object Tac50 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HarpoonGun, Blueprint).some
    val imgSrc: String               = "images/items/Tac50.png"
  }

  final case object Intervention extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HarpoonGun, TelephotoCamera).some
    val imgSrc: String               = "images/items/Intervention.png"
  }

  final case object NTW20 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (GoldenRifle, IronSheet).some
    val imgSrc: String               = "images/items/NTW20.png"
  }

  final case object Polaris extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Railgun, WhitePowder).some
    val imgSrc: String               = "images/items/Polaris.png"
  }

  final case object TheDeadlyRay extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.SniperRifle
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (Moonstone, GoldenRifle).some
    val imgSrc: String               = "images/items/TheDeadlyRay.png"
  }

  final case object Needle extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Needle.png"
  }

  final case object FencingRapier extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Needle, IronOre).some
    val imgSrc: String               = "images/items/FencingRapier.png"
  }

  final case object ApricotSword extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (FencingRapier, FlowerOfFate).some
    val imgSrc: String               = "images/items/ApricotSword.png"
  }

  final case object SwordOfJustice extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (FencingRapier, IronOre).some
    val imgSrc: String               = "images/items/SwordOfJustice.png"
  }

  final case object DurendalMk2 extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ApricotSword, LaserPointer).some
    val imgSrc: String               = "images/items/DurendalMk2.png"
  }

  final case object Volticletto extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ApricotSword, ElectronicParts).some
    val imgSrc: String               = "images/items/Volticletto.png"
  }

  final case object MeteorClaymore extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ApricotSword, Meteorite).some
    val imgSrc: String               = "images/items/MeteorClaymore.png"
  }

  final case object Joyeuse extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (FencingRapier, Mithril).some
    val imgSrc: String               = "images/items/Joyeuse.png"
  }

  final case object Mistilteinn extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Rapier
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (SwordOfJustice, Branch).some
    val imgSrc: String               = "images/items/Mistilteinn.png"
  }

  final case object ShortSpear extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/ShortSpear.png"
  }

  final case object BambooSpear extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ShortSpear, Bamboo).some
    val imgSrc: String               = "images/items/BambooSpear.png"
  }

  final case object Bident extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BambooSpear, KitchenKnife).some
    val imgSrc: String               = "images/items/Bident.png"
  }

  final case object Pike extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ShortSpear, Steel).some
    val imgSrc: String               = "images/items/Pike.png"
  }

  final case object HalberdAxe extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Hatchet, Pike).some
    val imgSrc: String               = "images/items/HalberdAxe.png"
  }

  final case object SharpenedSpear extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BambooSpear, Feather).some
    val imgSrc: String               = "images/items/SharpenedSpear.png"
  }

  final case object GentianSilverGun extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SharpenedSpear, WhitePowder).some
    val imgSrc: String               = "images/items/GentianSilverGun.png"
  }

  final case object EighteenFootSpear extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SharpenedSpear, IonBattery).some
    val imgSrc: String               = "images/items/EighteenFootSpear.png"
  }

  final case object CosmicBident extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Bident, Moonstone).some
    val imgSrc: String               = "images/items/CosmicBident.png"
  }

  final case object LanceOfPoseidon extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Bident, Pike).some
    val imgSrc: String               = "images/items/LanceOfPoseidon.png"
  }

  final case object FangtianHuaji extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HalberdAxe, GildedQuillFan).some
    val imgSrc: String               = "images/items/FangtianHuaji.png"
  }

  final case object DragonGuandao extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HalberdAxe, IronSheet).some
    val imgSrc: String               = "images/items/DragonGuandao.png"
  }

  final case object BlazingLance extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (TrueSamadhiFire, BambooSpear).some
    val imgSrc: String               = "images/items/BlazingLance.png"
  }

  final case object SpearOfLonginus extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Spear
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (VFBloodSample, Pike).some
    val imgSrc: String               = "images/items/SpearOfLonginus.png"
  }

  final case object Hammer extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Hammer.png"
  }

  final case object Warhammer extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hammer, ShortRod).some
    val imgSrc: String               = "images/items/Warhammer.png"
  }

  final case object MorningStar extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Warhammer, IronBall).some
    val imgSrc: String               = "images/items/MorningStar.png"
  }

  final case object BlackStagHammer extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Warhammer, Leather).some
    val imgSrc: String               = "images/items/BlackStagHammer.png"
  }

  final case object FangMace extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (MorningStar, HeatedStone).some
    val imgSrc: String               = "images/items/FangMace.png"
  }

  final case object HammerOfDagda extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BlackStagHammer, SaintsRelic).some
    val imgSrc: String               = "images/items/HammerOfDagda.png"
  }

  final case object HammerOfThor extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BlackStagHammer, IonBattery).some
    val imgSrc: String               = "images/items/HammerOfThor.png"
  }

  final case object EveningStar extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Moonstone, MorningStar).some
    val imgSrc: String               = "images/items/EveningStar.png"
  }

  final case object MagicStick extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Hammer
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Warhammer, MoonlightPendant).some
    val imgSrc: String               = "images/items/MagicStick.png"
  }

  final case object Branch extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Branch.png"
  }

  final case object ShortRod extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/ShortRod.png"
  }

  final case object LongRod extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ShortRod, Bamboo).some
    val imgSrc: String               = "images/items/LongRod.png"
  }

  final case object GoblinBat extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LongRod, Nail).some
    val imgSrc: String               = "images/items/GoblinBat.png"
  }

  final case object Torch extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (FeatherDuster, Oilcloth).some
    val imgSrc: String               = "images/items/Torch.png"
  }

  final case object Umbrella extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LongRod, Fan).some
    val imgSrc: String               = "images/items/Umbrella.png"
  }

  final case object StatueOfSoteria extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Torch, Doll).some
    val imgSrc: String               = "images/items/StatueOfSoteria.png"
  }

  final case object SpyUmbrella extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Umbrella, Poison).some
    val imgSrc: String               = "images/items/SpyUmbrella.png"
  }

  final case object Mallet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (GoblinBat, Motor).some
    val imgSrc: String               = "images/items/Mallet.png"
  }

  final case object MonkeyKingBar extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bat
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, LongRod).some
    val imgSrc: String               = "images/items/MonkeyKingBar.png"
  }

  final case object IronBall extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/IronBall.png"
  }

  final case object Baseball extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Baseball.png"
  }

  final case object Grenade extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (IronBall, Gunpowder).some
    val imgSrc: String               = "images/items/Grenade.png"
  }

  final case object MolotovCocktail extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (GlassBottle, Oil).some
    val imgSrc: String               = "images/items/MolotovCocktail.png"
  }

  final case object SignedBall extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Baseball, FountainPen).some
    val imgSrc: String               = "images/items/SignedBall.png"
  }

  final case object Sling extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SignedBall, Rubber).some
    val imgSrc: String               = "images/items/Sling.png"
  }

  final case object FlourBomb extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (WhitePowder, MolotovCocktail).some
    val imgSrc: String               = "images/items/FlourBomb.png"
  }

  final case object BallLightning extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (IronBall, DeadBattery).some
    val imgSrc: String               = "images/items/BallLightning.png"
  }

  final case object Flubber extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Rubber, BoilingWater).some
    val imgSrc: String               = "images/items/Flubber.png"
  }

  final case object SpikyBouncyBall extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Flubber, Nail).some
    val imgSrc: String               = "images/items/SpikyBouncyBall.png"
  }

  final case object IncendiaryBomb extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (MolotovCocktail, BallLightning).some
    val imgSrc: String               = "images/items/IncendiaryBomb.png"
  }

  final case object GrenadeOfAntioch extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HighExplosiveGrenade, Cross).some
    val imgSrc: String               = "images/items/GrenadeOfAntioch.png"
  }

  final case object DavidsSling extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Sling, SaintsRelic).some
    val imgSrc: String               = "images/items/DavidsSling.png"
  }

  final case object SmokeBomb extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (FlourBomb, Cola).some
    val imgSrc: String               = "images/items/SmokeBomb.png"
  }

  final case object HighExplosiveGrenade extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Grenade, RDX).some
    val imgSrc: String               = "images/items/HighExplosiveGrenade.png"
  }

  final case object RutheniumMarble extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Throw
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SpikyBouncyBall, Gold).some
    val imgSrc: String               = "images/items/RutheniumMarble.png"
  }

  final case object Razor extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Razor.png"
  }

  final case object PlayingCards extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/PlayingCards.png"
  }

  final case object Chalk extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Chalk.png"
  }

  final case object Dart extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Needle, Feather).some
    val imgSrc: String               = "images/items/Dart.png"
  }

  final case object VintageCards extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (PlayingCards, FountainPen).some
    val imgSrc: String               = "images/items/VintageCards.png"
  }

  final case object OnyxDagger extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Razor, Cross).some
    val imgSrc: String               = "images/items/OnyxDagger.png"
  }

  final case object ThrowingStars extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Razor, PianoWire).some
    val imgSrc: String               = "images/items/ThrowingStars.png"
  }

  final case object Charm extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (VintageCards, BuddhistScripture).some
    val imgSrc: String               = "images/items/Charm.png"
  }

  final case object WillowLeafSpike extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (OnyxDagger, Branch).some
    val imgSrc: String               = "images/items/WillowLeafSpike.png"
  }

  final case object Chakram extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ThrowingStars, StallionMedal).some
    val imgSrc: String               = "images/items/Chakram.png"
  }

  final case object ApricotFlowerTag extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (WillowLeafSpike, Flower).some
    val imgSrc: String               = "images/items/ApricotFlowerTag.png"
  }

  final case object VenomDart extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Needle, Poison).some
    val imgSrc: String               = "images/items/VenomDart.png"
  }

  final case object DharmaChakram extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Chakram, BuddhistScripture).some
    val imgSrc: String               = "images/items/DharmaChakram.png"
  }

  final case object Plumbata extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Dart, Steel).some
    val imgSrc: String               = "images/items/Plumbata.png"
  }

  final case object CardsOfTyranny extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (VintageCards, IonBattery).some
    val imgSrc: String               = "images/items/CardsOfTyranny.png"
  }

  final case object MysticJadeCharm extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Charm, Ash).some
    val imgSrc: String               = "images/items/MysticJadeCharm.png"
  }

  final case object FuhmaShuriken extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ApricotFlowerTag, Alcohol).some
    val imgSrc: String               = "images/items/FuhmaShuriken.png"
  }

  final case object WindAndFireWheels extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (DharmaChakram, Bamboo).some
    val imgSrc: String               = "images/items/WindAndFireWheels.png"
  }

  final case object AzureDagger extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (OnyxDagger, Poison).some
    val imgSrc: String               = "images/items/AzureDagger.png"
  }

  final case object Flechette extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Plumbata, WhitePowder).some
    val imgSrc: String               = "images/items/Flechette.png"
  }

  final case object FrostVenomDart extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (VenomDart, Ice).some
    val imgSrc: String               = "images/items/FrostVenomDart.png"
  }

  final case object DeathRune extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Charm, TreeOfLife).some
    val imgSrc: String               = "images/items/DeathRune.png"
  }

  final case object Sudarsana extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, ThrowingStars).some
    val imgSrc: String               = "images/items/Sudarsana.png"
  }

  final case object PetalTorrent extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Shuriken
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (FrostVenomDart, Stingburst).some
    val imgSrc: String               = "images/items/PetalTorrent.png"
  }

  final case object Bow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Bow.png"
  }

  final case object WoodenBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Branch, PianoWire).some
    val imgSrc: String               = "images/items/WoodenBow.png"
  }

  final case object Longbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Bow, Rubber).some
    val imgSrc: String               = "images/items/Longbow.png"
  }

  final case object CompositeBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Longbow, Nail).some
    val imgSrc: String               = "images/items/CompositeBow.png"
  }

  final case object StrongBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (WoodenBow, Oil).some
    val imgSrc: String               = "images/items/StrongBow.png"
  }

  final case object StallionBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (MightyBow, StallionMedal).some
    val imgSrc: String               = "images/items/StallionBow.png"
  }

  final case object MightyBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Longbow, Gunpowder).some
    val imgSrc: String               = "images/items/MightyBow.png"
  }

  final case object PelletBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (WoodenBow, HeatedStone).some
    val imgSrc: String               = "images/items/PelletBow.png"
  }

  final case object Scorchbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Longbow, Lighter).some
    val imgSrc: String               = "images/items/Scorchbow.png"
  }

  final case object AncientBolt extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (StallionBow, Bamboo).some
    val imgSrc: String               = "images/items/AncientBolt.png"
  }

  final case object GoldenRatioBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PelletBow, Gold).some
    val imgSrc: String               = "images/items/GoldenRatioBow.png"
  }

  final case object Twinbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (StrongBow, CompositeBow).some
    val imgSrc: String               = "images/items/Twinbow.png"
  }

  final case object ElementalBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Scorchbow, WhiteCraneFan).some
    val imgSrc: String               = "images/items/ElementalBow.png"
  }

  final case object Failnaught extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Bow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (VFBloodSample, StrongBow).some
    val imgSrc: String               = "images/items/Failnaught.png"
  }

  final case object ShortCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/ShortCrossbow.png"
  }

  final case object LongCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ShortCrossbow, PianoWire).some
    val imgSrc: String               = "images/items/LongCrossbow.png"
  }

  final case object Crossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ShortCrossbow, Bamboo).some
    val imgSrc: String               = "images/items/Crossbow.png"
  }

  final case object PowerCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LongCrossbow, Rubber).some
    val imgSrc: String               = "images/items/PowerCrossbow.png"
  }

  final case object LaserCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Crossbow, LaserPointer).some
    val imgSrc: String               = "images/items/LaserCrossbow.png"
  }

  final case object HeavyCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LongCrossbow, Steel).some
    val imgSrc: String               = "images/items/HeavyCrossbow.png"
  }

  final case object SteelBow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Crossbow, IronSheet).some
    val imgSrc: String               = "images/items/SteelBow.png"
  }

  final case object TheLegendOfTheGeneral extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SteelBow, Oilcloth).some
    val imgSrc: String               = "images/items/TheLegendOfTheGeneral.png"
  }

  final case object Ballista extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HeavyCrossbow, ShortSpear).some
    val imgSrc: String               = "images/items/Ballista.png"
  }

  final case object SniperCrossbow extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (LaserCrossbow, SniperScope).some
    val imgSrc: String               = "images/items/SniperCrossbow.png"
  }

  final case object TheGoldenGhost extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PowerCrossbow, RDX).some
    val imgSrc: String               = "images/items/TheGoldenGhost.png"
  }

  final case object Sharanga extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Crossbow
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ForceCore, Crossbow).some
    val imgSrc: String               = "images/items/Sharanga.png"
  }

  final case object BrassKnuckles extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/BrassKnuckles.png"
  }

  final case object CottonGloves extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/CottonGloves.png"
  }

  final case object LeatherGloves extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (CottonGloves, Leather).some
    val imgSrc: String               = "images/items/LeatherGloves.png"
  }

  final case object IronKnuckles extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (BrassKnuckles, IronOre).some
    val imgSrc: String               = "images/items/IronKnuckles.png"
  }

  final case object Gauntlet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (CottonGloves, Steel).some
    val imgSrc: String               = "images/items/Gauntlet.png"
  }

  final case object WingKnuckles extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (IronKnuckles, Feather).some
    val imgSrc: String               = "images/items/WingKnuckles.png"
  }

  final case object BoneGauntlet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Gauntlet, TurtleShell).some
    val imgSrc: String               = "images/items/BoneGauntlet.png"
  }

  final case object ShatterShellGauntlet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Gauntlet, Gunpowder).some
    val imgSrc: String               = "images/items/ShatterShellGauntlet.png"
  }

  final case object GlassKnuckles extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (IronKnuckles, GlassPieces).some
    val imgSrc: String               = "images/items/GlassKnuckles.png"
  }

  final case object PhoenixGloves extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LeatherGloves, Ash).some
    val imgSrc: String               = "images/items/PhoenixGloves.png"
  }

  final case object OneInchPunch extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BoneGauntlet, Doll).some
    val imgSrc: String               = "images/items/OneInchPunch.png"
  }

  final case object DivineFist extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ShatterShellGauntlet, Cross).some
    val imgSrc: String               = "images/items/DivineFist.png"
  }

  final case object BloodwingKnuckles extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (WingKnuckles, Ruby).some
    val imgSrc: String               = "images/items/BloodwingKnuckles.png"
  }

  final case object FrostPetalHand extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PhoenixGloves, Ice).some
    val imgSrc: String               = "images/items/FrostPetalHand.png"
  }

  final case object BuddhasPalm extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PhoenixGloves, BuddhaSarira).some
    val imgSrc: String               = "images/items/BuddhasPalm.png"
  }

  final case object BrasilGauntlet extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BoneGauntlet, Oilcloth).some
    val imgSrc: String               = "images/items/BrasilGauntlet.png"
  }

  final case object WhiteClawPunch extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (GlassKnuckles, WhitePowder).some
    val imgSrc: String               = "images/items/WhiteClawPunch.png"
  }

  final case object ImperialSilkGloves extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Glove
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mithril, LeatherGloves).some
    val imgSrc: String               = "images/items/ImperialSilkGloves.png"
  }

  final case object Bamboo extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Bamboo.png"
  }

  final case object WoodenTonfa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Bamboo, Branch).some
    val imgSrc: String               = "images/items/WoodenTonfa.png"
  }

  final case object PoliceBaton extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (PoliceBaton, StallionMedal).some
    val imgSrc: String               = "images/items/PoliceBaton.png"
  }

  final case object RyukyuTonfa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (WoodenTonfa, WhitePowder).some
    val imgSrc: String               = "images/items/RyukyuTonfa.png"
  }

  final case object TacticalTonfa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PoliceBaton, Blueprint).some
    val imgSrc: String               = "images/items/TacticalTonfa.png"
  }

  final case object MaiSok extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RyukyuTonfa, ShortRod).some
    val imgSrc: String               = "images/items/MaiSok.png"
  }

  final case object PlasmaTonfa extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Tonfa
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RyukyuTonfa, LaserPointer).some
    val imgSrc: String               = "images/items/PlasmaTonfa.png"
  }

  final case object StarterGuitar extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/StarterGuitar.png"
  }

  final case object GoldenBridge extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (StarterGuitar, Gold).some
    val imgSrc: String               = "images/items/GoldenBridge.png"
  }

  final case object SingleCoilPickup extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (StarterGuitar, DeadBattery).some
    val imgSrc: String               = "images/items/SingleCoilPickup.png"
  }

  final case object RubySpecial extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SingleCoilPickup, Ruby).some
    val imgSrc: String               = "images/items/RubySpecial.png"
  }

  final case object HumbuckerPickup extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (GoldenBridge, SniperScope).some
    val imgSrc: String               = "images/items/HumbuckerPickup.png"
  }

  final case object KingV extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (GoldenBridge, Scissors).some
    val imgSrc: String               = "images/items/KingV.png"
  }

  final case object Nocaster extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SingleCoilPickup, Blueprint).some
    val imgSrc: String               = "images/items/Nocaster.png"
  }

  final case object Superstrat extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SingleCoilPickup, Bamboo).some
    val imgSrc: String               = "images/items/Superstrat.png"
  }

  final case object WildHorse extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SingleCoilPickup, Oil).some
    val imgSrc: String               = "images/items/WildHorse.png"
  }

  final case object Bohemian extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RubySpecial, PlayingCards).some
    val imgSrc: String               = "images/items/Bohemian.png"
  }

  final case object StairwayToHeaven extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (HumbuckerPickup, HolyGrail).some
    val imgSrc: String               = "images/items/StairwayToHeaven.png"
  }

  final case object PurpleHaze extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (KingV, Ash).some
    val imgSrc: String               = "images/items/PurpleHaze.png"
  }

  final case object Satisfaction extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Nocaster, Stone).some
    val imgSrc: String               = "images/items/Satisfaction.png"
  }

  final case object WonderfulTonight extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Superstrat, Meteorite).some
    val imgSrc: String               = "images/items/WonderfulTonight.png"
  }

  final case object TheWall extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Superstrat, WhitePowder).some
    val imgSrc: String               = "images/items/TheWall.png"
  }

  final case object TeenSpirit extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Guitar
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (WildHorse, BuddhaSarira).some
    val imgSrc: String               = "images/items/TeenSpirit.png"
  }

  final case object SteelChain extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/SteelChain.png"
  }

  final case object Nunchaku extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (SteelChain, ScrapMetal).some
    val imgSrc: String               = "images/items/Nunchaku.png"
  }

  final case object Sharper extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Nunchaku, Nail).some
    val imgSrc: String               = "images/items/Sharper.png"
  }

  final case object Bleeder extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Nunchaku, Razor).some
    val imgSrc: String               = "images/items/Bleeder.png"
  }

  final case object TheSmitingDragon extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Sharper, Ash).some
    val imgSrc: String               = "images/items/TheSmitingDragon.png"
  }

  final case object VibroNunchaku extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Nunchaku
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Bleeder, Motor).some
    val imgSrc: String               = "images/items/VibroNunchaku.png"
  }

  final case object Whip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Whip.png"
  }

  final case object RopeCuffs extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Whip, StallionMedal).some
    val imgSrc: String               = "images/items/RopeCuffs.png"
  }

  final case object Bullwhip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Whip, Razor).some
    val imgSrc: String               = "images/items/Bullwhip.png"
  }

  final case object WindWhip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RopeCuffs, Fan).some
    val imgSrc: String               = "images/items/WindWhip.png"
  }

  final case object ThunderWhip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Bullwhip, Gold).some
    val imgSrc: String               = "images/items/ThunderWhip.png"
  }

  final case object LightningWhip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Bullwhip, DeadBattery).some
    val imgSrc: String               = "images/items/LightningWhip.png"
  }

  final case object Gleipnir extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (WindWhip, HoneyCodSteak).some
    val imgSrc: String               = "images/items/Gleipnir.png"
  }

  final case object PlasmaWhip extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (LightningWhip, LaserPointer).some
    val imgSrc: String               = "images/items/PlasmaWhip.png"
  }

  final case object WhipOfNineBloodyTails extends Item with Weapon {
    val weaponType: WeaponType       = WeaponType.Whip
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (VFBloodSample, WindWhip).some
    val imgSrc: String               = "images/items/WhipOfNineBloodyTails.png"
  }

  final case object Hairband extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Hairband.png"
  }

  final case object Hat extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Hat.png"
  }

  final case object BikeHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/BikeHelmet.png"
  }

  final case object Mask extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hairband, Feather).some
    val imgSrc: String               = "images/items/Mask.png"
  }

  final case object Circlet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hairband, Branch).some
    val imgSrc: String               = "images/items/Circlet.png"
  }

  final case object Beret extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hat, Scissors).some
    val imgSrc: String               = "images/items/Beret.png"
  }

  final case object ChainCoif extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hat, SteelChain).some
    val imgSrc: String               = "images/items/ChainCoif.png"
  }

  final case object SafetyHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (BikeHelmet, Stone).some
    val imgSrc: String               = "images/items/SafetyHelmet.png"
  }

  final case object BallisticHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Beret, BikeHelmet).some
    val imgSrc: String               = "images/items/BallisticHelmet.png"
  }

  final case object FireHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SafetyHelmet, Water).some
    val imgSrc: String               = "images/items/FireHelmet.png"
  }

  final case object Tiara extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Circlet, StallionMedal).some
    val imgSrc: String               = "images/items/Tiara.png"
  }

  final case object Crown extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Circlet, Gold).some
    val imgSrc: String               = "images/items/Crown.png"
  }

  final case object CloseHelm extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ChainCoif, Mask).some
    val imgSrc: String               = "images/items/CloseHelm.png"
  }

  final case object MotorcycleHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (FireHelmet, Binoculars).some
    val imgSrc: String               = "images/items/MotorcycleHelmet.png"
  }

  final case object CrystalTiara extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mithril, ChainCoif).some
    val imgSrc: String               = "images/items/CrystalTiara.png"
  }

  final case object MithrilHelm extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (CrystalTiara, GlassPieces).some
    val imgSrc: String               = "images/items/MithrilHelm.png"
  }

  final case object TacticalOPSHelmet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BallisticHelmet, ElectronicParts).some
    val imgSrc: String               = "images/items/TacticalOPSHelmet.png"
  }

  final case object HelmOfBanneret extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (CloseHelm, Rubber).some
    val imgSrc: String               = "images/items/HelmOfBanneret.png"
  }

  final case object ChineseOperaMask extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mask, VFBloodSample).some
    val imgSrc: String               = "images/items/ChineseOperaMask.png"
  }

  final case object ImperialCrown extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Crown, Ruby).some
    val imgSrc: String               = "images/items/ImperialCrown.png"
  }

  final case object ImperialBurgonet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (CloseHelm, Gold).some
    val imgSrc: String               = "images/items/ImperialBurgonet.png"
  }

  final case object LaurelWreath extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Head
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (TrueSamadhiFire, Circlet).some
    val imgSrc: String               = "images/items/LaurelWreath.png"
  }

  final case object Windbreaker extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Windbreaker.png"
  }

  final case object MonksRobe extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/MonksRobe.png"
  }

  final case object Wetsuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Wetsuit.png"
  }

  final case object FabricArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/FabricArmor.png"
  }

  final case object LeatherArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (FabricArmor, Leather).some
    val imgSrc: String               = "images/items/LeatherArmor.png"
  }

  final case object LeatherJacket extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Windbreaker, Leather).some
    val imgSrc: String               = "images/items/LeatherJacket.png"
  }

  final case object TurtleDobok extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (MonksRobe, TurtleShell).some
    val imgSrc: String               = "images/items/TurtleDobok.png"
  }

  final case object MilitarySuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Windbreaker, Branch).some
    val imgSrc: String               = "images/items/MilitarySuit.png"
  }

  final case object Bikini extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Wetsuit, Scissors).some
    val imgSrc: String               = "images/items/Bikini.png"
  }

  final case object PatchedRobe extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (MonksRobe, Bandage).some
    val imgSrc: String               = "images/items/PatchedRobe.png"
  }

  final case object Dress extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Cloth, Scissors).some
    val imgSrc: String               = "images/items/Dress.png"
  }

  final case object DivingSuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Wetsuit, Bandage).some
    val imgSrc: String               = "images/items/DivingSuit.png"
  }

  final case object RiderJacket extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RiderJacket, SteelChain).some
    val imgSrc: String               = "images/items/RiderJacket.png"
  }

  final case object SunsetArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LeatherArmor, Ruby).some
    val imgSrc: String               = "images/items/SunsetArmor.png"
  }

  final case object CovertAgentUniform extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Hanbok, StallionMedal).some
    val imgSrc: String               = "images/items/CovertAgentUniform.png"
  }

  final case object ChainArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LeatherArmor, SteelChain).some
    val imgSrc: String               = "images/items/ChainArmor.png"
  }

  final case object Suit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (MilitarySuit, Ribbon).some
    val imgSrc: String               = "images/items/Suit.png"
  }

  final case object Qipao extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Dress, Razor).some
    val imgSrc: String               = "images/items/Qipao.png"
  }

  final case object SheetMetalArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (FabricArmor, Steel).some
    val imgSrc: String               = "images/items/SheetMetalArmor.png"
  }

  final case object Hanbok extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (PatchedRobe, Flower).some
    val imgSrc: String               = "images/items/Hanbok.png"
  }

  final case object BulletproofVest extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (MilitarySuit, IronSheet).some
    val imgSrc: String               = "images/items/BulletproofVest.png"
  }

  final case object OpticalCamouflageSuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (DivingSuit, GlassPanel).some
    val imgSrc: String               = "images/items/OpticalCamouflageSuit.png"
  }

  final case object EODSuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BulletproofVest, PatchedRobe).some
    val imgSrc: String               = "images/items/EODSuit.png"
  }

  final case object ButlersSuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Suit, FeatherDuster).some
    val imgSrc: String               = "images/items/ButlersSuit.png"
  }

  final case object RockersJacket extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RiderJacket, ClangClatter).some
    val imgSrc: String               = "images/items/RockersJacket.png"
  }

  final case object MithrilArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mithril, LeatherArmor).some
    val imgSrc: String               = "images/items/MithrilArmor.png"
  }

  final case object CrusaderArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SheetMetalArmor, SaintsRelic).some
    val imgSrc: String               = "images/items/CrusaderArmor.png"
  }

  final case object AmazonessArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SheetMetalArmor, Bikini).some
    val imgSrc: String               = "images/items/AmazonessArmor.png"
  }

  final case object DragonDobok extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Qipao, TurtleDobok).some
    val imgSrc: String               = "images/items/DragonDobok.png"
  }

  final case object CommandersArmor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (ChainArmor, Gold).some
    val imgSrc: String               = "images/items/CommandersArmor.png"
  }

  final case object BattleSuit extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (BulletproofVest, DivingSuit).some
    val imgSrc: String               = "images/items/BattleSuit.png"
  }

  final case object Kabana extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (Meteorite, CommandersArmor).some
    val imgSrc: String               = "images/items/Kabana.png"
  }

  final case object QueenOfHearts extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (VFBloodSample, Hanbok).some
    val imgSrc: String               = "images/items/QueenOfHearts.png"
  }

  final case object BlazingDress extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Chest
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (TrueSamadhiFire, Dress).some
    val imgSrc: String               = "images/items/BlazingDress.png"
  }

  final case object Watch extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Watch.png"
  }

  final case object Bandage extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Bandage.png"
  }

  final case object Bracelet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Bracelet.png"
  }

  final case object SquadLeaderArmband extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Bandage, Needle).some
    val imgSrc: String               = "images/items/SquadLeaderArmband.png"
  }

  final case object LeatherShield extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (TurtleShell, Leather).some
    val imgSrc: String               = "images/items/LeatherShield.png"
  }

  final case object Bracer extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Bandage, Leather).some
    val imgSrc: String               = "images/items/Bracer.png"
  }

  final case object Sheath extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Leather, IronSheet).some
    val imgSrc: String               = "images/items/Sheath.png"
  }

  final case object CubeWatch extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Watch, Moonstone).some
    val imgSrc: String               = "images/items/CubeWatch.png"
  }

  final case object GoldenBracelet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Bracelet, Gold).some
    val imgSrc: String               = "images/items/GoldenBracelet.png"
  }

  final case object Bazuband extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Bracer, IronSheet).some
    val imgSrc: String               = "images/items/Bazuband.png"
  }

  final case object CrimsonBracelet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Bracelet, Ruby).some
    val imgSrc: String               = "images/items/CrimsonBracelet.png"
  }

  final case object SteelShield extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (LeatherShield, Steel).some
    val imgSrc: String               = "images/items/SteelShield.png"
  }

  final case object SwordStopper extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Bazuband, Nail).some
    val imgSrc: String               = "images/items/SwordStopper.png"
  }

  final case object MithrilShield extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mithril, LeatherShield).some
    val imgSrc: String               = "images/items/MithrilShield.png"
  }

  final case object VitalSignSensor extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Watch, CellPhone).some
    val imgSrc: String               = "images/items/VitalSignSensor.png"
  }

  final case object Draupnir extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (GoldenBracelet, Bracer).some
    val imgSrc: String               = "images/items/Draupnir.png"
  }

  final case object CreedOfTheKnight extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (SteelShield, SquadLeaderArmband).some
    val imgSrc: String               = "images/items/CreedOfTheKnight.png"
  }

  final case object SwordOfShahJahan extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Sheath, Ruby).some
    val imgSrc: String               = "images/items/SwordOfShahJahan.png"
  }

  final case object BraceletOfSkadi extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (GlacialIce, CrimsonBracelet).some
    val imgSrc: String               = "images/items/BraceletOfSkadi.png"
  }

  final case object AutoArms extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (Bracer, ForceCore).some
    val imgSrc: String               = "images/items/AutoArms.png"
  }

  final case object Radar extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Arm
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (VitalSignSensor, GlassPanel).some
    val imgSrc: String               = "images/items/Radar.png"
  }

  final case object Slippers extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Slippers.png"
  }

  final case object RunningShoes extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/RunningShoes.png"
  }

  final case object Tights extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Tights.png"
  }

  final case object KneePads extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Tights, Leather).some
    val imgSrc: String               = "images/items/KneePads.png"
  }

  final case object ChainLeggings extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Tights, SteelChain).some
    val imgSrc: String               = "images/items/ChainLeggings.png"
  }

  final case object HighHeels extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Slippers, ScrapMetal).some
    val imgSrc: String               = "images/items/HighHeels.png"
  }

  final case object Heelys extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (RunningShoes, IronBall).some
    val imgSrc: String               = "images/items/Heelys.png"
  }

  final case object RepairedSlippers extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Slippers, Cloth).some
    val imgSrc: String               = "images/items/RepairedSlippers.png"
  }

  final case object Boots extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RunningShoes, Oilcloth).some
    val imgSrc: String               = "images/items/Boots.png"
  }

  final case object SteelKneePads extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (KneePads, Steel).some
    val imgSrc: String               = "images/items/SteelKneePads.png"
  }

  final case object StraitjacketSneakers extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (RepairedSlippers, Ash).some
    val imgSrc: String               = "images/items/StraitjacketSneakers.png"
  }

  final case object MaverickRunner extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Heelys, ElectronicParts).some
    val imgSrc: String               = "images/items/MaverickRunner.png"
  }

  final case object CombatBoots extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Boots, Leather).some
    val imgSrc: String               = "images/items/CombatBoots.png"
  }

  final case object KillerHeels extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (HighHeels, GlassPieces).some
    val imgSrc: String               = "images/items/KillerHeels.png"
  }

  final case object FeatherBoots extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (CombatBoots, Feather).some
    val imgSrc: String               = "images/items/FeatherBoots.png"
  }

  final case object MithrilBoots extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mithril, ChainLeggings).some
    val imgSrc: String               = "images/items/MithrilBoots.png"
  }

  final case object Bucephalus extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (KillerHeels, ChainLeggings).some
    val imgSrc: String               = "images/items/Bucephalus.png"
  }

  final case object WhiteRhinos extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (CombatBoots, Nail).some
    val imgSrc: String               = "images/items/WhiteRhinos.png"
  }

  final case object EODBoots extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (Boots, SteelKneePads).some
    val imgSrc: String               = "images/items/EODBoots.png"
  }

  final case object RedShoes extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (KillerHeels, VFBloodSample).some
    val imgSrc: String               = "images/items/RedShoes.png"
  }

  final case object GlacialShoes extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (RepairedSlippers, GlacialIce).some
    val imgSrc: String               = "images/items/GlacialShoes.png"
  }

  final case object BootsOfHermes extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Leg
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, Heelys).some
    val imgSrc: String               = "images/items/BootsOfHermes.png"
  }

  final case object Feather extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Feather.png"
  }

  final case object Flower extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Flower.png"
  }

  final case object Ribbon extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Ribbon.png"
  }

  final case object Fan extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Fan.png"
  }

  final case object BuddhistScripture extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/BuddhistScripture.png"
  }

  final case object Box extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Box.png"
  }

  final case object HolyGrail extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/HolyGrail.png"
  }

  final case object Cross extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Cross.png"
  }

  final case object Binoculars extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Binoculars.png"
  }

  final case object GildedQuillFan extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Fan, Nail).some
    val imgSrc: String               = "images/items/GildedQuillFan.png"
  }

  final case object SaintsRelic extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Cross, HolyGrail).some
    val imgSrc: String               = "images/items/SaintsRelic.png"
  }

  final case object FlowerOfFate extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Flower, PlayingCards).some
    val imgSrc: String               = "images/items/FlowerOfFate.png"
  }

  final case object GlassPieces extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (GlassBottle, Stone).some
    val imgSrc: String               = "images/items/GlassPieces.png"
  }

  final case object Doll extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Ribbon, Cloth).some
    val imgSrc: String               = "images/items/Doll.png"
  }

  final case object SniperScope extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (LaserPointer, Binoculars).some
    val imgSrc: String               = "images/items/SniperScope.png"
  }

  final case object BuddhaSarira extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (BuddhistScripture, MonksRobe).some
    val imgSrc: String               = "images/items/BuddhaSarira.png"
  }

  final case object Quiver extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Leather, Bamboo).some
    val imgSrc: String               = "images/items/Quiver.png"
  }

  final case object FeatherDuster extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ShortRod, Feather).some
    val imgSrc: String               = "images/items/FeatherDuster.png"
  }

  final case object PowderOfLife extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (TreeOfLife, Stone).some
    val imgSrc: String               = "images/items/PowderOfLife.png"
  }

  final case object Uchiwa extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Doll, Fan).some
    val imgSrc: String               = "images/items/Uchiwa.png"
  }

  final case object Magazine extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Box, IronSheet).some
    val imgSrc: String               = "images/items/Magazine.png"
  }

  final case object LacedQuiver extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Quiver, FeatherDuster).some
    val imgSrc: String               = "images/items/LacedQuiver.png"
  }

  final case object MoonlightPendant extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Ribbon, Moonstone).some
    val imgSrc: String               = "images/items/MoonlightPendant.png"
  }

  final case object SchrodingersBox extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Box, Poison).some
    val imgSrc: String               = "images/items/SchrodingersBox.png"
  }

  final case object VeritasLuxMea extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BuddhaSarira, SaintsRelic).some
    val imgSrc: String               = "images/items/VeritasLuxMea.png"
  }

  final case object WhiteCraneFan extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (GildedQuillFan, Feather).some
    val imgSrc: String               = "images/items/WhiteCraneFan.png"
  }

  final case object GlacialIce extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PowderOfLife, Ice).some
    val imgSrc: String               = "images/items/GlacialIce.png"
  }

  final case object TrueSamadhiFire extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PowderOfLife, Lighter).some
    val imgSrc: String               = "images/items/TrueSamadhiFire.png"
  }

  final case object EmeraldTablet extends Item with Armor {
    val armorType: ArmorType         = ArmorType.Accessory
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, FlowerOfFate).some
    val imgSrc: String               = "images/items/EmeraldTablet.png"
  }

  final case object Potato extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Potato.png"
  }

  final case object Cod extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Cod.png"
  }

  final case object Lemon extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Lemon.png"
  }

  final case object Garlic extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Garlic.png"
  }

  final case object AdhesiveBandage extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/AdhesiveBandage.png"
  }

  final case object Carp extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Carp.png"
  }

  final case object Bread extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Bread.png"
  }

  final case object Meat extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Meat.png"
  }

  final case object Egg extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Egg.png"
  }

  final case object Ramen extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Ramen.png"
  }

  final case object OrientalHerb extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/OrientalHerb.png"
  }

  final case object Chocolate extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Chocolate.png"
  }

  final case object CurryPowder extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/CurryPowder.png"
  }

  final case object HoneyCodSteak extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Cod, Honey).some
    val imgSrc: String                 = "images/items/HoneyCodSteak.png"
  }

  final case object CannedCodLiver extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Cod, Can).some
    val imgSrc: String                 = "images/items/CannedCodLiver.png"
  }

  final case object GarlicBread extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Bread, Garlic).some
    val imgSrc: String                 = "images/items/GarlicBread.png"
  }

  final case object Butter extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Milk, Branch).some
    val imgSrc: String                 = "images/items/Butter.png"
  }

  final case object HerbalMedicine extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (OrientalHerb, TurtleShell).some
    val imgSrc: String                 = "images/items/HerbalMedicine.png"
  }

  final case object CarpBread extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Bread, Carp).some
    val imgSrc: String                 = "images/items/CarpBread.png"
  }

  final case object HolyWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/HolyWater.png"
  }

  final case object Disinfectant extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Alcohol, AdhesiveBandage).some
    val imgSrc: String                 = "images/items/Disinfectant.png"
  }

  final case object ChocoPie extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Bread, Chocolate).some
    val imgSrc: String                 = "images/items/ChocoPie.png"
  }

  final case object AcupunctureNeedle extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Alcohol, Needle).some
    val imgSrc: String                 = "images/items/AcupunctureNeedle.png"
  }

  final case object Orchid extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (OrientalHerb, Flower).some
    val imgSrc: String                 = "images/items/Orchid.png"
  }

  final case object TandooriChicken extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (CurryPowder, Meat).some
    val imgSrc: String                 = "images/items/TandooriChicken.png"
  }

  final case object BaconAndGarlicSticks extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Garlic, Meat).some
    val imgSrc: String                 = "images/items/BaconAndGarlicSticks.png"
  }

  final case object Bun extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Bread, Coffee).some
    val imgSrc: String                 = "images/items/Bun.png"
  }

  final case object Hamburger extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Meat, Bread).some
    val imgSrc: String                 = "images/items/Hamburger.png"
  }

  final case object PotatoBread extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Potato, Bread).some
    val imgSrc: String                 = "images/items/PotatoBread.png"
  }

  final case object PotatoSoup extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Potato, Milk).some
    val imgSrc: String                 = "images/items/PotatoSoup.png"
  }

  final case object FishFilletWithEgg extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Cod, Egg).some
    val imgSrc: String                 = "images/items/FishFilletWithEgg.png"
  }

  final case object CitrusCake extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Lemon, Bread).some
    val imgSrc: String                 = "images/items/CitrusCake.png"
  }

  final case object LemonIceCream extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Lemon, Egg).some
    val imgSrc: String                 = "images/items/LemonIceCream.png"
  }

  final case object HoneyGarlicPickle extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Garlic, Honey).some
    val imgSrc: String                 = "images/items/HoneyGarlicPickle.png"
  }

  final case object EggBun extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Egg, Bread).some
    val imgSrc: String                 = "images/items/EggBun.png"
  }

  final case object EasterEgg extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Egg, Chocolate).some
    val imgSrc: String                 = "images/items/EasterEgg.png"
  }

  final case object WhiskeyBonbon extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Whiskey, Chocolate).some
    val imgSrc: String                 = "images/items/WhiskeyBonbon.png"
  }

  final case object ChocoIceCream extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Ice, Chocolate).some
    val imgSrc: String                 = "images/items/ChocoIceCream.png"
  }

  final case object CurryBun extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (CurryPowder, Bread).some
    val imgSrc: String                 = "images/items/CurryBun.png"
  }

  final case object FrenchFries extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Potato, HeatedOil).some
    val imgSrc: String                 = "images/items/FrenchFries.png"
  }

  final case object BakedPotato extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Carp, BoilingWater).some
    val imgSrc: String                 = "images/items/BakedPotato.png"
  }

  final case object BakedCarp extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Carp, HeatedStone).some
    val imgSrc: String                 = "images/items/BakedCarp.png"
  }

  final case object GrilledChileanSeaBass extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Cod, HeatedStone).some
    val imgSrc: String                 = "images/items/GrilledChileanSeaBass.png"
  }

  final case object HotRamen extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Ramen, BoilingWater).some
    val imgSrc: String                 = "images/items/HotRamen.png"
  }

  final case object MochaBread extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Bread, CoffeeLiqueur).some
    val imgSrc: String                 = "images/items/MochaBread.png"
  }

  final case object ScrambledEggs extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Egg, HeatedOil).some
    val imgSrc: String                 = "images/items/ScrambledEggs.png"
  }

  final case object ChocolateChipCookies extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Chocolate, Butter).some
    val imgSrc: String                 = "images/items/ChocolateChipCookies.png"
  }

  final case object ChocoPieBox extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (ChocoPie, Box).some
    val imgSrc: String                 = "images/items/ChocoPieBox.png"
  }

  final case object Curry extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (CurryPowder, BoilingWater).some
    val imgSrc: String                 = "images/items/Curry.png"
  }

  final case object OrientalConcoction extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (OrientalHerb, BoilingWater).some
    val imgSrc: String                 = "images/items/OrientalConcoction.png"
  }

  final case object HoneyButter extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Butter, Honey).some
    val imgSrc: String                 = "images/items/HoneyButter.png"
  }

  final case object FriedChicken extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Meat, HeatedOil).some
    val imgSrc: String                 = "images/items/FriedChicken.png"
  }

  final case object HealingPotion extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Orchid, GlassBottle).some
    val imgSrc: String                 = "images/items/HealingPotion.png"
  }

  final case object BoiledEgg extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Egg, BoilingWater).some
    val imgSrc: String                 = "images/items/BoiledEgg.png"
  }

  final case object PoundCake extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Butter, Bread).some
    val imgSrc: String                 = "images/items/PoundCake.png"
  }

  final case object CurryCroquette extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (CurryPowder, HeatedOil).some
    val imgSrc: String                 = "images/items/CurryCroquette.png"
  }

  final case object Steak extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Meat, HeatedStone).some
    val imgSrc: String                 = "images/items/Steak.png"
  }

  final case object FirstAidKit extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Disinfectant, Bandage).some
    val imgSrc: String                 = "images/items/FirstAidKit.png"
  }

  final case object ButterFriedPotatoes extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Potato, Butter).some
    val imgSrc: String                 = "images/items/ButterFriedPotatoes.png"
  }

  final case object FishCutlet extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Cod, HeatedOil).some
    val imgSrc: String                 = "images/items/FishCutlet.png"
  }

  final case object StirFriedRamen extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Ramen, HeatedOil).some
    val imgSrc: String                 = "images/items/StirFriedRamen.png"
  }

  final case object ColdNoodles extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Ramen, IceWater).some
    val imgSrc: String                 = "images/items/ColdNoodles.png"
  }

  final case object ZenVitality extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (AcupunctureNeedle, HerbalMedicine).some
    val imgSrc: String                 = "images/items/ZenVitality.png"
  }

  final case object GarlicRamen extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (HotRamen, Garlic).some
    val imgSrc: String                 = "images/items/GarlicRamen.png"
  }

  final case object SpicyFishStew extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Carp, BoilingWater).some
    val imgSrc: String                 = "images/items/SpicyFishStew.png"
  }

  final case object FishAndChips extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Food
    val rarity: ItemRarity             = ItemRarity.Epic
    val recipe: Option[(Item, Item)]   = (FishCutlet, FrenchFries).some
    val imgSrc: String                 = "images/items/FishAndChips.png"
  }

  final case object Honey extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Honey.png"
  }

  final case object Water extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Water.png"
  }

  final case object Ice extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Ice.png"
  }

  final case object Whiskey extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Whiskey.png"
  }

  final case object Coffee extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Coffee.png"
  }

  final case object CarbonatedWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/CarbonatedWater.png"
  }

  final case object Milk extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Common
    val recipe: Option[(Item, Item)]   = None
    val imgSrc: String                 = "images/items/Milk.png"
  }

  final case object BoilingWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Water, Lighter).some
    val imgSrc: String                 = "images/items/BoilingWater.png"
  }

  final case object Lemonade extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (CarbonatedWater, Lemon).some
    val imgSrc: String                 = "images/items/Lemonade.png"
  }

  final case object WaterBottle extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Water, GlassBottle).some
    val imgSrc: String                 = "images/items/WaterBottle.png"
  }

  final case object Baijiu extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Alcohol, Lighter).some
    val imgSrc: String                 = "images/items/Baijiu.png"
  }

  final case object Soju extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Alcohol, Water).some
    val imgSrc: String                 = "images/items/Soju.png"
  }

  final case object IceCoffee extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Ice, Coffee).some
    val imgSrc: String                 = "images/items/IceCoffee.png"
  }

  final case object Cocktail extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Whiskey, Lemon).some
    val imgSrc: String                 = "images/items/Cocktail.png"
  }

  final case object CoffeeLiqueur extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Coffee, Alcohol).some
    val imgSrc: String                 = "images/items/CoffeeLiqueur.png"
  }

  final case object Cola extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (CarbonatedWater, Honey).some
    val imgSrc: String                 = "images/items/Cola.png"
  }

  final case object Latte extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Milk, Coffee).some
    val imgSrc: String                 = "images/items/Latte.png"
  }

  final case object HoneyMilk extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Milk, Honey).some
    val imgSrc: String                 = "images/items/HoneyMilk.png"
  }

  final case object Highball extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Whiskey, CarbonatedWater).some
    val imgSrc: String                 = "images/items/Highball.png"
  }

  final case object ChocolateMilk extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Chocolate, Milk).some
    val imgSrc: String                 = "images/items/ChocolateMilk.png"
  }

  final case object HoneyWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Honey, Water).some
    val imgSrc: String                 = "images/items/HoneyWater.png"
  }

  final case object IceWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Ice, Water).some
    val imgSrc: String                 = "images/items/IceWater.png"
  }

  final case object OnTheRocks extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Ice, Whiskey).some
    val imgSrc: String                 = "images/items/OnTheRocks.png"
  }

  final case object Cowboy extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)]   = (Milk, Whiskey).some
    val imgSrc: String                 = "images/items/Cowboy.png"
  }

  final case object KaoliangLiquor extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Baijiu, Lighter).some
    val imgSrc: String                 = "images/items/KaoliangLiquor.png"
  }

  final case object HotHoneyWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (BoilingWater, Honey).some
    val imgSrc: String                 = "images/items/HotHoneyWater.png"
  }

  final case object FlowerLiquor extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Baijiu, Flower).some
    val imgSrc: String                 = "images/items/FlowerLiquor.png"
  }

  final case object Americano extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (BoilingWater, Coffee).some
    val imgSrc: String                 = "images/items/Americano.png"
  }

  final case object HerbalLiquor extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Baijiu, OrientalHerb).some
    val imgSrc: String                 = "images/items/HerbalLiquor.png"
  }

  final case object WhiskeyCocktail extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Cola, Whiskey).some
    val imgSrc: String                 = "images/items/WhiskeyCocktail.png"
  }

  final case object PurifiedWater extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (BoilingWater, Ice).some
    val imgSrc: String                 = "images/items/PurifiedWater.png"
  }

  final case object CanOfCola extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (Cola, Can).some
    val imgSrc: String                 = "images/items/CanOfCola.png"
  }

  final case object HotChocolate extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (BoilingWater, Chocolate).some
    val imgSrc: String                 = "images/items/HotChocolate.png"
  }

  final case object WhiteRussian extends Item with Consumable {
    val consumableType: ConsumableType = ConsumableType.Beverage
    val rarity: ItemRarity             = ItemRarity.Rare
    val recipe: Option[(Item, Item)]   = (CoffeeLiqueur, Milk).some
    val imgSrc: String                 = "images/items/WhiteRussian.png"
  }

  final case object Snare extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Snare.png"
  }

  final case object Mousetrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Mousetrap.png"
  }

  final case object PianoWire extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/PianoWire.png"
  }

  final case object SpikedPlank extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Mousetrap, Nail).some
    val imgSrc: String               = "images/items/SpikedPlank.png"
  }

  final case object EnhancedMousetrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Mousetrap, IronOre).some
    val imgSrc: String               = "images/items/EnhancedMousetrap.png"
  }

  final case object Dynamite extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (PianoWire, Gunpowder).some
    val imgSrc: String               = "images/items/Dynamite.png"
  }

  final case object BambooTrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Snare, Bamboo).some
    val imgSrc: String               = "images/items/BambooTrap.png"
  }

  final case object BoobyTrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Snare, Glue).some
    val imgSrc: String               = "images/items/BoobyTrap.png"
  }

  final case object ClangClatter extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Can, IronBall).some
    val imgSrc: String               = "images/items/ClangClatter.png"
  }

  final case object JungleGuillotine extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Mousetrap, KitchenKnife).some
    val imgSrc: String               = "images/items/JungleGuillotine.png"
  }

  final case object ExplosiveTrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Mousetrap, Gunpowder).some
    val imgSrc: String               = "images/items/ExplosiveTrap.png"
  }

  final case object PendulumAxe extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BambooTrap, Hatchet).some
    val imgSrc: String               = "images/items/PendulumAxe.png"
  }

  final case object Mine extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (BoobyTrap, Gunpowder).some
    val imgSrc: String               = "images/items/Mine.png"
  }

  final case object RDX extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Dynamite, ScrapMetal).some
    val imgSrc: String               = "images/items/RDX.png"
  }

  final case object MithrilString extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Mithril, PianoWire).some
    val imgSrc: String               = "images/items/MithrilString.png"
  }

  final case object HiddenMaiden extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (SpikedPlank, JungleGuillotine).some
    val imgSrc: String               = "images/items/HiddenMaiden.png"
  }

  final case object FireTrap extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ExplosiveTrap, Oilcloth).some
    val imgSrc: String               = "images/items/FireTrap.png"
  }

  final case object C4 extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RDX, WhitePowder).some
    val imgSrc: String               = "images/items/C4.png"
  }

  final case object DoubleGuillotine extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (JungleGuillotine, PendulumAxe).some
    val imgSrc: String               = "images/items/DoubleGuillotine.png"
  }

  final case object Claymore extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (Mine, ExplosiveTrap).some
    val imgSrc: String               = "images/items/Claymore.png"
  }

  final case object Stingburst extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (RDX, SpikedPlank).some
    val imgSrc: String               = "images/items/Stingburst.png"
  }

  final case object RemoteMine extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (ForceCore, SpikedPlank).some
    val imgSrc: String               = "images/items/RemoteMine.png"
  }

  final case object SmartBomb extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Legendary
    val recipe: Option[(Item, Item)] = (RDX, CellPhone).some
    val imgSrc: String               = "images/items/SmartBomb.png"
  }

  final case object SurveillanceCamera extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/SurveillanceCamera.png"
  }

  final case object TelephotoCamera extends Item with Special {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = (SurveillanceCamera, Binoculars).some
    val imgSrc: String               = "images/items/TelephotoCamera.png"
  }

  final case object Stone extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Stone.png"
  }

  final case object GlassBottle extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/GlassBottle.png"
  }

  final case object Nail extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Nail.png"
  }

  final case object Leather extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Leather.png"
  }

  final case object TurtleShell extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/TurtleShell.png"
  }

  final case object Rubber extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Rubber.png"
  }

  final case object ScrapMetal extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/ScrapMetal.png"
  }

  final case object Lighter extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Lighter.png"
  }

  final case object LaserPointer extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/LaserPointer.png"
  }

  final case object StallionMedal extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/StallionMedal.png"
  }

  final case object Battery extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Battery.png"
  }

  final case object Alcohol extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Alcohol.png"
  }

  final case object Oil extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Oil.png"
  }

  final case object Cloth extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Cloth.png"
  }

  final case object Gemstone extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Gemstone.png"
  }

  final case object Glue extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Glue.png"
  }

  final case object Paper extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Paper.png"
  }

  final case object IronOre extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/IronOre.png"
  }

  final case object Can extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Can.png"
  }

  final case object Gunpowder extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Common
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Gunpowder.png"
  }

  final case object Steel extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ScrapMetal, IronOre).some
    val imgSrc: String               = "images/items/Steel.png"
  }

  final case object Oilcloth extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Oil, Bandage).some
    val imgSrc: String               = "images/items/Oilcloth.png"
  }

  final case object HeatedOil extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Oil, Lighter).some
    val imgSrc: String               = "images/items/HeatedOil.png"
  }

  final case object Ruby extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Hammer, Gemstone).some
    val imgSrc: String               = "images/items/Ruby.png"
  }

  final case object DeadBattery extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Battery, Water).some
    val imgSrc: String               = "images/items/DeadBattery.png"
  }

  final case object WhitePowder extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Chalk, Stone).some
    val imgSrc: String               = "images/items/WhitePowder.png"
  }

  final case object HeatedStone extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Stone, Lighter).some
    val imgSrc: String               = "images/items/HeatedStone.png"
  }

  final case object Meteorite extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Meteorite.png"
  }

  final case object Ash extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Paper, Lighter).some
    val imgSrc: String               = "images/items/Ash.png"
  }

  final case object ElectronicParts extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Battery, PianoWire).some
    val imgSrc: String               = "images/items/ElectronicParts.png"
  }

  final case object Blueprint extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (FountainPen, Paper).some
    val imgSrc: String               = "images/items/Blueprint.png"
  }

  final case object IronSheet extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (ScrapMetal, Hammer).some
    val imgSrc: String               = "images/items/IronSheet.png"
  }

  final case object Gold extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Uncommon
    val recipe: Option[(Item, Item)] = (Pickaxe, Gemstone).some
    val imgSrc: String               = "images/items/Gold.png"
  }

  final case object TreeOfLife extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/TreeOfLife.png"
  }

  final case object Moonstone extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Meteorite, Stone).some
    val imgSrc: String               = "images/items/Moonstone.png"
  }

  final case object Poison extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Ash, Water).some
    val imgSrc: String               = "images/items/Poison.png"
  }

  final case object Motor extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (ElectronicParts, ScrapMetal).some
    val imgSrc: String               = "images/items/Motor.png"
  }

  final case object Mithril extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/Mithril.png"
  }

  final case object GlassPanel extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (GlassPieces, Glue).some
    val imgSrc: String               = "images/items/GlassPanel.png"
  }

  final case object IonBattery extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (DeadBattery, CarbonatedWater).some
    val imgSrc: String               = "images/items/IonBattery.png"
  }

  final case object VFBloodSample extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = None
    val imgSrc: String               = "images/items/VFBloodSample.png"
  }

  final case object CellPhone extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Rare
    val recipe: Option[(Item, Item)] = (Blueprint, ElectronicParts).some
    val imgSrc: String               = "images/items/CellPhone.png"
  }

  final case object ForceCore extends Item with Material {
    val rarity: ItemRarity           = ItemRarity.Epic
    val recipe: Option[(Item, Item)] = (PowderOfLife, Meteorite).some
    val imgSrc: String               = "images/items/ForceCore.png"
  }

  val values: IndexedSeq[Item] = findValues
}
