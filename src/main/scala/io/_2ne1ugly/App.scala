package io._2ne1ugly

import cats.implicits._
import com.raquo.airstream.signal.Signal
import com.raquo.laminar.api.L._
import io._2ne1ugly.components.ItemSlot
import io._2ne1ugly.erbs.data.{Armor, ArmorType, EquipType, Item, Weapon, WeaponType}

object App {

  val weaponTypeVar: Var[WeaponType]  = Var(WeaponType.Throw)
  val weaponVar: Var[Option[Item]]    = Var(None)
  val headVar: Var[Option[Item]]      = Var(None)
  val chestVar: Var[Option[Item]]     = Var(None)
  val armVar: Var[Option[Item]]       = Var(None)
  val legVar: Var[Option[Item]]       = Var(None)
  val accessoryVar: Var[Option[Item]] = Var(None)
  val extraVar: Var[List[Item]]       = Var(Nil)

  val itemFilterVar: Var[Item => Boolean] = Var(_ => false)

  def apply() =
    div(
      cls := "flex h-screen",
      div(
        cls := "grid grid-row-1",
        div(
          cls := "grid grid-col-1",
          renderEquipSetup,
          renderItemGrid
        )
      ),
      div(
        cls := "hover:text-white",
        "HMM"
      )
    )

  def renderItemGrid =
    div(
      cls := "overflow-y-scroll h-screen grid grid-cols-3 gap-x-3 gap-y-2 content-start",
      children <-- itemFilterVar.signal.map(Item.values.toList.filter).split(_.getClass.getSimpleName) { (key, item, itemSig) =>
        ItemSlot(item.asRight)
      }
    )

  def renderEquipSetup =
    div(
      cls := "grid grid-cols-3 grid-rows-2 gap-x-2 gap-y-1",
      child <-- weaponVar.signal.map(renderEquipSetupWeaponSlot(_, weaponTypeVar.now())),
      child <-- headVar.signal.map(renderEquipSetupArmorSlot(_, ArmorType.Head)),
      child <-- chestVar.signal.map(renderEquipSetupArmorSlot(_, ArmorType.Chest)),
      child <-- armVar.signal.map(renderEquipSetupArmorSlot(_, ArmorType.Arm)),
      child <-- legVar.signal.map(renderEquipSetupArmorSlot(_, ArmorType.Leg)),
      child <-- accessoryVar.signal.map(renderEquipSetupArmorSlot(_, ArmorType.Accessory))
    )

  def renderEquipSetupWeaponSlot(item: Option[Item], equipType: EquipType) =
    ItemSlot(item.toRight(equipType))
      .amend(
        onClick.mapTo((item: Item) =>
          item match {
            case weapon: Item with Weapon => weapon.weaponType == equipType
            case _                        => false
          }
        ) --> itemFilterVar.writer
      )
  def renderEquipSetupArmorSlot(item: Option[Item], equipType: EquipType)  =
    ItemSlot(item.toRight(equipType))
      .amend(
        onClick.mapTo((item: Item) =>
          item match {
            case armor: Item with Armor => armor.armorType == equipType
            case _                      => false
          }
        ) --> itemFilterVar.writer
      )
}
