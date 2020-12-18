package io._2ne1ugly

import cats.implicits._
import com.raquo.airstream.signal.Signal
import com.raquo.laminar.api.L._
import io._2ne1ugly.components.ItemSlot
import io._2ne1ugly.erbs.data.{Accessory, Arm, Chest, EquipType, Head, Item, Leg, Weapon}

object App {

  val weaponVar: Var[Option[Weapon]]       = Var(None)
  val headVar: Var[Option[Head]]           = Var(None)
  val chestVar: Var[Option[Chest]]         = Var(None)
  val armVar: Var[Option[Arm]]             = Var(None)
  val legVar: Var[Option[Leg]]             = Var(None)
  val accessoryVar: Var[Option[Accessory]] = Var(None)
  val extraVar: Var[List[Item]]            = Var(Nil)

  val itemFilterVar: Var[List[Item] => List[Item]] = Var(_ => List())

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
      children <-- itemFilterVar.signal.map(_(Item.values.toList)).split(_.getClass.getSimpleName) { (key, item, itemSig) =>
        ItemSlot(item.asRight)
      }
    )

  def renderEquipSetup =
    div(
      cls := "grid grid-cols-3 grid-rows-2 gap-x-2 gap-y-1",
      child <-- weaponVar.signal.map(item =>
        ItemSlot(item.toRight(EquipType.Weapon))
          .amend(onClick.mapTo((items: List[Item]) => items.collect { case item: Weapon => item }) --> itemFilterVar)
      ),
      child <-- headVar.signal.map(item =>
        ItemSlot(item.toRight(EquipType.Head))
          .amend(onClick.mapTo((items: List[Item]) => items.collect { case item: Head => item }) --> itemFilterVar)
      ),
      child <-- chestVar.signal.map(item =>
        ItemSlot(item.toRight(EquipType.Chest))
          .amend(onClick.mapTo((items: List[Item]) => items.collect { case item: Chest => item }) --> itemFilterVar)
      ),
      child <-- armVar.signal.map(item =>
        ItemSlot(item.toRight(EquipType.Arm))
          .amend(onClick.mapTo((items: List[Item]) => items.collect { case item: Arm => item }) --> itemFilterVar)
      ),
      child <-- legVar.signal.map(item =>
        ItemSlot(item.toRight(EquipType.Leg))
          .amend(onClick.mapTo((items: List[Item]) => items.collect { case item: Leg => item }) --> itemFilterVar)
      ),
      child <-- accessoryVar.signal.map(item =>
        ItemSlot(item.toRight(EquipType.Accessory))
          .amend(onClick.mapTo((items: List[Item]) => items.collect { case item: Accessory => item }) --> itemFilterVar)
      )
    )
}
