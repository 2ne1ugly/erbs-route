package io._2ne1ugly

import com.raquo.laminar.api.L._
import components._
import io._2ne1ugly.erbs.data.Item

object App {
  def apply() =
    div(
      cls := "flex h-screen",
      div(
        cls := "m-auto flex",
        renderItemGrid(Item.values.toList)
      ),
      div(
        cls := "hover:text-white",
        "HMM"
      )
    )

  def renderItemGrid(items: List[Item]) =
    div(
      cls := "grid grid-cols-3 gap-x-3 gap-y-2",
      items.sortBy(_.rarity).map { item =>
        ItemThumbnail(item)
      }
    )
}
