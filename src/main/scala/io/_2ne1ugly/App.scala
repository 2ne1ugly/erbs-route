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
        Item.values.sortBy(_.getClass.getSimpleName).map(ItemThumbnail.apply),
        div(
          "RIGHT"
        )
      )
    )
}
