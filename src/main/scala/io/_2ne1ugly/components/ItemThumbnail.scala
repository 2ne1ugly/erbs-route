package io._2ne1ugly.components

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io._2ne1ugly.erbs.data.{Item, ItemRarity}
import org.scalajs.dom.{document, html}

object ItemThumbnail {

  def apply(item: Item) = {
    val showTooltipVar: Var[Boolean]            = Var(false)
    val mousePositionVar: Var[(Double, Double)] = Var((0, 0))
    div(
      onMouseMove.map {
        val documentElement = document.documentElement
        event => (event.screenX + documentElement.scrollLeft, event.clientY + documentElement.scrollTop)
      } --> mousePositionVar.writer,
      onMouseEnter.mapTo(true) --> showTooltipVar.writer,
      onMouseLeave.mapTo(false) --> showTooltipVar.writer,
      renderImg(item),
      div(
        cls.toggle("invisible") <-- showTooltipVar.signal.map(!_),
        position := "absolute",
        left <-- mousePositionVar.signal.map(_._1.toString + "px"),
        top <-- mousePositionVar.signal.map(coordinate => (coordinate._2 + 20).toString + "px"),
        renderTooltip(item)
      )
    )
  }

  def renderImg(item: Item) =
    img(
      cls := "shadow-inner w-20 bg-gradient-to-t border-2",
      cls := (item.rarity match {
        case ItemRarity.Common    => "from-gray-600 to-gray-900 border-gray-600"
        case ItemRarity.Uncommon  => "from-green-600 to-green-900 border-green-600"
        case ItemRarity.Rare      => "from-blue-600 to-blue-900 border-blue-600"
        case ItemRarity.Epic      => "from-purple-600 to-purple-900 border-purple-600"
        case ItemRarity.Legendary => "from-yellow-600 to-yellow-900 border-yellow-600"
      }),
      src := item.imgSrc
    )

  def renderTooltip(item: Item): Mod[ReactiveHtmlElement[html.Div]] =
    div(
      cls := "grid grid-cols-1 bg-gray-1000",
      div(
        cls := "flex bg-gray-700 p-1 m-1 items-start",
        renderImg(item),
        div(
          cls := "grid grid-cols-1 mx-1",
          div(
            cls := (item.rarity match {
              case ItemRarity.Common    => "text-gray-400"
              case ItemRarity.Uncommon  => "text-green-400"
              case ItemRarity.Rare      => "text-blue-400"
              case ItemRarity.Epic      => "text-purple-400"
              case ItemRarity.Legendary => "text-yellow-400"
            }),
            item.getClass.getSimpleName
          ),
          div(
            cls := "text-white",
            item.getDesc
          )
        )
      )
    )
}
