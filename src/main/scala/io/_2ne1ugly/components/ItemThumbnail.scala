package io._2ne1ugly.components

import com.raquo.laminar.api.L._
import io._2ne1ugly.erbs.data.Item

object ItemThumbnail {

  def apply(item: Item) = img(
    src := item.imgSrc
  )
}
