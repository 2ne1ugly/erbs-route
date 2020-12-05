package io._2ne1ugly

import com.raquo.laminar.api.L._
import org.scalajs.dom.document

import scala.scalajs.js.annotation.JSExportTopLevel

object Main {

  @JSExportTopLevel("main")
  def main(): Unit = {
    val container = document.getElementById("app-container")
    render(container, div("LOLEL"))
  }
}
