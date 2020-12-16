package io._2ne1ugly

import com.raquo.laminar.api.L._

package object implicits {
  implicit class SignalOptionOpt[A](signal: Signal[Option[A]]) {}
}
