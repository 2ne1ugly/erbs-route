package io._2ne1ugly

import com.raquo.laminar.api.L._

import scala.reflect.ClassTag

package object implicits {

  implicit class listOpts[A](list: List[A]) {
    def collectT[T: ClassTag]: List[T] =
      list.collect { case t: T => t }
  }
}
