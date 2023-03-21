package maf.util

sealed trait HList
case object HNil extends HList
case class ::[H, T](head: H, tail: HList) extends HList
