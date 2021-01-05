package maf.core

import maf.util.SmartHash

trait Environment[A <: Address] extends SmartHash {
  /** Restrict the environment to only certain keys */
  def restrictTo(keys: Set[String]): Environment[A]
  /** Looks up a value in the environment */
  def lookup(name: String): Option[A]
  /** Extend the environment */
  def extend(name: String, a: A): Environment[A]
  def extend(values: Iterable[(String, A)]): Environment[A]
  /** Mapping over the environment */
  def mapAddrs(f: A => A): Environment[A]
}

/** Mapping from variable name to addresses */
case class BasicEnvironment[A <: Address](content: Map[String,A]) extends Environment[A] {
  def restrictTo(keys: Set[String]): Environment[A]         = this.copy(content = content.view.filterKeys(keys).toMap)
  def lookup(name: String): Option[A]                       = content.get(name)
  def extend(name: String, a: A): Environment[A]            = this.copy(content = content + (name -> a))
  def extend(values: Iterable[(String, A)]): Environment[A] = this.copy(content = content ++ values)
  def mapAddrs(f: A => A): Environment[A]                   = this.copy(content.view.mapValues(f).toMap)
  /** Better printing. */
  override def toString: String = s"ENV{${content.filter(_._2.printable).mkString(", ")}}"
}

object Environment {
  def apply[A <: Address](bds: Iterable[(String,A)]): Environment[A] = BasicEnvironment(bds.toMap)
}
