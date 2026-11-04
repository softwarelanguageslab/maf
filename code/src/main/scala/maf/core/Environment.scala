package maf.core

import maf.util.SmartHash

sealed trait Environment[A <: Address] extends SmartHash { outer =>
    type This >: this.type <: Environment[A] { type This = outer.This }

    /** Restrict the environment to only certain keys */
    def restrictTo(keys: Set[String]): This

    /** Looks up a value in the environment */
    def lookup(name: String): Option[A]
    def apply(name: String): A = lookup(name).get

    /** Extend the environment */
    def extend(name: String, a: A): This
    def extend(values: Iterable[(String, A)]): This

    /** Mapping over the environment */
    def mapAddrs(f: A => A): This

    def addrs: Set[Address]

    def toList: List[(String, Address)]

    def size: Int
}

/** Mapping from variable name to addresses */
case class BasicEnvironment[A <: Address](content: Map[String, A]) extends Environment[A]:
    type This = BasicEnvironment[A]
    def restrictTo(keys: Set[String]): BasicEnvironment[A] = this.copy(content = content.view.filterKeys(keys).toMap)
    def lookup(name: String): Option[A] = content.get(name)
    def extend(name: String, a: A): BasicEnvironment[A] = this.copy(content = content + (name -> a))
    def extend(values: Iterable[(String, A)]): BasicEnvironment[A] = this.copy(content = content ++ values)
    def map[B <: Address](f: (String, A) => (String, B)): BasicEnvironment[B] = this.copy(content = content.map(f.tupled))
    def mapAddrs(f: A => A): BasicEnvironment[A] = this.copy(content.view.mapValues(f).toMap)
    def size: Int = content.size
    def addrs = content.values.toSet
    def toList = content.toList
    /** Better printing. */
    override def toString: String = s"ENV{${content.filter(_._2.printable).mkString(", ")}}"
//override def toString: String = "ENV"

object Environment:
    def empty[A <: Address]: Environment[A] = BasicEnvironment(Map.empty)
    def apply[A <: Address](bds: Iterable[(String, A)]): Environment[A] = BasicEnvironment(bds.toMap)

