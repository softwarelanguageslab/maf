package maf.util

import maf.util.SmartHash
import cats.Show

sealed trait Environment[A <: Address] extends SmartHash { outer =>
    type This >: this.type <: Environment[A] { type This = outer.This }

    /** Type (hack) to make environments from a subtype to environment of a supertype */
    def as[A1 >: A <: Address]: Environment[A1]

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

object Environment:
    given [A <: Address]: Show[Environment[A]] with
        def show(e: Environment[A]): String = ???

    def empty[A <: Address]: Environment[A] = BasicEnvironment(Map.empty)
    def apply[A <: Address](bds: Iterable[(String, A)]): Environment[A] =
        BasicEnvironment(bds.toMap)

/** Mapping from variable name to addresses */
case class BasicEnvironment[A <: Address](content: Map[String, A]) extends Environment[A]:
    type This = BasicEnvironment[A]
    def as[A1 >: A <: Address]: BasicEnvironment[A1] = BasicEnvironment(content.map { case (key, value) => (key) -> (value: A1) })
    def restrictTo(keys: Set[String]): BasicEnvironment[A] =
        this.copy(content = content.view.filterKeys(keys).toMap)
    def lookup(name: String): Option[A] = content.get(name)
    def extend(name: String, a: A): BasicEnvironment[A] =
        this.copy(content = content + (name -> a))
    def extend(values: Iterable[(String, A)]): BasicEnvironment[A] =
        this.copy(content = content ++ values)
    def map[B <: Address](f: (String, A) => (String, B)): BasicEnvironment[B] =
        this.copy(content = content.map(f.tupled))
    def mapAddrs(f: A => A): BasicEnvironment[A] =
        this.copy(content.view.mapValues(f).toMap)
    def size: Int = content.size
    def addrs = content.values.toSet
    def toList = content.toList

    /** Better printing. */
    override def toString: String =
        s"ENV{${content.filter(_._2.printable).mkString(", ")}}"
//override def toString: String = "ENV"

case class WrappedEnv[A <: Address, D](env: Environment[A], depth: Int, data: D) extends Environment[A]:
    type This = WrappedEnv[A, D]

    def as[A1 >: A <: Address]: WrappedEnv[A1, D] =
        WrappedEnv(env.as[A1], depth, data)

    def restrictTo(keys: Set[String]): WrappedEnv[A, D] =
        this.copy(env = env.restrictTo(keys))
    def lookup(name: String): Option[A] = env.lookup(name)
    def extend(name: String, a: A): WrappedEnv[A, D] =
        this.copy(env = env.extend(name, a))
    def extend(values: Iterable[(String, A)]): WrappedEnv[A, D] =
        this.copy(env = env.extend(values))
    def mapAddrs(f: A => A): WrappedEnv[A, D] = this.copy(env = env.mapAddrs(f))
    def addrs = env.addrs
    def toList = env.toList
    def size: Int = env.size

case class NestedEnv[A <: Address, E <: Address](
    content: Map[String, A],
    rst: Option[E])
    extends Environment[A]:
    type This = NestedEnv[A, E]

    def as[A1 >: A <: Address]: NestedEnv[A1, E] =
        NestedEnv(content.map { case (k, v) => k -> (v: A1) }, rst)

    /** Restrict the environment to only certain keys */
    def restrictTo(keys: Set[String]): This =
        this.copy(content = content.view.filterKeys(keys).toMap)

    /** Looks up a value in the environment */
    def lookup(name: String): Option[A] = content.get(name)

    /** Extend the environment */
    def extend(name: String, a: A): This =
        this.copy(content = content + (name -> a))
    def extend(values: Iterable[(String, A)]): This =
        this.copy(content = content ++ values)

    /** Mapping over the environment */
    def mapAddrs(f: A => A): This =
        this.copy(content = content.view.mapValues(f).toMap)
    def addrs = rst match
        case Some(addr) => content.values.toSet + addr
        case None       => content.values.toSet
    def size: Int = content.size
    def toList = throw new Exception("NYI -- NestedEnv.toList")