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

  def size: Int
}

/** Mapping from variable name to addresses */
case class BasicEnvironment[A <: Address](content: Map[String, A]) extends Environment[A]:
    type This = BasicEnvironment[A]
    def restrictTo(keys: Set[String]): BasicEnvironment[A] = this.copy(content = content.view.filterKeys(keys).toMap)
    def lookup(name: String): Option[A] = content.get(name)
    def extend(name: String, a: A): BasicEnvironment[A] = this.copy(content = content + (name -> a))
    def extend(values: Iterable[(String, A)]): BasicEnvironment[A] = this.copy(content = content ++ values)
    def mapAddrs(f: A => A): BasicEnvironment[A] = this.copy(content.view.mapValues(f).toMap)
    def size: Int = content.size
    def addrs = content.values.toSet

    /** Better printing. */
    override def toString: String = s"ENV{${content.filter(_._2.printable).mkString(", ")}}"
//override def toString: String = "ENV"

case class WrappedEnv[A <: Address, D](
    env: Environment[A],
    depth: Int,
    data: D)
    extends Environment[A]:
    type This = WrappedEnv[A, D]
    def restrictTo(keys: Set[String]): WrappedEnv[A, D] = this.copy(env = env.restrictTo(keys))
    def lookup(name: String): Option[A] = env.lookup(name)
    def extend(name: String, a: A): WrappedEnv[A, D] = this.copy(env = env.extend(name, a))
    def extend(values: Iterable[(String, A)]): WrappedEnv[A, D] = this.copy(env = env.extend(values))
    def mapAddrs(f: A => A): WrappedEnv[A, D] = this.copy(env = env.mapAddrs(f))
    def addrs = env.addrs
    def size: Int = env.size

object Environment:
    def empty[A <: Address]: Environment[A] = BasicEnvironment(Map.empty)
    def apply[A <: Address](bds: Iterable[(String, A)]): Environment[A] = BasicEnvironment(bds.toMap)

case class NestedEnv[A <: Address, E <: Address](content: Map[String, A], rst: Option[E]) extends Environment[A]:
    type This = NestedEnv[A, E]

    /** Restrict the environment to only certain keys */
    def restrictTo(keys: Set[String]): This = this.copy(content = content.view.filterKeys(keys).toMap)

    /** Looks up a value in the environment */
    def lookup(name: String): Option[A] = content.get(name)

    /** Extend the environment */
    def extend(name: String, a: A): This = this.copy(content = content + (name -> a))
    def extend(values: Iterable[(String, A)]): This = this.copy(content = content ++ values)

    /** Mapping over the environment */
    def mapAddrs(f: A => A): This = this.copy(content = content.view.mapValues(f).toMap)
    def addrs = rst match
        case Some(addr) => content.values.toSet + addr
        case None       => content.values.toSet
    def size: Int = content.size
