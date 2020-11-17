package maf.modular.contracts

import maf.core.{Environment, Lattice}
import maf.language.contracts.{ScExp, ScIdentifier, ScNil}

/**
  * This trait provides a monad that aids with defining a big step semantics for soft contract verification
  */
trait ScSemanticsMonad extends ScModSemantics {
  type PC = ScExp
  type PostValue = (Value, ScExp)
  type StoreCache = Map[Addr, PostValue]
  case class Context(env: Environment[Addr], pc: PC, cache: StoreCache) {
    def store: Store = cache.view.mapValues(_._1).toMap
  }

  def value(v: Value): PostValue = (v, ScNil())

  object ScEvalM {
    def pure[X](v: => X): ScEvalM[X] = ScEvalM((context) => List((context, v)).toSet)
    def unit: ScEvalM[()] = pure(())
    def void[X]: ScEvalM[X] = ScEvalM((context) => Set[(Context, X)]())

    case class ScEvalM[X](run: Context => Set[(Context, X)]) {
      def map[Y](f: X => Y): ScEvalM[Y] = ScEvalM((context) => {
        run(context).map {
          case (updatedContext, value) => (updatedContext, f(value))
        }
      })

      def flatMap[Y](f: X => ScEvalM[Y]): ScEvalM[Y] = ScEvalM((context) =>
        run(context).flatMap {
          case (updatedContext, value) => f(value).run(updatedContext)
        }
      )
    }

    def sequence[X](xs: List[ScEvalM[X]]): ScEvalM[List[X]] = xs match {
      case List() => pure(List())
      case _ =>
        for {
          result <- xs.head
          results <- sequence(xs.tail)
        } yield (result :: results)
    }

    def sequenceLast[X](xs: List[ScEvalM[X]]): ScEvalM[X] =
      sequence(xs).map(_.last)

    def withEnv[B](f: Environment[Addr] => ScEvalM[B]): ScEvalM[B] =
      ScEvalM((context) => f(context.env).run(context))

    def lookup(identifier: String): ScEvalM[Addr] = withEnv((env) => {
      pure(env.lookup(identifier).getOrElse(throw new Exception(s"variable ${identifier} not found")))
    })

    def lookupOrDefine(identifier: ScIdentifier, component: Component): ScEvalM[Addr] = withEnv((env) => {
      pure(env.lookup(identifier.name).getOrElse {
        val addr = allocVar(identifier, context(component))
        addr
      })
    })

    def nondet[X](t: ScEvalM[X], f: ScEvalM[X]): ScEvalM[X] = ScEvalM((context) => {
      val resF = f.run(context)
      val resT = t.run(context)
      resF ++ resT
    })

    def nondets[X](s: Set[ScEvalM[X]]): ScEvalM[X] = ScEvalM((context) => {
      s.flatMap(_.run(context))
    })

    def withPc[X](f: PC => X): ScEvalM[X] = ScEvalM((context) => {
      Set((context, f(context.pc)))
    })

    def withStoreCache[X](f: StoreCache => ScEvalM[X]): ScEvalM[X] = ScEvalM((context) => {
      f(context.cache).run(context)
    })

    def withContext[X](f: Context => ScEvalM[X]): ScEvalM[X] = ScEvalM((context) => f(context).run(context))

    def addToCache(mapping: (Addr, PostValue)): ScEvalM[()] = ScEvalM((context) => {
      List((context.copy(cache = context.cache + mapping), ())).toSet
    })

    def joinInCache(addr: Addr, value: PostValue): ScEvalM[()] = ScEvalM((context) => {
      Set((
        (context.copy(
          cache = context.cache.updated(
            addr, (lattice.join(context.store.getOrElse(addr, lattice.bottom), value._1), value._2)))), ()
      ))
    })

    def replacePc[X](pc: PC)(c: ScEvalM[X]): ScEvalM[X] = ScEvalM((context) => {
      c.run(context.copy(pc = pc))
    })

    /**
      * This function creates a computation that yields a single state contain the abstract value with no
      * symbolic information.
      */
    def result(v: Value): ScEvalM[PostValue] = pure(value(v))

    def extended[X](ident: ScIdentifier, component: Component)(c: Addr => ScEvalM[X]): ScEvalM[X] = ScEvalM((ctx) => {
      val addr = allocVar(ident, context(component))
      val extendedEnv = ctx.env.extend(ident.name, addr)
      c(addr).run(ctx.copy(env = extendedEnv)).map {
        case (updatedContext, value) => (updatedContext.copy(env = ctx.env), value)
      }
    })

    def addBindingToEnv(ident: ScIdentifier, component: Component): ScEvalM[()] = ScEvalM((ctx) => {
      val addr = allocVar(ident, context(component))
      Set((ctx.copy(env = ctx.env.extend(ident.name, addr)), ()))
    })

    /**
      * Given a computation that yields a value corresponding to a certain lattice, this function runs the computation
      * on the given context, and joins all the values of the resulting states together using the join operator of the
      * lattice.
      */
    def merged[L: Lattice](c: ScEvalM[L])(context: Context): (L, Store) = {
      import maf.lattice.MapLattice._
      c.run(context).foldLeft((Lattice[L].bottom, Lattice[Store].bottom))((acc, v) => v match {
        case (context, l) => (Lattice[L].join(acc._1, l), Lattice[Store].join(acc._2, context.store))
      })
    }

    /**
      * Given a computation that yields states that contain sets of values, this operator yields a single computation
      * that gives rises to a state for every element in the given set.
      */
    def options[X](c: ScEvalM[Set[X]]): ScEvalM[X] = ScEvalM((context) =>
      c.run(context).flatMap {
        case (updatedContext, set) => set.map((updatedContext, _))
      }
    )

    def debug(c: => ()): ScEvalM[()] = unit.flatMap(_ => {
      c
      pure(())
    })

    /**
      * Executes the given action simply for its side effects
      */
    def effectful(c: => ()): ScEvalM[()] = debug(c)

    def trace[X]: (X => ScEvalM[X]) = { x =>
      println(("trace", x))
      pure(x)
    }

    def evict(addresses: List[Addr]): ScEvalM[()] = ScEvalM(context => {
      Set((context.copy(cache = context.cache.removedAll(addresses)), ()))
    })

    def mergeStores(calleeStore: Store): ScEvalM[()] =
      sequence(calleeStore.view.map {
        case (addr, v)  => joinInCache(addr, value(v))
      }.toList).flatMap(_ => unit)

    def getStore: ScEvalM[Store] = ScEvalM(context => {
      Set((context, context.store))
    })
  }

}
