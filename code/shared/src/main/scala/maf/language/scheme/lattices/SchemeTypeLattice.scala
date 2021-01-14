package maf.language.scheme.lattices

import maf.core._
import maf.language.CScheme.TID
import maf.language.scheme.primitives._
import maf.language.scheme._
import maf.lattice.interfaces.BoolLattice
import maf.util._

class TypeSchemeLattice[A <: Address, K] {
  type P = SchemePrimitive[L, A]

  case class L(
      str: Boolean = false,
      bool: Boolean = false,
      num: Boolean = false,
      char: Boolean = false,
      sym: Boolean = false,
      nil: Boolean = false,
      inputPort: Boolean = false,
      outputPort: Boolean = false,
      prims: Set[P] = Set.empty,
      clos: Set[((SchemeLambdaExp, schemeLattice.Env), Option[String])] = Set.empty,
      ptrs: Set[A] = Set.empty,
      consCells: (L, L) = (L(), L()))
      extends SmartHash {
    def isBottom: Boolean =
      !str && !bool && !num && !char && !sym && !nil && prims.isEmpty && clos.isEmpty && consCells._1.isBottom && consCells._2.isBottom
  }
  object Inject {
    val bottom: L = L()
    val str: L = L(str = true)
    val bool: L = L(bool = true)
    val num: L = L(num = true)
    val char: L = L(char = true)
    val sym: L = L(sym = true)
    val nil: L = L(nil = true)
    val inputPort: L = L(inputPort = true)
    val outputPort: L = L(outputPort = true)
    def prim(p: P): L = L(prims = Set(p))
    def pointer(a: A): L = L(ptrs = Set(a))
    def clo(clo: schemeLattice.Closure, name: Option[String]): L = L(clos = Set((clo, name)))
    def cons(car: L, cdr: L): L = L(consCells = (car, cdr))
  }

  def check(b: Boolean, v: L)(name: String, args: List[L]): MayFail[L, Error] =
    if (b) { MayFail.success(v) }
    else { MayFail.failure(OperatorNotApplicable(name, args)) }

  val schemeLattice: SchemeLattice[L, A, P] = new SchemeLattice[L, A, P] {
    def show(x: L): String = s"$x"
    def isTrue(x: L): Boolean = true // only "false" is not true, but we only have Bool represented
    def isFalse(x: L): Boolean = x.bool
    def op(op: SchemeOp)(args: List[L]): MayFail[L, Error] = {
      import SchemeOp._
      op.checkArity(args)
      if (args.exists(_.isBottom)) { MayFail.success(bottom) }
      else {
        op match {
          case Car        => MayFail.success(args(0).consCells._1)
          case Cdr        => MayFail.success(args(0).consCells._2)
          case MakeVector => throw new Exception("NYI: vectors in type lattice")
          case VectorRef  => throw new Exception("NYI: vectors in type lattice")
          case VectorSet  => throw new Exception("NYI: vectors in type lattice")
          case IsNull | IsCons | IsPointer | IsChar | IsSymbol | IsInteger | IsString | IsReal | IsBoolean | IsVector | IsThread | IsLock |
              IsProcedure | IsInputPort | IsOutputPort | Not =>
            // Any -> Bool
            MayFail.success(Inject.bool)
          case Ceiling | Floor | Round | Log | Random | Sin | Cos | ACos | Tan | ATan | Sqrt | ExactToInexact | InexactToExact =>
            // Num -> Num
            check(args(0).num, Inject.num)(op.name, args)
          case VectorLength =>
            // Vector -> Num
            throw new Exception("NYI: vectors in type lattice")
          case StringLength =>
            // String -> Num
            check(args(0).str, Inject.str)(op.name, args)
          case NumberToString =>
            // Number -> String
            check(args(0).num, Inject.str)(op.name, args)
          case SymbolToString =>
            // Symbol -> String
            check(args(0).sym, Inject.str)(op.name, args)
          case StringToSymbol =>
            // String -> Symbol
            check(args(0).str, Inject.sym)(op.name, args)
          case StringToNumber =>
            // String -> Num
            check(args(0).str, Inject.num)(op.name, args)
          case IntegerToCharacter =>
            // Num -> Char
            check(args(0).num, Inject.char)(op.name, args)
          case CharacterToInteger =>
            // Char -> Num
            check(args(0).char, Inject.num)(op.name, args)
          case CharacterToString =>
            // Char -> String
            check(args(0).char, Inject.str)(op.name, args)
          case CharacterDowncase | CharacterUpcase =>
            // Char -> Char
            check(args(0).char, Inject.char)(op.name, args)
          case CharacterIsLower | CharacterIsUpper =>
            // Char -> Bool
            check(args(0).char, Inject.bool)(op.name, args)
          case MakeInputPort =>
            // String -> InputPort
            check(args(0).str, Inject.inputPort)(op.name, args)
          case MakeOutputPort =>
            // String -> OutputPort
            check(args(0).str, Inject.outputPort)(op.name, args)
          case Plus | Minus | Times | Quotient | Div | Expt | Modulo | Remainder =>
            // Num -> Num -> Num
            check(args(0).num && args(1).num, Inject.num)(op.name, args)
          case Lt | NumEq =>
            // Num -> Num -> Bool
            check(args(0).num && args(1).num, Inject.num)(op.name, args)
          case Eq =>
            // Any -> Any -> Bool
            MayFail.success(Inject.bool)
          case StringAppend =>
            // Str -> Str -> Str
            check(args(0).str && args(1).str, Inject.str)(op.name, args)
          case StringRef =>
            // Str -> Num -> Char
            check(args(0).str && args(1).num, Inject.char)(op.name, args)
          case StringLt =>
            // Str -> Str -> Bool
            check(args(0).str && args(1).str, Inject.bool)(op.name, args)
          case CharacterEq | CharacterLt | CharacterEqCI | CharacterLtCI =>
            // Char -> Char -> Bool
            check(args(0).char && args(1).char, Inject.bool)(op.name, args)
          case MakeString =>
            // Int -> Char -> Bool
            check(args(0).num && args(1).char, Inject.str)(op.name, args)
          case Substring =>
            // Str -> Int -> Int -> Str
            check(args(0).str && args(1).num && args(2).num, Inject.str)(op.name, args)
        }
      }
    }
    def join(x: L, y: => L): L =
      L(
        str = x.str || y.str,
        bool = x.bool || y.bool,
        num = x.num || y.num,
        char = x.char || y.char,
        sym = x.sym || y.sym,
        nil = x.nil || y.nil,
        inputPort = x.inputPort || y.inputPort,
        outputPort = x.outputPort || y.outputPort,
        prims = x.prims.union(y.prims),
        clos = x.clos.union(y.clos),
        ptrs = x.ptrs.union(y.ptrs),
        consCells = (join(x.consCells._1, y.consCells._1), join(x.consCells._2, y.consCells._2))
      )
    def subsumes(x: L, y: => L): Boolean =
      (if (x.str) y.str else true) &&
        (if (x.bool) y.bool else true) &&
        (if (x.num) y.num else true) &&
        (if (x.char) y.char else true) &&
        (if (x.sym) y.sym else true) &&
        (if (x.nil) y.nil else true) &&
        (if (x.inputPort) y.inputPort else true) &&
        (if (x.outputPort) y.outputPort else true) &&
        y.prims.subsetOf(x.prims) &&
        y.clos.subsetOf(y.clos) &&
        subsumes(x.consCells._1, y.consCells._1) &&
        subsumes(x.consCells._1, y.consCells._2)
    def top: L = ???
    def getClosures(x: L): Set[(Closure, Option[String])] = x.clos
    def getPrimitives(x: L): Set[P] = x.prims
    def getPointerAddresses(x: L): Set[A] = Set()
    def getThreads(x: L): Set[TID] = throw new Exception("Not supported.")
    def getContinuations(x: L): Set[K] = ???
    def bottom: L = Inject.bottom
    def number(x: scala.Int): L = Inject.num
    def numTop: L = Inject.num
    def charTop: L = Inject.char
    def stringTop: L = Inject.str
    def realTop: L = Inject.num
    def symbolTop: L = Inject.sym
    def real(x: Double): L = Inject.num
    def string(x: String): L = Inject.str
    def bool(x: Boolean): L = Inject.bool
    def char(x: scala.Char): L = Inject.char
    def primitive(x: P): L = Inject.prim(x)
    def closure(x: schemeLattice.Closure, name: Option[String]): L = Inject.clo(x, name)
    def symbol(x: String): L = Inject.sym
    def nil: L = Inject.nil
    def cons(car: L, cdr: L): L = Inject.cons(car, cdr)
    def pointer(a: A): L = Inject.pointer(a)
    def eql[B: BoolLattice](x: L, y: L): B = BoolLattice[B].top /* could be refined in some cases */
    def thread(tid: TID): L = ???
    def cont(k: K): L = ???
    def lock(threads: Set[TID]) = ???
    def void: L = ???
    def acquire(lock: L, caller: TID): MayFail[L, Error] = ???
    def release(lock: L, caller: TID): MayFail[L, Error] = ???

  }
  object L {
    implicit val lattice: SchemeLattice[L, A, P] = schemeLattice
  }

  object Primitives extends SchemeLatticePrimitives[L, A] with PrimitiveBuildingBlocks[L, A] {
    override def allPrimitives = super.allPrimitives ++ List(
      `abs`,
      // `assoc`, // TODO
      // `assq`, // TODO
      // `assv`, // TODO
      `display`,
      `equal?`,
      `eqv?`,
      `even?`,
      `gcd`,
      `lcm`,
      `length`,
      // `list-ref`, // TODO
      // `list->vector`, // TODO? or not
      // `list-tail`, // TODO
      `list?`,
      // `member`, // TODO
      // `memq`, // TODO
      // `memv`, // TODO
      `negative?`,
      `newline`,
      `not`,
      `odd?`,
      `positive?`,
      `zero?`,
      `<=`,
      `>`,
      `>=`,
      `caar`,
      `cadr`,
      `cdar`,
      `cddr`,
      `caddr`,
      `cdddr`,
      `caadr`,
      `cdadr`,
      `cadddr`
      // TODO: other cxr
      // `vector->list // TODO
      // We decided not to implement some primitives as they can't be properly supported in the framework: reverse, map, for-each, apply
    )
    class SimplePrim(val name: String, ret: L) extends SchemePrimitive[L, A] {
      def call(
          fexp: SchemeExp,
          args: List[(SchemeExp, L)],
          store: Store[A, L],
          alloc: SchemeInterpreterBridge[L, A]
        ): MayFail[(L, Store[A, L]), Error] =
        MayFail.success((ret, store))
    }
    object `abs` extends SimplePrim("abs", Inject.num)
    object `display` extends SimplePrim("display", Inject.str) // undefined behavior in R5RS
    object `equal?` extends SimplePrim("equal?", Inject.bool)
    object `eqv?` extends SimplePrim("eqv?", Inject.bool)
    object `even?` extends SimplePrim("even?", Inject.bool)
    object `gcd` extends SimplePrim("gcd", Inject.num)
    object `lcm` extends SimplePrim("lcm", Inject.num)
    object `length` extends SimplePrim("length", Inject.num)
    object `list?` extends SimplePrim("list?", Inject.bool)
    object `negative?` extends SimplePrim("negative?", Inject.bool)
    object `newline` extends SimplePrim("newline", Inject.bool)
    object `not` extends SimplePrim("not", Inject.bool)
    object `odd?` extends SimplePrim("odd?", Inject.bool)
    object `positive?` extends SimplePrim("positive?", Inject.bool)
    object `zero?` extends SimplePrim("zero?", Inject.bool)
    object `<=` extends SimplePrim("<=", Inject.bool)
    object `>` extends SimplePrim(">", Inject.bool)
    object `>=` extends SimplePrim(">=", Inject.bool)
    object `caar`
        extends Store1Operation("caar",
                                (x, store) =>
                                  dereferencePointer(x, store) { c1 =>
                                    L.lattice.car(c1) >>= { car =>
                                      dereferencePointer(car, store) { c2 =>
                                        L.lattice.car(c2)
                                      }
                                    }
                                  }.map((_, store))
        )
    object `cadr`
        extends Store1Operation("cadr",
                                (x, store) =>
                                  dereferencePointer(x, store) { c1 =>
                                    L.lattice.cdr(c1) >>= { cdr =>
                                      dereferencePointer(cdr, store) { c2 =>
                                        L.lattice.car(c2)
                                      }
                                    }
                                  }.map((_, store))
        )
    object `cdar`
        extends Store1Operation("cdar",
                                (x, store) =>
                                  dereferencePointer(x, store) { c1 =>
                                    L.lattice.car(c1) >>= { car =>
                                      dereferencePointer(car, store) { c2 =>
                                        L.lattice.cdr(c2)
                                      }
                                    }
                                  }.map((_, store))
        )
    object `cddr`
        extends Store1Operation("cddr",
                                (x, store) =>
                                  dereferencePointer(x, store) { c1 =>
                                    L.lattice.cdr(c1) >>= { cdr =>
                                      dereferencePointer(cdr, store) { c2 =>
                                        L.lattice.cdr(c2)
                                      }
                                    }
                                  }.map((_, store))
        )
    object `caddr`
        extends Store1Operation("caddr",
                                (x, store) =>
                                  dereferencePointer(x, store) { c1 =>
                                    L.lattice.cdr(c1) >>= { cdr =>
                                      dereferencePointer(cdr, store) { c2 =>
                                        L.lattice.cdr(c2) >>= { cddr =>
                                          dereferencePointer(cddr, store) { c3 =>
                                            L.lattice.car(c3)
                                          }
                                        }
                                      }
                                    }
                                  }.map((_, store))
        )
    object `caadr`
        extends Store1Operation("caadr",
                                (x, store) =>
                                  dereferencePointer(x, store) { c1 =>
                                    L.lattice.cdr(c1) >>= { cdr =>
                                      dereferencePointer(cdr, store) { c2 =>
                                        L.lattice.car(c2) >>= { cadr =>
                                          dereferencePointer(cadr, store) { c3 =>
                                            L.lattice.car(c3)
                                          }
                                        }
                                      }
                                    }
                                  }.map((_, store))
        )
    object `cdadr`
        extends Store1Operation("cdadr",
                                (x, store) =>
                                  dereferencePointer(x, store) { c1 =>
                                    L.lattice.cdr(c1) >>= { cdr =>
                                      dereferencePointer(cdr, store) { c2 =>
                                        L.lattice.car(c2) >>= { cadr =>
                                          dereferencePointer(cadr, store) { c3 =>
                                            L.lattice.cdr(c3)
                                          }
                                        }
                                      }
                                    }
                                  }.map((_, store))
        )
    object `cdddr`
        extends Store1Operation("cdddr",
                                (x, store) =>
                                  dereferencePointer(x, store) { c1 =>
                                    L.lattice.cdr(c1) >>= { cdr =>
                                      dereferencePointer(cdr, store) { c2 =>
                                        L.lattice.cdr(c2) >>= { cddr =>
                                          dereferencePointer(cddr, store) { c3 =>
                                            L.lattice.cdr(c3)
                                          }
                                        }
                                      }
                                    }
                                  }.map((_, store))
        )
    object `cadddr`
        extends Store1Operation("cadddr",
                                (x, store) =>
                                  dereferencePointer(x, store) { c1 =>
                                    L.lattice.cdr(c1) >>= { cdr =>
                                      dereferencePointer(cdr, store) { c2 =>
                                        L.lattice.cdr(c2) >>= { cddr =>
                                          dereferencePointer(cddr, store) { c3 =>
                                            L.lattice.cdr(c3) >>= { cdddr =>
                                              dereferencePointer(cdddr, store) { c4 =>
                                                L.lattice.cdr(c4)
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }.map((_, store))
        )
  }
}
