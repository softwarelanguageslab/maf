package maf.language.scheme

import maf.core.{Identifier, Identity, Position}
import maf.language.sexp._
import maf.util.Reader

object SchemeLoader:

    def load(file: String, e: SExp): List[SExp] =
        val prefix = file.split("/").nn.dropRight(1).mkString("") + "/"
        load(e, prefix, List(file)) match
            case Left(e)   => List(e)
            case Right(es) => es

    /** Loads the definitions as the given place in the AST. This does not create additional environments (such as begin etc.). */
    def load(
        exp: SExp,
        prefix: String,
        loaded: List[String]
      ): Either[SExp, List[SExp]] = exp match
        case id @ SExpId(_) => Left(id)
        case SExpPair(SExpId(Identifier("load", _)), SExpPair(SExpValue(Value.String(file), idn), SExpValue(Value.Nil, _), _), _) =>
          if loaded.contains(file) then throw new Exception(s"Circular references: ${loaded.mkString(" ")}")
          val parsed = SExpParser.parse(Reader.loadFile(prefix + file), Position.newTag(s"[${file} ${idn.pos}]"))
          val exps = parsed.foldRight(List[SExp]()) { case (exp, acc) =>
            load(exp, prefix, file :: loaded) match
                case Left(e)   => e :: acc
                case Right(es) => es ::: acc
          }
          exps match
              case e :: Nil => Left(e)
              case _        => Right(exps)
        case exp @ SExpPair(SExpId(Identifier("load", _)), _, _) =>
          throw new Exception(s"Invalid Scheme load: $exp (${exp.idn}).")
        case SExpPair(car, cdr, idn) =>
          val carl = load(car, prefix, loaded)
          val cdrl = load(cdr, prefix, loaded)
          (carl, cdrl) match
              case (Left(car), Left(cdr))   => Left(SExpPair(car, cdr, idn))
              case (Left(car), Right(cdr))  => Left(SExpPair(car, SExpList(cdr, SExpValue(Value.Nil, Identity.none)), idn))
              case (Right(car), Left(cdr))  => Left(SExpList(car, cdr))
              case (Right(car), Right(cdr)) => Left(SExpList(car, SExpList(cdr, SExpValue(Value.Nil, Identity.none))))
        case value @ SExpValue(_, _) => Left(value)

    def SExpAppend(car: SExp, cdr: SExp): SExp = car match
        case id @ SExpId(_)           => SExpPair(id, cdr, Identity.none)
        case SExpPair(car, cadr, idn) => SExpPair(car, SExpAppend(cadr, cdr), idn)
        case value @ SExpValue(_, _)  => SExpPair(value, cdr, Identity.none)
