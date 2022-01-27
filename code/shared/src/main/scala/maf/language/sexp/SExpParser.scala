package maf.language.sexp

import maf.core.Position._
import maf.core._
import maf.lattice._

/**
 * Implementation of a simple s-expression parser, which supports some Scheme-like constructs. It however doesn't fully support any RnRS standard
 * syntax.
 */
/**
 * NOTE: How the lexer/parser works and how to debug it
 *
 * The SExpTokens trait defines the tokens of the language. The chars field of each token is the textual representation of the token.
 *
 * The SExpLexer class defines a bunch of lexers. Some of them are helper lexers, used in other ones. Lexers of type Parser[Token] will lex one of our
 * token. All these lexers are then assembled into the 'token' function, which will parse either one of them, ignoring whitespace and comments.
 *
 * To test a lexer, one just has to apply it, providing a Reader[Char] as argument. For example, to test the character lexer: val lexical = new
 * SExpLexer println(lexical.character(new scala.util.parsing.input.CharArrayReader("#\c".toCharArray))
 *
 * The SExpParser class defines parsers, similarly as SExpLexer. The difference is that the parser works by assembling a bunch of tokens into grammar
 * items, whereas the lexer works by assembling a bunch of characters to tokens.
 *
 * To test a parser, similarly to the lexers, one just has to apply it, providing a Reader[Token] as argument (given by the Scanner class of the
 * lexer). For example, to test the 'nil' parser: val lexical = new SExpLexer println(SExpParser.nil(new SExpParser.lexical.Scanner("()"))
 *
 * You may ask why is SExpLexer defined as a class, and SExpParser as an object. The answer is simple: I don't know, but that's apparently the idiom
 * to use. SExpParser's lexical variable *needs* to be set to the lexer used. Also, having SExpLexer as a separate class seems the only way to be able
 * to import it and test it outside this file.
 */
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical.TokenParsers

trait SExpTokens extends Tokens:
    trait SExpToken extends Token with Positional
    case class TIdentifier(s: String) extends SExpToken:
        def chars = s
    case class TString(s: String) extends SExpToken:
        def chars = s""""$s""""

    case class TInteger(n: BigInt) extends SExpToken:
        def chars = n.toString

    case class TReal(n: Double) extends SExpToken:
        def chars = n.toString
    case class TBoolean(b: Boolean) extends SExpToken:
        def chars =
          if b then "#t"
          else "#f"
    case class TCharacter(c: Char) extends SExpToken:
        def chars = s"#\\$c"
    case class TQuote() extends SExpToken:
        def chars = "'"
    case class TLeftParen() extends SExpToken:
        def chars = "("
    case class TLeftBracket() extends SExpToken:
        def chars = "["
    case class TRightParen() extends SExpToken:
        def chars = ")"
    case class TRightBracket() extends SExpToken:
        def chars = "]"
    case class THashParen() extends SExpToken:
        def chars = "#("
    case class TBackquote() extends SExpToken:
        def chars = "`"
    case class TUnquote() extends SExpToken:
        def chars = ","
    case class TUnquoteSplicing() extends SExpToken:
        def chars = ",@"
    case class TDot() extends SExpToken:
        def chars = "."

class SExpLexer extends Lexical with SExpTokens:
    def whitespace: Parser[String] = rep(whitespaceChar) ^^ (_.mkString)
    def eoi: Parser[Any] = new Parser[Any] {
      def apply(in: Input) =
        if in.atEnd then new Success("EOI", in)
        else Failure("End of Input expected", in)
    }
    def eol: Parser[Any] = acceptIf(n => n == '\n')(n => "")
    def notEol: Parser[Char] = acceptIf(n => n != '\n')(n => "")
    def comment: Parser[String] = ';' ~> rep(notEol) ^^ (_.mkString)
    def nonRelevant: Parser[Unit] = rep(comment | whitespaceChar | eol) ^^ (_ => ())

    def any: Parser[Char] = chrExcept()
    def chr(c: Char): Parser[Char] = elem(s"character $c", _ == c)
    def sign: Parser[Option[Char]] = opt(chr('+') | chr('-'))
    def stringContentNoEscape: Parser[String] =
      rep(chrExcept('\\', '"')) ^^ (_.mkString)
    def stringContent: Parser[String] =
      (stringContentNoEscape ~ '\\' ~ any ~ stringContent ^^ { case s1 ~ '\\' ~ c ~ s2 =>
        s"$s1\\$c$s2"
      }) |
        stringContentNoEscape

    // R5RS: Tokens which require implicit termination (identifiers, numbers, characters, and dot) may be terminated by any <delimiter>, but not necessarily by anything else.  */
    def delimiter: Parser[Unit] =
      (whitespaceChar | eol | eoi | chr('(') | chr('[') | chr(')') | chr(']') | chr('"') | chr(';')) ^^ (_ => ())

    def boolean: Parser[SExpToken] =
      '#' ~> ('t' ^^^ TBoolean(true) | 'T' ^^^ TBoolean(true) |
        'f' ^^^ TBoolean(false) | 'F' ^^^ TBoolean(false))
    def integer: Parser[SExpToken] =
      sign ~ rep1(digit) <~ guard(delimiter) ^^ { case s ~ n =>
        s match
            case Some(_) => TInteger(NumOps.bigIntFromString((s ++ n).mkString).get) // Take into account an explicit sign annotation.
            case None    => TInteger(NumOps.bigIntFromString(n.mkString).get) // No explicit sign annotation.
      }
    def character: Parser[SExpToken] =
      (('#' ~> '\\' ~> 's' ~> 'p' ~> 'a' ~> 'c' ~> 'e' ^^^ TCharacter(' ')) |
        ('#' ~> '\\' ~> 'n' ~> 'e' ~> 'w' ~> 'l' ~> 'i' ~> 'n' ~> 'e' ^^^ TCharacter('\n')) |
        ('#' ~> '\\' ~> 't' ~> 'a' ~> 'b' ^^^ TCharacter('\t')) | // Not present in R5RS Scheme.
        ('#' ~> '\\' ~> any ^^ (c => TCharacter(c))))
    def string: Parser[SExpToken] =
      ('"' ~> stringContent ~ chrExcept('\\') <~ '"' ^^ { case s ~ ending => TString(s + ending) }) |
        ('"' ~> stringContent <~ '"' ^^ (s => TString(s)))
    def identifier: Parser[SExpToken] =
        def specialInitial: Parser[Char] =
          (chr('!') | chr('$') | chr('%') | chr('&') | chr('*') | chr('/') | chr(':') | chr('<') | chr(
            '='
          ) | chr('>') | chr('?') | chr('^') | chr('_') | chr('~') | chr('@')) ^^ (x => x)
        // def initial: Parser[Char] = letter | specialInitial
        def initial: Parser[Char] = not(chr('@') | chr('.') | chr('#') | digit | whitespaceChar | delimiter | quotes | chr('"')) ~> chrExcept()
        def specialSubsequent: Parser[Char] = chr('+') | chr('-') | chr('.') | chr('@')
        //def subsequent: Parser[Char] = initial | digit | specialSubsequent
        def subsequent: Parser[Char] = not(whitespaceChar | delimiter | quotes | chr('"') | chr('\'')) ~> chrExcept()
        def peculiarIdentifier: Parser[String] = { in =>
          // R5RS specifies + | - | ..., not clear what ... is supposed to be
          // so let's be very flexible with this definition
          (rep1(subsequent) andThen {
            case Success(List('.'), _) => Failure("not a valid identifier: .", in)
            case Success(List('@'), _) => Failure("not a valid identifier: @", in)
            case Success(cs, next)     => Success(cs.mkString(""), next)
            case Failure(msg, next)    => Failure(msg, next)
            case e: Error              => e
          })(in)
        }
        (initial ~ rep(subsequent) ^^ { case i ~ s => s"$i${s.mkString}" }
          | peculiarIdentifier) <~ guard(delimiter) ^^ (s => TIdentifier(s))
    def leftParen: Parser[SExpToken] = chr('(') ^^^ TLeftParen()
    def leftBracket: Parser[SExpToken] = chr('[') ^^^ TLeftBracket()
    def rightParen: Parser[SExpToken] = chr(')') ^^^ TRightParen()
    def rightBracket: Parser[SExpToken] = chr(']') ^^^ TRightBracket()
    def hashParen: Parser[SExpToken] = chr('#') ~ chr('(') ^^^ THashParen()
    def quote: Parser[SExpToken] = chr('\'') ^^^ TQuote()
    def backquote: Parser[SExpToken] = chr('`') ^^^ TBackquote()
    def unquote: Parser[SExpToken] = chr(',') ^^^ TUnquote()
    def unquoteSplicing: Parser[SExpToken] = chr(',') ~ chr('@') ^^^ TUnquoteSplicing()
    def quotes: Parser[SExpToken] = quote | backquote | unquote | unquoteSplicing
    def dot: Parser[SExpToken] = chr('.') <~ guard(delimiter) ^^^ TDot()
    def real: Parser[SExpToken] =
      sign ~ rep(digit) ~ opt('.' ~ rep1(digit)) ~ opt('e' ~ integer) <~ guard(delimiter) ^? {
        case s ~ pre ~ post ~ exp if (exp.isDefined || post.isDefined) && (pre.nonEmpty || post.isDefined) =>
          val signstr = s.map(_.toString).getOrElse("")
          val poststr = post.map({ case _ ~ digits => s".${digits.mkString}" }).getOrElse("")
          val expstr = exp
            .map({
              case _ ~ TInteger(n) => s"e$n"
              case _               => throw new Exception(s"cannot parse real ($exp)")
            })
            .getOrElse("")
          val n = s"$signstr${pre.mkString}$poststr$expstr"
          TReal(n.toDouble)
      }
    def number: Parser[SExpToken] = real | integer
    def token: Parser[SExpToken] =
      nonRelevant ~> positioned({
        boolean | number |
          character | string |
          leftParen | leftBracket | rightParen | rightBracket |
          hashParen | quote | backquote | unquoteSplicing | unquote | dot | identifier
      }) <~ nonRelevant

object SExpParser extends TokenParsers:
    type Tokens = SExpTokens
    override val lexical: SExpLexer = new SExpLexer
    import lexical._

    def bool: Parser[Value] = elem("boolean", _.isInstanceOf[TBoolean]) ^^ { case TBoolean(b) =>
      Value.Boolean(b)
    }
    def integer: Parser[Value] = elem("integer", _.isInstanceOf[TInteger]) ^^ { case TInteger(n) =>
      Value.Integer(n)
    }
    def real: Parser[Value] = elem("real", _.isInstanceOf[TReal]) ^^ { case TReal(n) =>
      Value.Real(n)
    }
    def character: Parser[Value] = elem("character", _.isInstanceOf[TCharacter]) ^^ { case TCharacter(c) =>
      Value.Character(c)
    }
    def string: Parser[Value] = elem("string", _.isInstanceOf[TString]) ^^ { case TString(s) =>
      Value.String(s)
    }
    def nil: Parser[Value] = ((leftParen ~ rightParen) | (leftBracket ~ rightBracket)) ^^^ Value.Nil

    def value(tag: PTag): Parser[SExp] = Parser { in =>
      (bool | real | integer | character | string | nil)(in) match
          case Success(t, in1) => Success(SExpValue(t, Identity(in.pos, tag)), in1)
          case ns: NoSuccess   => ns
    }

    def identifier(tag: PTag): Parser[SExp] = Parser { in =>
      elem("identifier", _.isInstanceOf[TIdentifier])(in) match
          case Success(TIdentifier(s), in1) => Success(SExpId(Identifier(s, Identity(in.pos, tag))), in1)
          case Success(v, in1)              => Failure(s"Expected identifier, got $v", in1)
          case ns: NoSuccess                => ns
    }

    def leftParen = elem("left parenthesis", _.isInstanceOf[TLeftParen])
    def leftBracket = elem("left bracket", _.isInstanceOf[TLeftBracket])
    def rightParen = elem("right parenthesis", _.isInstanceOf[TRightParen])
    def rightBracket = elem("right bracket", _.isInstanceOf[TRightBracket])
    def dot = elem("dot", _.isInstanceOf[TDot])
    def quote = elem("quote", _.isInstanceOf[TQuote])
    def quasiquote = elem("quasiquote", _.isInstanceOf[TBackquote])
    def unquote = elem("unquote", _.isInstanceOf[TUnquote])
    def unquoteSplicing = elem("unquote-splicing", _.isInstanceOf[TUnquoteSplicing])

    def parenList(tag: PTag): Parser[SExp] = Parser { in =>
      (leftParen ~> rep1(exp(tag)) ~ opt(dot ~> exp(tag)) <~ rightParen)(in) match
          case Success(es ~ None, in1) =>
            Success(SExpList(es, Identity(in.pos, tag)), in1)
          case Success(es ~ Some(tail), in1) =>
            Success(SExpList(es, tail), in1)
          case ns: NoSuccess => ns
    }

    def bracketList(tag: PTag): Parser[SExp] = Parser { in =>
      (leftBracket ~> rep1(exp(tag)) ~ opt(dot ~> exp(tag)) <~ rightBracket)(in) match
          case Success(es ~ None, in1) =>
            Success(SExpList(es, Identity(in.pos, tag)), in1)
          case Success(es ~ Some(tail), in1) =>
            Success(SExpList(es, tail), in1)
          case ns: NoSuccess => ns
    }

    def list(tag: PTag): Parser[SExp] = parenList(tag) | bracketList(tag)

    def withQuote(tag: PTag)(p: Parser[_], make: (SExp, Identity) => SExp) = Parser { in =>
      (p ~> exp(tag))(in) match
          case Success(e, in1) => Success(make(e, Identity(in.pos, tag)), in1)
          case ns: NoSuccess   => ns
    }

    def quoted(tag: PTag): Parser[SExp] = withQuote(tag)(quote, SExpQuoted(_, _))
    def quasiquoted(tag: PTag): Parser[SExp] = withQuote(tag)(quasiquote, SExpQuasiquoted(_, _))
    def unquoted(tag: PTag): Parser[SExp] = withQuote(tag)(unquote, SExpUnquoted(_, _))
    def unquotedSplicing(tag: PTag): Parser[SExp] = withQuote(tag)(unquoteSplicing, SExpUnquotedSplicing(_, _))

    def exp(tag: PTag): Parser[SExp] =
      value(tag) | identifier(tag) | list(tag) | quoted(tag) | quasiquoted(tag) | unquoted(tag) | unquotedSplicing(tag)
    def expList(tag: PTag): Parser[List[SExp]] = rep1(exp(tag))

    def parse(s: String, tag: PTag = noTag): List[SExp] = expList(tag)(new Scanner(s)) match
        case Success(res, next) if next.atEnd =>
          res
        case Success(res, next) =>
          println(s"token list $res")
          throw new Exception(
            s"cannot fully parse expression, stopped at ${next.pos} after parsing $res"
          )
        case Failure(msg, next) =>
          println(s"msg: $msg $next")
          throw new Exception(s"cannot parse expression: $msg, at ${next.pos}, before ${next.source}")
        case Error(msg, next) => throw new Exception(s"cannot parse expression: $msg, at ${next.pos}, before ${next.source}")

    /*
     * Similar to parse, but:
     * - only parses a single SExp
     * - doesn't require having reached the end of the reader after parsing
     * - can use any reader as input (instead of reading a full string from the beginning)
     */
    def parseIn(s: Reader[Char], tag: PTag = noTag): (SExp, Int) = exp(tag)(new Scanner(s)) match
        case Success(res, next) => (res, next.offset)
        case failure: NoSuccess => throw new Exception(s"cannot parse expression: $failure")
