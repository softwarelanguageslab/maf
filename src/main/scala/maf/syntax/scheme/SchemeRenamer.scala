package maf.syntax
package scheme

/** Object that provides a method to rename variables in a Scheme program in
  * order to have only unique names. For example, (let ((x 1)) (let ((x 2)) x))
  * will be converted to (let ((_x0 1)) (let ((_x1 2)) _x1)). This is useful to
  * perform ANF conversion.
  */
object SchemeRenamer:

  /** Maps each variables to their alpha-renamed version (eg. x -> _x0) */
  type NameMap = Map[String, String]

  /** Map each variables to the number of times it is bound */
  type CountMap = Map[String, Int]

  def rename(exp: SchemeExp): SchemeExp =
    rename(exp, Map[String, String](), Map[String, Int]()) match
      case (e, _) => e

  def rename(
      exp: SchemeExp,
      names: NameMap,
      count: CountMap
  ): (SchemeExp, CountMap) = exp match
    case SchemeLambda(name, args, body, ann, pos) =>
      countl(args, names, count) match
        case (args1, names1, count1) =>
          renameList(body, names1, count1) match
            case (body1, count2) =>
              (
                SchemeLambda(
                  name.map(n => names.getOrElse(n, n)),
                  args1,
                  body1,
                  ann,
                  pos
                ),
                count2
              )
    case SchemeVarArgLambda(name, args, vararg, body, ann, pos) =>
      countl(args ++ List(vararg), names, count) match
        case (args1, names1, count1) =>
          renameList(body, names1, count1) match
            case (body1, count2) =>
              (
                SchemeVarArgLambda(
                  name.map(n => names.getOrElse(n, n)),
                  args1.init,
                  args1.last,
                  body1,
                  ann,
                  pos
                ),
                count2
              )
    case SchemeFuncall(f, args, pos) =>
      rename(f, names, count) match
        case (f1, count1) =>
          renameList(args, names, count1) match
            case (args1, count2) => (SchemeFuncall(f1, args1, pos), count2)
    case SchemeIf(cond, cons, alt, pos) =>
      rename(cond, names, count) match
        case (cond1, count1) =>
          rename(cons, names, count1) match
            case (cons1, count2) =>
              rename(alt, names, count2) match
                case (alt1, count3) =>
                  (SchemeIf(cond1, cons1, alt1, pos), count3)
    case SchemeLet(bindings, body, pos) =>
      countl(bindings.map(_._1), names, count) match
        /* Use old names for expressions of bindings */
        case (variables, names1, count1) =>
          renameList(bindings.map(_._2), names, count1) match
            case (exps, count2) =>
              renameList(body, names1, count2) match
                case (body1, count3) =>
                  (SchemeLet(variables.zip(exps), body1, pos), count3)
    case SchemeLetStar(bindings, body, pos) =>
      renameLetStarBindings(bindings, names, count) match
        case (bindings1, names1, count1) =>
          renameList(body, names1, count1) match
            case (body1, count2) =>
              (SchemeLetStar(bindings1, body1, pos), count2)
    case SchemeLetrec(bindings, body, pos) =>
      countl(bindings.map(_._1), names, count) match
        /* Use new names for expressions of bindings */
        case (variables, names1, count1) =>
          renameList(bindings.map(_._2), names1, count1) match
            case (exps, count2) =>
              renameList(body, names1, count2) match
                case (body1, count3) =>
                  (SchemeLetrec(variables.zip(exps), body1, pos), count3)

    case SchemeSet(variable, value, pos) =>
      rename(value, names, count) match
        case (value1, count1) =>
          (
            SchemeSet(
              names.get(variable.name) match {
                case Some(n) => Identifier[SchemeExp](n, variable.idn)
                case None    => variable
              },
              value1,
              pos
            ),
            count1
          )
    case SchemeBegin(body, pos) =>
      renameList(body, names, count) match
        case (body1, count1) => (SchemeBegin(body1, pos), count1)
    case SchemeAssert(exp, pos) =>
      rename(exp, names, count) match
        case (exp1, count1) => (SchemeAssert(exp1, pos), count1)
    case SchemeDefineVariable(name, value, pos) =>
      /* Keeps name untouched (maybe not correct?) */
      rename(value, names, count) match
        case (value1, count1) =>
          (SchemeDefineVariable(name, value1, pos), count1)
    case SchemeVar(id) =>
      names.get(id.name) match
        case Some(n) => (SchemeVar(Identifier[SchemeExp](n, id.idn)), count)
        case None =>
          (
            SchemeVar(Identifier[SchemeExp](id.name, id.idn)),
            count
          ) /* keep original name */
    case SchemeValue(v, pos) =>
      (SchemeValue(v, pos), count)
    case _ => throw new Exception(s"Unhandled expression in renamer: $exp")

  /** Renames a list of expressions executed sequentially (eg. within a begin)
    */
  def renameList(
      exps: List[SchemeExp],
      names: NameMap,
      count: CountMap
  ): (List[SchemeExp], CountMap) = exps match
    case exp :: rest =>
      val (exp1, count1) = rename(exp, names, count)
      val (rest1, count2) = renameList(rest, names, count1)
      (exp1 :: rest1, count2)
    case Nil => (Nil, count)

  def renameLetStarBindings(
      bindings: List[(Identifier[SchemeExp], SchemeExp)],
      names: NameMap,
      count: CountMap
  ): (List[(Identifier[SchemeExp], SchemeExp)], NameMap, CountMap) =
    bindings match
      case (v, e) :: rest =>
        count1(v, names, count) match
          /* use old names, as with a let* the variable is not yet bound in its
           * definition */
          case (v1, names1, count1) =>
            rename(e, names, count1) match
              case (e1, count2) =>
                renameLetStarBindings(rest, names1, count2) match
                  case (rest1, names2, count3) =>
                    ((v1, e1) :: rest1, names2, count3)
      case Nil => (Nil, names, count)

  /** To be called when a new variable is introduced in the scope. Adds it to
    * the name map and count map
    */
  def count1(
      variable: Identifier[SchemeExp],
      names: NameMap,
      count: CountMap
  ): (Identifier[SchemeExp], NameMap, CountMap) =
    val c: Int = count.get(variable.name) match
      case Some(x) => x + 1
      case None    => 0
    val n = s"_$variable$c"
    (
      Identifier[SchemeExp](n, variable.idn),
      names + (variable.name -> n),
      count + (variable.name -> c)
    )

  /** Same as count1 but for a list of variables */
  def countl(
      variables: List[Identifier[SchemeExp]],
      names: NameMap,
      count: CountMap
  ): (List[Identifier[SchemeExp]], NameMap, CountMap) =
    variables.foldLeft((List[Identifier[SchemeExp]](), names, count))(
      (
          st: (List[Identifier[SchemeExp]], NameMap, CountMap),
          v: Identifier[SchemeExp]
      ) =>
        st match {
          case (l, ns, cs) =>
            count1(v, ns, cs) match {
              case (v1, ns1, cs1) => (v1 :: l, ns1, cs1)
            }
        }
    ) match
      case (l, ns, cs) => (l.reverse, ns, cs)
