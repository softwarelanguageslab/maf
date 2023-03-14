package maf.syntax

/** An opaque value to differentiate between identities of expressions on the
  * same position but in different contexts (e.g., from different files)
  */
type PTag = Any

enum Identity:
  /** Neutral identity for elements not part of the cade (i.e., constructed by
    * the analysis)
    */
  case None

  /** Identity that carries positional information about an element in the code
    *
    * @param pos
    *   the position of the element in the code
    * @param tag
    *   an optional tag for differentiation
    */
  case Positional(pos: scala.util.parsing.input.Position, tag: Option[PTag])

  /** Identity for synthetic children in an AST which are generated from a
    * parent that has a position (e.g., for macro expansion)
    *
    * @param parent
    *   the position of the parent of the synthetic element
    * @param depth
    *   the depth of the element in the AST
    * @param nchild
    *   the position in the list of children
    */
  case TreePositional(parent: Identity.Positional, depth: Int, nchild: Int)

  /** Returns true if the the identity is equal to `None` */
  def isEmpty: Boolean = this match
    case None => true
    case _    => false

  /** Returns true if the identity contains a tag that is equal to `tag` */
  def hasTag(tag: PTag): Boolean = this match
    case Positional(pos, tag)         => tag.contains(tag)
    case TreePositional(parent, _, _) => parent.hasTag(tag)
    case _                            => false

object Identity:
  /** Alias for the `None` case */
  def none: Identity = Identity.None
