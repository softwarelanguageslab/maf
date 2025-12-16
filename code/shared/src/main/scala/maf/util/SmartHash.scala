package maf.util

import scala.runtime.ScalaRunTime

/* Imported from JSAI's notjs.util */

// the Scala compiler is not yet smart enough to figure out that it
// only needs to hash immutable objects once; extending case classes
// with this trait will make that happen. this one optimization can
// improve performance by orders of magnitude.
trait SmartHash extends Product with Serializable:
    private lazy val cached = computeHash
    protected def computeHash = ScalaRunTime._hashCode(this)
    override def hashCode(): Int = cached
//    override def equals(other: Any): Boolean = other match
//        case obj: SmartHash => this.cached == obj.cached && super.equals(obj) 
//        case _ => false  