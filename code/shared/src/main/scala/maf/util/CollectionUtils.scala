package maf.util

import scala.collection.immutable.LinearSeq

/** A set of utilities working on collections */
object CollectionUtils:
    extension [T](seq: List[T])
      def zip2[A, B](seqA: List[A], seqB: List[B]): List[(T, A, B)] =
        if seq.isEmpty && seqA.isEmpty && seqB.isEmpty then List()
        else if seq.isEmpty || seqA.isEmpty || seqB.isEmpty then throw new Exception("Zip2 requires three collections of the same length")
        else (seq.tail).zip2(seqA.tail, seqB.tail).prepended((seq.head, seqA.head, seqB.head))
