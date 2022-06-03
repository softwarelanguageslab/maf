package maf.util

import scala.collection.immutable.LinearSeq

/** A set of utilities working on collections */
object CollectionUtils:
    extension [T](seq: List[T])
        def zip2[A, B](seqA: List[A], seqB: List[B]): List[(T, A, B)] =
            if seq.isEmpty && seqA.isEmpty && seqB.isEmpty then List()
            else if seq.isEmpty || seqA.isEmpty || seqB.isEmpty then throw new Exception("Zip2 requires three collections of the same length")
            else (seq.tail).zip2(seqA.tail, seqB.tail).prepended((seq.head, seqA.head, seqB.head))

        /** Removes duplicates from a list, but keeps the order of the list */
        def distinct: List[T] =
            def rec(curr: List[T], seen: Set[T]): List[T] = curr match
                case x :: xs if !seen.contains(x) => x :: rec(xs, seen + x)
                case _ :: xs                      => rec(xs, seen)
                case List()                       => List()

            rec(seq, Set())

    extension (seq: Seq[Double])
        def median =
            val (lower, upper) = seq.sortWith((l, r) => l < r).splitAt(seq.size / 2)
            if seq.size % 2 == 0 then (lower.last + upper.head) / 2.toDouble else upper.head

        def avg =
            seq.sum / seq.size

    extension [A, B](ms: List[Map[A, B]])
        /**
         * Given a list of maps combines them into a single map as follows: List[Map[A, B]] => Map[A, List[B]]
         *
         * @param prj
         *   an optional projection function that is used to determine which keys need to be merged together
         */
        def pointwise[C](prj: (A => C)): Map[C, List[B]] =
            ms.foldRight(Map[C, List[B]]()) { (curr, m) =>
                curr.foldLeft(m) { case (m, (k, v)) =>
                    m + (prj(k) -> (v :: m.get(prj(k)).getOrElse(List())))
                }
            }

        def pointwise(): Map[A, List[B]] =
            pointwise(x => x)
