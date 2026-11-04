package maf.util

trait Show[V] extends Serializable:
    def show(v: V): String

object Show:
    def apply[V: Show]: Show[V] = implicitly

    given listShow[X: Show]: Show[List[X]] with
        def show(v: List[X]): String =
            s"[${v.map(Show[X].show).mkString(",")}]"
