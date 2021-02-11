package maf.test

import org.scalatest.Tag

// Tags by non-functional test characteristics.
object SlowTest extends Tag("SlowTest")

// Tags by function.
object ParserTest extends Tag("ParserTest")
object LatticeTest extends Tag("LatticeTest")
object PrimitiveTest extends Tag("PrimitiveTest")
object SoundnessTest extends Tag("SoundnessTest")

object InterpreterTest extends Tag("InterpreterTest")

// Tags by language.
object SchemeModFTest extends Tag("SchemeModFTest")

object SchemeModConcTest extends Tag("SchemeModConcTest")

// Tags by semantic type.
object SmallStepTest extends Tag("SmallStepTest")

object BigStepTest extends Tag("BigStepTest")

// Tags by analysis variation.
object IncrementalTest extends Tag("IncrementalTest")

// Other tags
object UtilityTest extends Tag("UtilityTest")
