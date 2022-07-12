package maf.language.AScheme.interpreter

import maf.language.AScheme.ASchemeValues.SimpleActorId
import maf.language.AScheme.ASchemeValues.Future
import maf.core.*

case class IncorrectTerminationException(createdFutures: Map[SimpleActorId, Set[Future]]) extends Exception
case class FutureCannotBeSent(location: Identity) extends Exception
case object FutureMustTerminateBeforeEndOfTurn extends Exception
