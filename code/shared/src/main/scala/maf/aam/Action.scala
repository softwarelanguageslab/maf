package maf.aam

trait Continuation 

object Action: 
  def push(kont: Continuation) 
