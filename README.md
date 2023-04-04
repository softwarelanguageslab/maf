Modular Analysis Framework (MAF): A Framework for Modular Analysis of Dynamic Languages

## Contents of this branch

This branch contains a rewrite of the original framework (as located in the `master` branch), henceforth called MAF 1.0. 
We decided to create a fresh Scala project to incorporate a series of large refactorings and redesigns of the framework.
Currently, these are centered around two major areas of the framework:     
* The representation of the abstract domain, solving a number of long open Github issues.
* The analysis code itself which is turned into a more functional programming style, featuring open recursion for extensibility instead of subclassing, and removing the pattern of outer and inner traits for representing the inter and intra-analysis respectively.

Furthermore MAF2 embraces ubiquitous Scala functional programming libraries such as `cats` instead of rolling our own implementations of core functional programming typeclasses.
