Abstracting Abstract Machines (AAM) vs ModF
=====================================================

MAF is a complete rework of Scala-AM which did not focus on modular static analyses 
but was primarily used to experiment with AAM-style analyses.

To compare MAF's primary analyses (ModF and ModConc), MAF includes an implementation
of an AAM style analysis with various optimisations. It shares the implementation 
of primitives and abstract domains with ModF, but implements its own semantics
and worklist algorithms.

This document describes how a ModF style analysis can be obtained from an AAM style
analysis, by modifying specific parts of the AAM style analysis.

Similar to Van Horn et al. [1], we focus on the abstraction of CESK machines.

## Classic AAM analysis

A classic AAM style analysis is defined as a fixed point of the semantics' transition
relation named `step`. 

```scala
trait ClassicAAM: 
   type Continuation = { def next: Option[Address] }
   enum Control:
      Ev(prg: Program, env: Env)
      Ap(vlu: Value)
      Hlt

   type State = (Control, Sto, Continuation)
   def step(state: State): State
   def inject(program: Program): State
   def analyze(program: Program): Result = 
      fix(step(inject(program)))
```

In a CESK machine the state is described using the four tuple: (Control, Environment, State, Continuation). For convience, the environment can be factored out into the control component, such that does not need to be restored using specialized continuation frames.

The control component consists of three alternatives: 

* Ev: instructs the machine to evaluate the given expression in the given environment 
* Ap: instructs the machine to apply the continuation that is on top of its continuation stack

The store is a mapping from addresses to values, and the continuation is a linked list of continuations. In classic AAM, the "next pointer" of the continuation is an address in the continuation store. In practice, a continuation store can be merged with a value store into a single store as long as both are allocated on different addresses and cannot be joined together.

## Proper call-return matching

Gilray et al. [2] illustrate how classic AAM does not have proper-call matching. To achieve proper-call return matching, it is sufficient to allocate the addresses in such a way that the continuations (the context in which the function is called) is taken into account. 

In the simplest form, it is sufficient to have a seperate allocation strategy for function continuations. For example, the function that is being called could be part of the continuation address so that when the function returns, it is only calls to that function that will continue their analysis.

Such an analysis can be constructed using the following code (from the `aam` package):

```scala
class SimpleAnalysis(b: SchemeExp)
   extends BaseSchemeAAMSemantics(b) // we want to analyse Scheme
   with SchemeAAMNoExt // no extensions to the state
   with SchemeAAMLocalStore // classic AAM uses stores that are part of the state
   with SchemeConstantPropagationDomain // abstract domain
   with SchemeAAMContextInsensitvity // 1-m-cfa
```

## Frontier Logging Global Store

### Background

A deciding factor for the performance of an AAM analysis is the size of the local store. In classic AAM this store is rather larger: all continuations and values are allocated in the store. This means that we can have (in the worst) case `#Values ^ #Address` number of stores. 

Since each state contains a store (that could be potentially different, even on a single address) this largely impacts the number of states that need to be analyzed before a fixedpoint is reached. The worst-case performance of a classic AAM analysis is: `(#Values ^ #Address) * #Control * #Continuation`. 

Van Horn et al. [3] propose a number of optimisations to improve the performance of the analysis. In particular, the store is no longer local, but shared between all states. Updates to the store are only performed when the frontier (the successors of the previous fully analyzed state) is fully analyzed, and can be replaced by a new frontier (those successors that are generated when analyzing the previous frontier). The delayed application is important for a number of reasons, in particular we want to avoid states from different branches to see side-effects from the future.

Each state in the set of seen states, as well as in the frontier are timestamped. The timestamps of the states in the new frontier are incremented as soon as a global store change is introduced. This mechanism ensures that states take changes to the global store into account, as the store is no longer part of the state itself and cannot be used for differentiation in the set of seen states.

To do this, the analysis of a single state keeps a log of changes it needs to make to the global store. These logs are collected until they can be applied on the global store. 

### Implementation

We require a change of terminology to maintain a difference between the states used in the semantics, and the states used in the analysis.

The semantics still require access to *a* store. Access can be described in terms of reads and writes to the store:

```scala
trait Store[S]: 
   def read(sto: S, adr: Address): (Sto, Value)
   def write(sto: S, adr: Address, vlu: Value): (Sto, Value)

trait ClassicAAM:
   /** The concrete type of the store */
   type Sto
   /** Access to an implementation of `read` and `write` */
   val sto: Store[Sto]

class LogginStore(writes: Set[Write], globalStoRef: GlobalStoRef)
trait LoggingStoreAnalysis extends ClassicAAM:
   type Sto = LoggingStore 
   val sto: Store[LoggingSto] = new Store {
      /* ... implementation to log writes to the store ... */
   }
```

Both `read` and `write` return stores (in addition to their regular return value), the store from the `read` operation is ignored for the logging global store. For the `write` operation it logs that a component has written to the store on a particular address but only applies it locally and not directly globally. 

As the `LoggingStore` keeps a reference to the global store it is able to satisfy the queries from the semantics. 

To differentiate between the states from the semantics, and the states in the set of seen states. We rename the states from the analysis to "configurations". The analysis' `step` relation still produces `states`, but the analysis keeps track of `configurations`. 

In the case of the logging global store, a configuration consists of: a control component, a continuation component, a reference to the global store, and a timestamp.

```scala
trait Configuration[Conf, State]: 
   def asConf(st: State): Conf
   def asState(c: Conf): State

trait AAMAnalysis:
   /* State, Control and Continuation are as before */
   type Conf
   val conf: Configuration[Conf, State]

trait LoggingStoAnalysis extends ClassicAAM: 
   type Conf = (Control, Continuation, GlobalStoRef, Int)
   val conf = new Configuration {
      def asConf(st: State) = /** Strip log from state */
      def asState(conf: Conf) = /* Wrap GlobalStore into LoggingStore */
   }
```

As the configuration only contains a reference to the global store which is the same for all configurations, the number of configurations is reduced significantly.

A simple fixed point algorithm on the `step` relation is no longer sufficient. Instead, we define the analysis as a fixedpoint on a `System`. In the case of the logging global stores, the system consists of two worklists (the current frontier and the next frontier), a global store (**not** a reference), and the current timestamp.

```scala
trait AAMAnalysis:
   /** Advance the worklists in the system */
   def step(system: System): System
   /** Inject a program into the system */
   def inject(program: Program): System
   def analyze(program: Program): Result =
      fix(step(inject(program)))

trait LoggingStoAnalysis extends AAMAnalysis: 
   /** ... as before ... */ 
   type System = LoggingStoSystem
   case class LoggingStoSystem(frontier: Set[Conf], newFrontier: Set[Conf], seen: Set[Conf], store: GlobalSto, logs: List[Write])
   /** step and inject function as described above */
```

This variant of the analysis can be constructed as follows:

```scala 
class SimpleAnalysis(b: SchemeExp)
   extends BaseSchemeAAMSemantics(b) // we want to analyse Scheme
   with SchemeAAMNoExt // no extensions to the state
   with BaseSchemeLoggingLocalStore // logging store + timestamped configurations
   with SchemeConstantPropagationDomain // abstract domain
   with SchemeAAMContextInsensitvity // 1-m-cfa
   with BaseSimpleWorklistSystem // frontier
```

## Function Call Boundaries

### Background

AAM ensures termination by obtaining an abstraction of the CESK machine that consists a finite number of states. Many functional programming languages such as Scheme and Haskell have no explicit looping constructs (such as `for` and `while`), the only way to obtain a non-terminating program (in the concrete) is by using recursion.

Therefore, in terms of termination, the allocation of continuations within a function call is not particularly interesting. Even if they are directly linked without using the continuation store, they could not cause an infinite continuation stack.

An optimisation therefore is to not store-allocate intra-function continuations, but store allocate them as soon as a function boundary is encountered. This reduces the size of the store significantly, which can yield memory reductions in the global store, and performance improvements in the local store version of the analysis.

### Implementation

This optimisations can be implemented by adapting the set of `Control` components, and ensuring that states are only created using an abstraction. This is important as we would like to continue the semantics internally instead of producing a successor state.

```scala
trait AAMAnalysis:
   /** .. as before .. **/
   enum Control:
      case Ev(prg: Program, env: Env) 
      case Ap(vlu: Value)
      case Hlt
      /** Enter a function call, args must be already written to the store, env is the extended 
          lexical environment. */
      case Call(fun: FunExp, env: Env)
      /** Exit a function call */
      case Exit(vlu: Value)

   /** An analysis that does not step internally can just return the different 
       control components here */

   def ev(prg: Program, env: Env, sto: Sto, kon: Continuation): State
   def ap(vlu: Value, sto: Sto, kon: Continuation): State
   def call(fun: FunExp, env: Env, sto: Sto, kon: Continuation): State
   def exit(vlu: Value, env: Env, sto: Sto, kon: Continuation): State

trait FunctionCallBoundary extends AAMAnalysis:
   /** provide internally stepping implementations for `ev` and `ap`, but generate 
       successor states for `call` and `exit` */
```

## Store allocated return values

### Background

In a classic AAM analysis, the abstract domain of values needs to be finite for the analysis to terminate. The reason for this is that no widening on the values of the abstract domain is defined (other than joining values together using a powerset lattice). 

However, in the case of a constant propagation domain, which is a domain that is infinite in width, the analysis does not terminate. ModF, in contrast, is garantueed to terminate for lattices of finite height (but does not need lattices of finite width). The key insight here is that the analysis does not terminate because upon return, the value in the `Ap` or `Call` component can take any value from the abstract domain. This is especially problamatic for functions whose return value does not depend on the value of an argument (which are allocated and potentially widened in the store). 

For example:

```scheme 
(define (length lst)
   (if (null? lst)
       1 
       (+ 1 (length (cdr lst)))))
```

Eventually, due to imprecision, the analysis will consider both branches of the `if` expression, which means that the semantics produces two successor states: `1` and `+ 1 1` (the second one comes from the continuation that returns `1` in the consequent branch of the `if` expression). This result in both `1`and `2` being send to the continuation of the recursive call to `length`. Now we repeat `+ 1 1` and `+ 1 2`. Since, we already visited the `+ 1 1` case, we do not consider this again, however we can continue with a new value `3` in the `Ap` control component. This process repeats itself ad infinitum, meaning that the analysis does not terminate.

A common way to solve this problem is by store allocating the value of `Ap` as well. Which means that the widening is taken care of by the `write` operation of the store. The only requirement is that values that return to the same continuation are allocated on the same address. An address that uses the return continuation can be used to achieve this;

This variant can be constructed using:

```scala
class SimpleAnalysis(b: SchemeExp)
   extends BaseSchemeAAMSemantics(b) // we want to analyse Scheme
   with SchemeAAMNoExt // no extensions to the state
   with BaseSchemeLoggingLocalStore // logging store + timestamped configurations
   with SchemeConstantPropagationDomain // abstract domain
   with SchemeAAMContextInsensitvity // 1-m-cfa
   with BaseSimpleWorklistSystem // frontier
   with SchemeStoreAllocateReturn // NEW
```

## Effect-Driven analysis

The frontier based global store has one major drawback: it will analyse all configurations in the frontier when the global store has changed, even if those configurations are not impacted by the change. As shown by Nicolay et al. [4], this can have a major impact on the performance of the analysis. 

A naive way to solve this problem is by keeping track of the addresses that are read by each configuration. For this the `Store` typeclass implementation of `LoggingStore` can be changed to also log reads.

Configurations in the frontier can then be filtered by only selecting those that have read dependencies on the write dependencies of its predecessor. The problem here is that read dependencies might only occur in the continuation of a particular state, a continuation that is not triggered because the successor does not have any direct read dependencies.

Even though this variant of the analysis is **unsound**, it can be constructed in MAF using:

```scala 
class SimpleAnalysis(b: SchemeExp)
   extends BaseSchemeAAMSemantics(b) // we want to analyse Scheme
   with SchemeAAMNoExt // no extensions to the state
   with SchemeConstantPropagationDomain // abstract domain
   with SchemeAAMContextInsensitvity // 1-m-cfa
   with BaseSimpleWorklistSystem // frontier
   with SchemeStoreAllocateReturn 
   with BaseSchemeDependencyLoggingStore
```

## Towards ModF

An alternative to the frontier driven analysis, is an effect driven analysis. Instead of keeping track of a frontier, we keep track of dependencies between configurations, and effects yielded by these configuration. These dependencies and effects are of the following form: 

* call effect: during the analysis of a particular configuration a call is generated. This results creating a new configuration that needs to be analyzed, unless it is already part of the set of seen configurations.
* write effect: is emitted when a write is performed against the global store (just as with the logging global store), however the write is immediatly applied to the global store and the store in the state of the semantics only keeps track of write effects by keeping track of the addresses written.
* read effect/dependency: is emitted when a read is performed against the global store. Again the store typeclass can be adapted to keep track of reads in the store of the semantics' state. 

So far, the write and read effects are quite similar to the logging global store. However, instead of keeping track of a frontier, and filter them based on these effects, we trigger the (re-) analysis of a particular configuration when the semantics write to an address that is read from that configuration.

This variant can be constructed using: 

```scala 
class SimpleAnalysis(b: SchemeExp)
   extends BaseSchemeAAMSemantics(b) // we want to analyse Scheme
   with SchemeAAMNoExt // no extensions to the state
   with SchemeConstantPropagationDomain // abstract domain
   with SchemeAAMContextInsensitvity // 1-m-cfa
   with SchemeStoreAllocateReturn // store-allocate Ap values
   with SchemeFunctionCallBoundary // only produce successors at function call boundaries
   with SchemeImperativeStoreWidening // NEW
```

## ModF

The difference between the previous version and ModF is the granularity of configurations. 
In ModF the granularity is whole functions, while in the previous variant, the granularity 
is function call boundaries.

Consider, for example, the following Scheme program:

```scheme 
(define (main)
        ...  ;; #1
	(f)
	...) ;; #2
```

Where `#2` is the continuation of the function call to `f`. In ModF there will only be one configuration for the `main` function whereas in the previous variant a configuration for `#1` and `#2` will be generated.

ModF achieves this by changing the semantics of the `call` function (see the section about function call boundaries). Instead of generating a successor state for the call to `f` with `#2` as its continuation, it will try to read the return value of `f` from the global store (the return address can be easily pre-computed) and continue with the body of `main` function using that return value. By doing so, it registers a `read` dependency on the return address of the call to `f` which will re-trigger a reanalysis of the configuration associated with the `main` once `f` is analyzed and the return address is written.

This variant of the analysis can be constructed using:

```scala
class SimpleAnalysis(b: SchemeExp)
   extends BaseSchemeAAMSemantics(b) // we want to analyse Scheme
   with SchemeAAMNoExt // no extensions to the state
   with SchemeConstantPropagationDomain // abstract domain
   with SchemeAAMContextInsensitvity // 1-m-cfa
   with SchemeStoreAllocateReturn // store-allocate Ap values
   with SchemeFunctionCallBoundary // only produce successors at function call boundaries
   with SchemeImperativeStoreWidening // Effect-driven analysis
   with SchemeFunctionModularAAM // ModF
```


## References

[1] Van Horn, D., & Might, M. (2010, September). Abstracting abstract machines. In Proceedings of the 15th ACM SIGPLAN international conference on Functional programming (pp. 51-62).       
[2] Gilray, T., Lyde, S., Adams, M. D., Might, M., & Van Horn, D. (2016, January). Pushdown control-flow analysis for free. In Proceedings of the 43rd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (pp. 691-704).      
[3] Johnson, J. I., Labich, N., Might, M., & Van Horn, D. (2012). Optimizing abstract abstract machines. arXiv preprint arXiv:1211.3722.      
[4] Nicolay, J., Stiévenart, Q., De Meuter, W., & De Roover, C. (2019, January). Effect-driven flow analysis. In International Conference on Verification, Model Checking, and Abstract Interpretation (pp. 247-274). Springer, Cham.     
