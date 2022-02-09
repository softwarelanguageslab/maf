Incompatibilities with R5RS
=============================

The analyses in MAF are largely focussed around Scheme programs. As such, we follow for the most part the R5RS spec. 
However, on some aspects both the concrete and abstract semantics in MAF differ from the actual R5RS semantics, or differ from the semantics implemented in popular R5RS implementations (such as DrRacket).

This document aims to describe these differences. 

## The `define` special form

MAF does not support `define` directly in either its concrete or abstract interpreters. Instead, it relies on a preprocessor, called the _undefiner_ (found [here](https://github.com/softwarelanguageslab/maf/blob/master/code/shared/src/main/scala/maf/language/scheme/SchemeMonadicUndefiner.scala)) to translate all `define` special forms in the program to (almost) equivalent `letrec` expressions. However, in doing so, some minor incompatibilities are inevitably introduced w.r.t. R5RS semantics.

### `letrec` is `letrec*`

In order to approximate top-level `define` semantics, our `letrec` special form is implemented as a `letrec*` special form, which means that the values of earlier bindings will be available in the next bindings. In contrast, a `letrec` also first makes the variables available in the environment, but only assigns them after all bindings have been evaluated.

### Duplicate definitions

In one of the proprocessing steps, all `define`s in the same scope are translated to a single `letrec`. Both `letrec` and `let` are restricted in that the variables of its bindings should be unique. As a result, duplicate definitions in a single scope using `define` are never allowed in MAF. 

In R5RS, internal definitions (i.e., `define`s that appear in regular bodies) are also translated into `letrec` in the same way, therefore also disallowing duplicate definitions for the same reason. However, as an exception, R5RS does allow duplicate definitions at the top level, treating redefinitions of a previously defined identifier using `set!` semantics. We do not support such top level redefinitions, as the top level is also just translated into a single `letrec`.

This means that the following code at the top level (which is valid in R5RS) will be rejected:

```scheme
(define x 10)
(define x 20)
(displayln x)
```

Because it is translated to: 

```scheme 
(letrec 
    ((x 10)
     (x 20)
     (_1 (displayln x)))
  _1)
```

which is illegal.

### Redefining primitives

MAF features a _preluder_ which prepends (Scheme) implementations for some primitives to the source file.
As a consequence of not allowing duplicate definitions (cf. sup.), it is not allowed to redefine functions that have the same name as preluded primitives.

When doing so anyway, the preluder will in fact not prepend such primitives to the program, since all top-level definitions are transformed by the undefiner to one single `letrec`, and hence only the self-defined function can be used. This may lead to unexpected behaviour w.r.t. other R5RS implementations (although some of them will also disallow the redefinition of primitives).

For example, the following program

```scheme
(define old-apply apply)
(define (apply f args) (display f))
(old-apply + '(1 2 3))
```
is transformed into:

```scheme
(letrec ((old-apply apply)
         (apply (lambda (f args) (display f))))
  (old-apply + '(1 2 3)))
```

and therefore throws the following error in the concrete Scheme interpreter:

```
Uninitialised variable apply at position 1:20.
```

### Internal definitions 

During preprocessing MAF rejects any program that contains internal definitions on invalid locations. Define's may only occur in the beginning of a body (according to the R5RS specification). However, in popular R5RS implementations such as the one provided with DrRacket, this is only detected at run time. 

## Errors in the abstract interpreter

None of the analyses in MAF currently model errors explicitly.
Errors encountered by the abstract interpreter are passed to method `fail` (which is part of the `MonadError` interface). 
By default, this method logs the error (to aid with debugging) and just returns `bottom` (which causes the analysis to halt for the program path where the error was encountered, but does not include any information about the error state in the analysis results). 
This behaviour can easily be changed by providing a different implementation for method `fail`.

Note that some errors are not properly handled yet, and may result in unspecified behaviour for the abstract interpreter itself (see [#21](https://github.com/softwarelanguageslab/maf/issues/21)).






## Numerical system

Scheme has a powerful numerical system. This numerical system is only modelled partially: we support both integers and real numbers. 

## Primitives

Some Scheme primitives, such as `map` and `<=` do not support a variable number of arguments, but have a fixed arity. For primitives such as `map`, the reason is twofold. 
First, a naive (preluded) implementation using a variable number of arguments would lead to a precision loss, as an extra loop is required to iterate over the variable number of arguments. 
This is especially undesirable when the primitive in practice is typically only used with a fixed number of arguments (though this problem can possibly be avoided by implementing the primitive differently; cf. the implementation of preluded primitive `min` as an example).
Second, additional precision loss can happen due to a known bug where the addresses used to allocate the vararg list may accidentally overlap with the addresses used for allocations in argument positions (see [#30](https://github.com/softwarelanguageslab/maf/issues/30)). This is especially problematic for primitives that operate on lists (such as `map`), since these often take arguments that may be allocated at the same program location as the addresses of the vararg list.


Scheme's `eqv?` currently is implemented as `eq?`.

Finally, various primitives (in particular, tricky ones such as `eval`) are not supported (yet) or only supported in limited form (such as primitive `apply`, which only works for argument lists up to a certain length).
