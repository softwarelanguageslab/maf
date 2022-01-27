Incompatibilities with R5RS
=============================

The analyses in MAF are largely focussed around Scheme programs. As such, we follow for the most part the R5RS spec. 
However, on some aspects both the concrete and abstract semantics in MAF differ from the actual R5RS semantics, or differ from the semantics implemented in popular R5RS implementations (such as DrRacket).

This document aims to describe these differences. 

## The `define` special form

### Letrec is letrec*

In order to approximate top-level "define" semantics, our `letrec` special form is implemented as a `letrec*` special form, which means that the values of earlier bindings will be available in the next bindings. In contrast, a `letrec` also first makes the variables available in the environment, but only assigns them after all bindings have been evaluated.

### Double define's

In one of the proprocessing steps, `define`s are translated to `letrec`. Both `letrec` and `let` are restricted in that the variables of its bindings should be unique. This means that the following program will be rejected:

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

### Internal definitions 

During preprocessing MAF rejects any program that contains internal definitions on invalid locations. Define's may only occur in the beginning of a body (according to the R5RS specification). However, in popular R5RS implementations such as the one provided with DrRacket, this is only detected at run time. 


## Errors in the abstract interpreter

MAF currently does not explicitly model errors. Invalid operations simply yield `bottom` and the analysis halts for that particular program path.

## Numerical system

Scheme has a powerful numerical system. This numerical system is only modelled partially: we support both integers and real numbers. 


