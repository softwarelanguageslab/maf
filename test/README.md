# MAF benchmark suite

This directory contains the MAF benchmark suite. This benchmark suite currently focuses on
the R5RS Scheme language. Benchmarks are also present for an extension of this language with constructs
for concurrency.

This file contains an overview of the structure of the benchmark suite, as well as
information on particular subsets of this benchmark suite.

## R5RS

### AD
All files in this and underlying folders are taken from ftp://cw.vub.ac.be/pub/courses/curriculum/AlgoDat1/programmacode/

Added tests to some files. Tests should normally evaluate to #t.
* prioq.scm should return 'Patrick .

### Gabriel
This directory includes the Gabriel benchmarks, retrieved from http://www.larcenists.org/Twobit/benchmarksAbout.html

Some benchmarks are slightly (or not-so-slightly) modified:
  - added definitions of some Scheme primitives (e.g., assq, member)
  - only run the benchmark for a single iteration, or run on smaller input
  - defines might be desugared to letrecs
  - cond desugared to ifs
  
### Gambit
All files in this and underlying folders are taken from http://github.com/gambit/gambit (2016-11-18).
Source directory: \bench\src

Adapted tests in some files. Tests should normally evaluate to #t.
* browse.scm should evaluate to 1101.

Additional remarks
* cat.scm       =>  Some necessary files are not included in the test directory yet, but the test would fail inevitably because of other reasons.
* slatex.scm    =>  Some necessary files are not included in the test directory yet, but the test would fail inevitably because of other reasons.
* tail.scm      =>  Some necessary files are not included in the test directory yet, but the test would fail inevitably because of other reasons.
* wc.scm        =>  Some necessary files are not included in the test directory yet, but the test would fail inevitably because of other reasons.

Some similar benchmarks were already included in MAF's test suite and are not included again.
* ack.scm
* boyer.scm
* conform.scm
* cpstak.scm
* dderiv.scm
* divrec.scm
* fib.scm
* nqueens.scm
* takl.scm
* trav2.scm

Some programs are not included for other reasons (non-standard primitives, ...).
* crash.scm
* dynamic.scm
* fail.scm
* fft.scm
* fibpf.scm
* gcbench.scm
* maze.scm
* mbrot.scm
* nucleic.scm
* pi.scm
* pnpoly.scm
* ray.scm
* simplex.scm
* succeed.scm
* sum1.scm
* sumfp.scm
* test.scm
* tfib.scm

### GitHub
Contains code from projects on GitHub. The files contain information on the exact source of the code, and a copyright notice if applicable.

### ICP

### KernighanVanwyk

### Rosetta

### SCP1
All files in this and underlying folders are taken from http://soft.vub.ac.be/SCPI/ .

Added/adapted tests to some files. Tests should normally evaluate to #t.

Note that by now new exercises may be present on this website and some older ones may have been deleted.
Also not all exercises have been included in this benchmark suite. TODO Add if necessary.

_SCP1 programs per topic_

**Procedures blocks conditions**
leap-year.scm
third-root.scm

**Recursion vs. iteration**
addition.scm
fast-multiply.scm
multiply.scm
calc-e-and-cos.scm
counter.scm
weird.scm
sim-fast-multiply.scm
draw-umbrella.scm

**Higher-Order Procedures**
print-abc.scm
simpson-integral.scm

**Lists**
add-to-end.scm
append.scm
super-list-merge-n.scm
list-compare-n.scm
grades.scm
compress-measurements.scm
sales-period.scm

**Trees**
count-tree.scm
fringe.scm
unfringe.scm
same-structure.scm
deep-map-combine.scm
apple-tree.scm
organigram.scm
fireworks.scm
university.scm
animal-classification.scm
tree-with-branches.scm
coca-cola.scm
family-budget.scm
circus.scm

**Objects**
flip.scm
flip2.scm
polynome.scm
haha.scm
scoreboard.scm
parking-counter.scm
square-and-rectangle.scm
lightbulb.scm
cashdesk-counter.scm
car-counter.scm
twitter.scm

**Destructive operations**
count-pairs.scm
ring.scm
ring-rotate.scm
find-cycles.scm
ring-copy.scm
josephus-problem.scm
count-pairs2.scm
flatten.scm
ring-squares.scm
slide-in.scm
dedouble.scm
insert.scm
all-but-interval.scm
merge.scm

### SCP1-compressed
All files in this and underlying folders are taken from http://soft.vub.ac.be/SCPI/ .

Added/adapted tests to some files. Tests should normally evaluate to #t.

### SETL

### Sigscheme
All files in this and underlying folders are taken from https://github.com/uim/sigscheme/tree/master/bench (2017-04-11).

Added/adapted tests to some files. Tests should normally evaluate to #t.
* takr.scm should return 7.
* loop.scm should evaluate to 8000.
* let-loop.scm should return 20000.
* arithint.scm should evaluate to 20001.

Some similar benchmarks were already included in MAF's test suite and are not included again.
* fib.scm
* cpstak.scm

### WeiChenRompf2019
All files in this and underlying folders are taken from 'Artifact for Staged Abstract Interpreters (OOPSLA 2019)'. <br>
Source: https://zenodo.org/record/3374032#.XahuIi97HYo <br>
Following benchmarks were already present (in original or modified form) and have been omitted: 
<ul>
  <li>blur.scm</li>
  <li>church.sch</li>
  <li>church_backup.sch</li>
  <li>sat.scm</li>
  <li>toplas98/matrix.scm</li>
</ul> 
Some benchmarks with identical names were kept since the file contents were
sufficiently different. Where needed, benchmarks were modified to make them work (e.g., some benchmarks
did not use valid Scheme syntax such as square brackets or had missing parentheses).

#### the-little-schemer
Code from the book The Little Schemer.

[The Little Schemer](http://mitpress.mit.edu/books/little-schemer)

#### toplas98
Benchmarks from _Polymorphic splitting: an effective polyvariant flow analysis_, Andrew K. Wright and Suresh Jagannathan.
