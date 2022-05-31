#!/usr/bin/env python

import pandas as pd

programs = [
    "games/snake.rkt",
    "games/tetris.rkt",
    "games/zombie.rkt",
    "mochi/fold-div.rkt",
    "mochi/hors.rkt",
    "mochi/hrec.rkt",
    "mochi/l-zipunzip.rkt",
    "mochi/map-foldr.rkt",
    "mochi/mappend.rkt",
    "mochi/mem.rkt",
    "mochi/mult.rkt",
    "mochi/neg.rkt",
    "mochi/nth0.rkt",
    "mochi/r-file.rkt",
    "mochi/r-lock.rkt",
    "mochi/reverse.rkt",
    "mochi/sum.rkt",
    "mochi/zip.rkt",
    "sergey/blur.rkt",
    "sergey/eta.rkt",
    "sergey/kcfa2.rkt",
    "sergey/kcfa3.rkt",
    "sergey/mj09.rkt",
    "sergey/sat.rkt",
    "softy/append.rkt",
    "softy/cpstak.rkt",
    "softy/last-pair.rkt",
    "softy/last.rkt",
    "softy/length-acc.rkt",
    "softy/length.rkt",
    "softy/member.rkt",
    "softy/recursive-div2.rkt",
    "softy/subst.rkt",
    "softy/tak.rkt"
]

groups = [
        "mochi/",
        "softy/",
        "sergey/",
        "games/snake.rkt",
        "games/zombie.rkt", 
        "games/tetris.rkt"
]

def count_lines(filename):
    """
    Count the number of lines in the given file
    
    :param file the file to count the number of lines of
    :return the number of lines in the file
    """
    with open(filename) as f:
        return len(f.readlines())


prefixed_programs = map(lambda program: "test/scv/NguyenGTH18/safe/"+program, programs)
number_of_lines = [ (unprefixed, count_lines(filename)) for filename, unprefixed in zip(prefixed_programs, programs) ]

print("{:^60} | {:^10}".format("Name", "# Lines"))
print("-"*60 + " | " + "-"*10)

# Compute totals
totals = {}


for name, num in number_of_lines:
    for group in groups:
        if name.startswith(group):
            total, count =  totals.get(group, (0, 0)) 
            totals[group] = (total + num, count + 1)
            break
    else:
        raise Exception("unhandled group "+group)


for group, (total, count) in totals.items():
    print("{:<60} | {:<10}".format(group, total/count))



