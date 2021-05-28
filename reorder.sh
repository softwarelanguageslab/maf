#!/bin/sh
rm -f reordered.csv
IN=$@
sed 's/, [0-9]-CFA//g' $IN  | grep -v "TIMEOUT" > input.csv
python reorder.py input.csv
mv reordered.csv $IN
