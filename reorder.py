import csv
import sys

with open(sys.argv[1], 'r') as infile, open('reordered.csv', 'a') as outfile:
    # output dict needs a list for new column ordering
    fieldnames = [
        '',
        'parallel (n = 1)',
        'parallel (n = 2)',
        'parallel (n = 4)',
        'parallel (n = 8)',
        'parallel (n = 16)',
        'parallel (n = 32)',
        'parallel (n = 64)',
        'parallel (n = 128)',
    ]
    writer = csv.DictWriter(outfile, fieldnames=fieldnames)
    # reorder the header first
    writer.writeheader()
    for row in csv.DictReader(infile):
        # writes the reordered rows to the new file
        writer.writerow(row)
