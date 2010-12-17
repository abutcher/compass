#!/usr/bin/env python

import argparse
import csv
from util import *
from scipy import stats
import numpy as np

def main():
    args = parse_options()

    try:
        reader = csv.reader(open(args.input, "r"))
    except IOError:
        print "Don't pull that crap again!"
        
    data = []
    for row in reader:
        for i in range(len(row)):
            if isnumeric(row[i]):
                row[i] = float(row[i])
        data.append(row)

    num_treatments = (len(data) - 2)/(args.xval*args.xval)

    treatments = []
    for i in range(num_treatments):
        treatments.append((data[i+2][0], [], []))

    for i in range(len(data)):
        for j in range(len(treatments)):
            if data[i][0] == treatments[j][0]:
                treatments[j][1].append(data[i][1])
                treatments[j][2].append(data[i][2])

    treatments = sorted(treatments, key=lambda treatment: np.median(treatment[2]), reverse=True)

    ranks = [0]
    current = 0
    last = 0
    for i in range(len(treatments) - 1):
        if stats.mannwhitneyu(treatments[i][2], treatments[i+1][2])[1] < 0.5:
            current += 1
            ranks.append(current)
        else:
            ranks.append(current)
    
    for treatment in treatments:
        minn = min(treatment[2])*100
        maxn = max(treatment[2])*100
        medn = np.median(treatment[2])*100
        print "%d & %.2f & %.2f &\\boxplot{%.2f}{%.2f}{%.2f}{%.2f}{%.2f} \\\\" % (ranks[treatments.index(treatment)]+1, treatment[0]*100, np.average(treatment[1]), minn, medn - minn, medn, maxn-medn, maxn)

        
def parse_options():
    parser = argparse.ArgumentParser(description="Nasty script to make latex boxplots")
    
    parser.add_argument('-i',
                        dest='input',
                        metavar='FILE',
                        type=str,
                        help='csv file')
    parser.add_argument('-x',
                        dest='xval',
                        metavar='#',
                        type=int,
                        help='how many cross vals')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
