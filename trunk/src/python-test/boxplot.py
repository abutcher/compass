#!/usr/bin/env python

import argparse
import csv
from util import *
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

    num_treatments = (len(data) - 1)/(args.xval*args.xval)

    treatments = []
    for i in range(num_treatments):
        treatments.append((data[i+1][0], data[i+1][1], data[i+1][2], []))

    for i in range(len(data)):
        for j in range(len(treatments)):
            if data[i][0] == treatments[j][0] and data[i][1] == treatments[j][1] and data[i][2] == treatments[j][2]:
                treatments[j][3].append(data[i][-1])

    treatments = sorted(treatments, key=lambda treatment: np.median(treatment[3]), reverse=True)
    
    for treatment in treatments:
        if treatment[2] == "TRUE":
            mins = min(treatment[3])*100
            meds = np.median(treatment[3])*100
            maxs = max(treatment[3])*100
            print "%s & %s & %s & \\boxplot{%.2f}{%.2f}{%.2f}{%.2f}{%.2f} \\\\" % (treatment[0], treatment[1].replace("%","\\%"), treatment[2], mins, meds - mins, meds, maxs - meds, maxs)

    for treatment in treatments:
        if treatment[2] == "FALSE":
            mins = min(treatment[3])*100
            meds = np.median(treatment[3])*100
            maxs = max(treatment[3])*100
            print "%s & %s & %s & \\boxplot{%.2f}{%.2f}{%.2f}{%.2f}{%.2f} \\\\" % (treatment[0], treatment[1].replace("%","\\%"), treatment[2], mins, meds - mins, meds, maxs - meds, maxs)                

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
                        
    
