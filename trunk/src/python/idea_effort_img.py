#!/usr/bin/env python

import argparse
from arff import *
from statistics import *
from gridclus2 import *
from instance import *
from quadrant import *
from figure import *
import time
from util import *
from copy import deepcopy
from bore import *
from NaiveBayes import *
import random

def main():
    random.seed(5)
    args = parse_options()

    title = args.train[0].split("/")[-1].split(".")[0]
    arff = Arff(args.train[0])
    
    print "Running dataset %s, outputting to %s.png" % (title, title)

    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    train = deepcopy(ic.instances)
    trainXY = log_y(log_x(deepcopy(train)))

    print "Generating quadrant tree..."
    qtree = QuadrantTree(trainXY)

    print "Culling high variance branches..."
    # Implement method for culling high variance quadrants.

    print "Creating image from regions of good/badness..."
    # Pass of quadrants to Figure to generate good and bad regions
    # based on variance.
        
    print "Writing figure %s.png ..." % (title)
    fig = Figure(title, trainXY, quadrants, culled_quadrants)
    fig.write_png()

def parse_options():
    """Place new options that you would like to be parsed here."""
    parser = argparse.ArgumentParser(description='Perform IDEA on given train and test sets.')
    parser.add_argument('--train',
                        dest='train',
                        metavar='FILE',
                        type=str,
                        nargs='+',
                        help='Specify arff file[s] from which to construct the training set.')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
