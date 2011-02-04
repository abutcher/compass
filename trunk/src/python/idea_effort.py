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

def main():
    args = parse_options()

    title = args.train[0].split("/")[-1].split(".")[0]
    arff = Arff(args.train[0])

    print "dataset,%s" % (title)

    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    stats = EffortStats()
    
    for i in range(len(ic.instances)):
        test = deepcopy(ic.instances[i])
        train = deepcopy(ic.instances)
        train.remove(test)

        trainXY = log_y(log_x(deepcopy(train)))
        # Log functions need to check for just one item.
        testXY = log_y(log_x(deepcopy(test)))

        quadrants = QuadrantTree(trainXY)

        # Implement method for culling high variance quadrants.
        # Traverse quadrant tree to find a suitable quadrant.
        # got = ...

        stats.Evaluate(got, testXY[-1])

    print "mdmre, %.2f" % (stats.MDMRE())

def parse_options():
    """Place new options that you would like to be parsed here."""
    parser = argparse.ArgumentParser(description='Perform IDEA on given train and test sets.')
    parser.add_argument('--train',
                        dest='train',
                        metavar='FILE',
                        type=str,
                        nargs='+',
                        help='Specify arff file[s] from which to construct the training set.')
    parser.add_argument('--accept',
                        dest='accept',
                        default=None,
                        metavar='FLOAT 0-1',
                        type=float,
                        nargs=1,
                        help='Specify the acceptance rate to use when generating clusters.')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()

