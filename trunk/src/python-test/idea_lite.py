#!/usr/bin/env python

import argparse
from arff import *
from statistics import *
from gridclus import *
from instance import *
from quadrant import *
from figure import *
import time
from util import *
from copy import deepcopy

def main():
    args = parse_options()

    title = args.train[0].split("/")[-1].split(".")[0]
    arff = Arff(args.train)

    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    train, test = ic.stratified_cross_val(args.stratified)

    quadrants = QuadrantTree(deepcopy(train)).leaves()
    clusters = GRIDCLUS(deepcopy(quadrants), args.accept)

    stats = []
    for cluster in clusters:
        stats.append(DefectStats())

    for instance in test:

        # Find closest cluster to test instance.
        closest_cluster = [sys.maxint, None]
        for i in range(len(clusters)):
            for quadrant in clusters[i]:
                tmp_distance = distance(instance.datum, quadrant.center())
                if tmp_distance < closest_cluster[0]:
                    closest_cluster[0] = tmp_distance
                    closest_cluster[1] = i

        # Points and associated classes for the closest cluster's quadrants.
        modified_train = []
        for quadrant in clusters[closest_cluster[1]]:
            modified_train.extend(quadrant.ClassCoords())

        
        # Classify the test instance
        got = classify(instance.datum, modified_train, "DEFECT")
        want = instance.datum[-1]
        
        # Increment the stat object appropriately using Evaluate
        stats[closest_cluster[1]].Evaluate(got, want)
        
        # Win the game.


def parse_options():
    """Place new options that you would like to be parsed here."""
    parser = argparse.ArgumentParser(description='Perform IDEA on given train and test sets.')
    parser.add_argument('--train',
                        dest='train',
                        metavar='FILE',
                        type=str,
                        nargs='+',
                        help='Specify arff file[s] from which to construct the training set.')
    parser.add_argument('--test',
                        dest='test',
                        metavar='FILE',
                        type=str,
                        nargs='+',
                        help='Specify arff files[s] from which to construct a test set.\
                              Not specifying this results in a self-test that\'s only useful\
                              for quick tests of the software.')
    parser.add_argument('--stratified',
                        dest='stratified',
                        default=None,
                        metavar='RATIO',
                        type=int,
                        nargs=2,
                        help='Specify a ratio for a stratified cross-validation scheme.\
                              This takes two arguments for a x train to x test ratio.\
                              Do not include a test set with this flag.)')
    parser.add_argument('--accept',
                        dest='accept',
                        default=None,
                        metavar='FLOAT 0-1',
                        type=float,
                        nargs=1,
                        help='Specify the acceptance rate to use when generating clusters.')
    parser.add_argument('--output',
                        dest='output',
                        type=str,
                        metavar='CONCAT',
                        default='',
                        help='Specify an output dir.')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
