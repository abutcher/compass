#!/usr/bin/env python
"""IDEA is ..."""

import argparse
from arff import *
from statistics import *
from gridclus import *
from instance import *
from quadrant import *
from figure import *

def main():
    """All of the magic happens here"""
    args = parse_options()

    title = args.train[0].split("/")[-1].split(".")[0]
    arff = Arff(args.train)

    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)

    ic.normalize_coordinates()
    ic.log_y_coordinates()
    ic.log_x_coordinates()

    k_fold = ic.k_fold_stratified_cross_val(args.xval)

    print ""
    PrintHeaderLine()
    
    for i in range(args.xval):
        test = k_fold[0]
        k_fold.remove(test)
        train = squash(k_fold)
        k_fold.append(test)
        
        quadrants = QuadrantTree(train).leaves()
        clusters = GRIDCLUS(quadrants)

        # Figure(args.train[0], train, quadrants, clusters).write_png()

        PerformIDEACluster(clusters, test, title)
        PerformBaseline(train, test, title)
        del train, test

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
                        type=str, nargs='+',
                        help='Specify arff files[s] from which to construct a test set.\
                              Not specifying this results in a self-test that\'s only useful\
                              for quick tests of the software.')
    parser.add_argument('--xval',
                        dest='xval',
                        metavar='INT',
                        type=int,
                        help='Specify the number of folds for cross validation.')
    parser.add_argument('--stratified',
                        dest='stratified',
                        default=None,
                        metavar='RATIO',
                        type=int, nargs=2,
                        help='Specify a ratio for a stratified cross-validation scheme.\
                              This takes two arguments for a x train to x test ratio.\
                              Do not include a test set with this flag.)')
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
