#!/usr/bin/env python
"""IDEA is ..."""

import argparse
from arff import *
from statistics import *
from gridclus import *
from instance import *
from quadrant import *
from figure import *
from util import *
from copy import deepcopy

def main():
    """All of the magic happens here"""
    args = parse_options()

    title = args.train[0].split("/")[-1].split(".")[0]
    arff = Arff(args.train)

    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)

    PrintHeaderLine()
    for i in range(args.xval):
        
        ic.normalize_coordinates()

        k_fold = ic.k_fold_stratified_cross_val(args.xval)
        print ""

        for by in range(args.xval):
            test = k_fold[0]
            k_fold.remove(test)
            train = squash(k_fold)
            k_fold.append(test)

            trainLogX = log_x(deepcopy(train))
            trainLogY = log_y(deepcopy(train))
            trainLogXY = log_x(log_y(deepcopy(train)))

            testLogX = log_x(deepcopy(test))
            testLogY = log_y(deepcopy(test))
            testLogXY = log_x(log_y(deepcopy(test)))

            quadrants = QuadrantTree(train).leaves()
            quadrantsX = QuadrantTree(trainLogX).leaves()
            quadrantsY = QuadrantTree(trainLogY).leaves()
            quadrantsXY = QuadrantTree(trainLogXY).leaves()
            
            acceptance = [0.1,0.25,0.4,0.5]
           
            for accept in acceptance:
                
                clusters = GRIDCLUS(quadrants, accept)
                # Figure(args.train[0], train, quadrants, clusters).write_png()
                PerformIDEACluster(clusters, test, title, "IDEACLUSTER-"+str(accept))
                del clusters

                clustersX = GRIDCLUS(quadrantsX,accept)
                PerformIDEACluster(clustersX, testLogX,title,"IDEACLUSTER-"+str(accept)+"-logX")
                del clustersX
                              
                clustersY = GRIDCLUS(quadrantsY,accept)
                PerformIDEACluster(clustersY, testLogY,title,"IDEACLUSTER-"+str(accept)+"-logY")
                del clustersY

                clustersXY = GRIDCLUS(quadrantsXY,accept)
                PerformIDEACluster(clustersXY, testLogXY,title,"IDEACLUSTER-"+str(accept)+"-logXY")
                del clustersXY

            del quadrants
            del trainLogX, testLogX, quadrantsX
            del trainLogY, testLogY, quadrantsY
            del trainLogXY, testLogXY, quadrantsXY
                
            PerformBaseline(train, test, title)
            PerformBaseline(log_datum(train), log_datum(test), title,"log")
                
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
