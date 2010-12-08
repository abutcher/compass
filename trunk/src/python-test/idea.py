#!/usr/bin/env python
"""IDEA is ..."""

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
    """All of the magic happens here"""
    args = parse_options()

    title = args.train[0].split("/")[-1].split(".")[0]
    arff = Arff(args.train)

    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    PrintHeaderLine()
    for i in range(args.xval):
        k_fold = ic.k_fold_stratified_cross_val(args.xval)

        for by in range(args.xval):
            test = k_fold[0]
            k_fold.remove(test)
            train = squash(deepcopy(k_fold))
            k_fold.append(test)

            trainLogX = log_x(deepcopy(train))
            trainLogY = log_y(deepcopy(train))
            trainLogXY = log_x(log_y(deepcopy(train)))

            testLogX = log_x(deepcopy(test))
            testLogY = log_y(deepcopy(test))
            testLogXY = log_x(log_y(deepcopy(test)))

            quadrantsL3 = QuadrantTree(deepcopy(train), True, 3).leaves()
            quadrantsL6 = QuadrantTree(deepcopy(train), True, 6).leaves()
            quadrantsL12 = QuadrantTree(deepcopy(train), True, 12).leaves()
            quadrants = QuadrantTree(deepcopy(train)).leaves()
            quadrantsXL3 = QuadrantTree(deepcopy(trainLogX), True, 3).leaves()
            quadrantsXL6 = QuadrantTree(deepcopy(trainLogX), True, 6).leaves()
            quadrantsXL12 = QuadrantTree(deepcopy(trainLogX), True, 12).leaves()
            quadrantsX = QuadrantTree(deepcopy(trainLogX)).leaves()
            quadrantsYL3 = QuadrantTree(deepcopy(trainLogY), True, 3).leaves()
            quadrantsYL6 = QuadrantTree(deepcopy(trainLogY), True, 6).leaves()
            quadrantsYL12 = QuadrantTree(deepcopy(trainLogY), True, 12).leaves()
            quadrantsY = QuadrantTree(deepcopy(trainLogY)).leaves()
            quadrantsXYL3 = QuadrantTree(deepcopy(trainLogXY), True, 3).leaves()
            quadrantsXYL6 = QuadrantTree(deepcopy(trainLogXY), True, 6).leaves()
            quadrantsXYL12 = QuadrantTree(deepcopy(trainLogXY), True, 12).leaves()
            quadrantsXY = QuadrantTree(deepcopy(trainLogXY)).leaves()
            
            acceptance = [0.3, 0.4, 0.5]
           
            for accept in acceptance:
                
                clusters = GRIDCLUS(deepcopy(quadrants), accept)
                clustersL3 = GRIDCLUS(deepcopy(quadrantsL3), accept)
                clustersL6 = GRIDCLUS(deepcopy(quadrantsL6), accept)
                clustersL12 = GRIDCLUS(deepcopy(quadrantsL12), accept)
                pruned10 = cluster_prune(deepcopy(clusters),.1)
                pruned20 = cluster_prune(deepcopy(clusters),.2)
                pruned30 = cluster_prune(deepcopy(clusters),.3)
                PerformIDEACluster(deepcopy(clusters), deepcopy(test), title, "IDEACLUSTER-"+str(accept))
                PerformIDEACluster(deepcopy(pruned10), deepcopy(test), title, "IDEACLUSTER-PRUNED10%-"+str(accept))
                PerformIDEACluster(deepcopy(pruned20), deepcopy(test), title, "IDEACLUSTER-PRUNED20%-"+str(accept))
                PerformIDEACluster(deepcopy(pruned30), deepcopy(test), title, "IDEACLUSTER-PRUNED30%-"+str(accept))
                PerformIDEACluster(deepcopy(clustersL3), deepcopy(test), title, "IDEACLUSTER-3LIVES"+str(accept))
                PerformIDEACluster(deepcopy(clustersL6), deepcopy(test), title, "IDEACLUSTER-6LIVES"+str(accept))
                PerformIDEACluster(deepcopy(clustersL12), deepcopy(test), title, "IDEACLUSTER-12LIVES"+str(accept))                
                del clusters, clustersL3, clustersL6, clustersL12, pruned10, pruned20, pruned30

                """
                clustersX = GRIDCLUS(deepcopy(quadrantsX),accept)
                clustersXL3 = GRIDCLUS(deepcopy(quadrantsXL3), accept)
                clustersXL6 = GRIDCLUS(deepcopy(quadrantsXL6), accept)
                clustersXL12 = GRIDCLUS(deepcopy(quadrantsXL12), accept)
                prunedX10 = cluster_prune(deepcopy(clustersX),.1)
                prunedX20 = cluster_prune(deepcopy(clustersX),.2)
                prunedX30 = cluster_prune(deepcopy(clustersX),.3)
                PerformIDEACluster(deepcopy(clustersX), deepcopy(testLogX), title,"IDEACLUSTER-"+str(accept)+"-logX")
                PerformIDEACluster(deepcopy(prunedX10), deepcopy(testLogX), title,"IDEACLUSTER-PRUNED10%-"+str(accept)+"-logX")
                PerformIDEACluster(deepcopy(prunedX20), deepcopy(testLogX), title,"IDEACLUSTER-PRUNED20%-"+str(accept)+"-logX")
                PerformIDEACluster(deepcopy(prunedX30), deepcopy(testLogX), title,"IDEACLUSTER-PRUNED30%-"+str(accept)+"-logX")
                PerformIDEACluster(deepcopy(clustersXL3), deepcopy(testLogX), title, "IDEACLUSTER-3LIVES"+str(accept)+"-logX")
                PerformIDEACluster(deepcopy(clustersXL6), deepcopy(testLogX), title, "IDEACLUSTER-6LIVES"+str(accept)+"-logX")
                PerformIDEACluster(deepcopy(clustersXL12), deepcopy(testLogX), title, "IDEACLUSTER-12LIVES"+str(accept)+"-logX")
                del clustersX, clustersXL3, clustersXL6, clustersXL12, prunedX10, prunedX20, prunedX30
                """
                """           
                clustersY = GRIDCLUS(deepcopy(quadrantsY),accept)
                prunedY10 = cluster_prune(deepcopy(clustersY),.1)
                prunedY20 = cluster_prune(deepcopy(clustersY),.2)
                prunedY30 = cluster_prune(deepcopy(clustersY),.3)
                clustersYL3 = GRIDCLUS(deepcopy(quadrantsYL3), accept)
                clustersYL6 = GRIDCLUS(deepcopy(quadrantsYL6), accept)
                clustersYL12 = GRIDCLUS(deepcopy(quadrantsYL12), accept)
                PerformIDEACluster(deepcopy(clustersY), deepcopy(testLogY), title,"IDEACLUSTER-"+str(accept)+"-logY")
                PerformIDEACluster(deepcopy(prunedY10), deepcopy(testLogY), title,"IDEACLUSTER-PRUNED10%-"+str(accept)+"-logY")
                PerformIDEACluster(deepcopy(prunedY20), deepcopy(testLogY), title,"IDEACLUSTER-PRUNED20%-"+str(accept)+"-logY")
                PerformIDEACluster(deepcopy(prunedY30), deepcopy(testLogY), title,"IDEACLUSTER-PRUNED30%-"+str(accept)+"-logY")
                PerformIDEACluster(deepcopy(clustersYL3), deepcopy(testLogY), title, "IDEACLUSTER-3LIVES"+str(accept)+"-logY")
                PerformIDEACluster(deepcopy(clustersYL6), deepcopy(testLogY), title, "IDEACLUSTER-6LIVES"+str(accept)+"-logY")
                PerformIDEACluster(deepcopy(clustersYL12), deepcopy(testLogY), title, "IDEACLUSTER-12LIVES"+str(accept)+"-logY")
                del clustersY, clustersYL3, clustersYL6, clustersYL12, prunedY10, prunedY20, prunedY30
                """

                clustersXY = GRIDCLUS(deepcopy(quadrantsXY),accept)
                prunedXY10 = cluster_prune(deepcopy(clustersXY),.1)
                prunedXY20 = cluster_prune(deepcopy(clustersXY),.2)
                prunedXY30 = cluster_prune(deepcopy(clustersXY),.3)
                clustersXYL3 = GRIDCLUS(deepcopy(quadrantsXYL3), accept)
                clustersXYL6 = GRIDCLUS(deepcopy(quadrantsXYL6), accept)
                clustersXYL12 = GRIDCLUS(deepcopy(quadrantsXYL12), accept)                
                PerformIDEACluster(deepcopy(clustersXY), deepcopy(testLogXY), title,"IDEACLUSTER-"+str(accept)+"-logXY")
                PerformIDEACluster(deepcopy(prunedXY10), deepcopy(testLogXY), title,"IDEACLUSTER-PRUNED10%-"+str(accept)+"-logXY")
                PerformIDEACluster(deepcopy(prunedXY20), deepcopy(testLogXY), title,"IDEACLUSTER-PRUNED20%-"+str(accept)+"-logXY")
                PerformIDEACluster(deepcopy(prunedXY30), deepcopy(testLogXY), title,"IDEACLUSTER-PRUNED30%-"+str(accept)+"-logXY")
                PerformIDEACluster(deepcopy(clustersXYL3), deepcopy(testLogXY), title, "IDEACLUSTER-3LIVES"+str(accept)+"-logXY")
                PerformIDEACluster(deepcopy(clustersXYL6), deepcopy(testLogXY), title, "IDEACLUSTER-6LIVES"+str(accept)+"-logXY")
                PerformIDEACluster(deepcopy(clustersXYL12), deepcopy(testLogXY), title, "IDEACLUSTER-12LIVES"+str(accept)+"-logXY")                
                del clustersXY, prunedXY10, prunedXY20, prunedXY30

            del quadrants, quadrantsL3, quadrantsL6, quadrantsL12
            del trainLogX, testLogX, quadrantsX, quadrantsXL3, quadrantsXL6, quadrantsXL12
            del trainLogY, testLogY, quadrantsY, quadrantsYL3, quadrantsYL6, quadrantsYL12
            del trainLogXY, testLogXY, quadrantsXY, quadrantsXYL3, quadrantsXYL6, quadrantsXYL12
                
            PerformBaseline(deepcopy(train), deepcopy(test), title)
            PerformBaseline(log_datum(deepcopy(train)), log_datum(deepcopy(test)), title,"log")
                
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
