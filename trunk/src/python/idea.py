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

            quadrants = QuadrantTree(deepcopy(train)).leaves()
            quadrantsX = QuadrantTree(deepcopy(trainLogX)).leaves()
            quadrantsY = QuadrantTree(deepcopy(trainLogY)).leaves()
            quadrantsXY = QuadrantTree(deepcopy(trainLogXY)).leaves()
            
            acceptance = [0.4, 0.5]
           
            for accept in acceptance:
                
                clusters = GRIDCLUS(deepcopy(quadrants), accept)
                #pruned10 = cluster_prune(deepcopy(clusters),.1)
                #pruned20 = cluster_prune(deepcopy(clusters),.2)
                #pruned30 = cluster_prune(deepcopy(clusters),.3)
                PerformIDEACluster(deepcopy(clusters), deepcopy(test), title, "IDEACLUSTER-"+str(accept))
                #PerformIDEACluster(deepcopy(pruned10), deepcopy(test), title, "IDEACLUSTER-PRUNED10%-"+str(accept))
                #PerformIDEACluster(deepcopy(pruned20), deepcopy(test), title, "IDEACLUSTER-PRUNED20%-"+str(accept))
                #PerformIDEACluster(deepcopy(pruned30), deepcopy(test), title, "IDEACLUSTER-PRUNED30%-"+str(accept))

                PerformIDEACluster(deepcopy(clusters), deepcopy(test), title, "IDEACLUSTER-XOnly-"+str(accept),True)
                #PerformIDEACluster(deepcopy(pruned10), deepcopy(test), title, "IDEACLUSTER-XOnly-PRUNED10%-"+str(accept),True)
                #PerformIDEACluster(deepcopy(pruned20), deepcopy(test), title, "IDEACLUSTER-XOnly-PRUNED20%-"+str(accept),True)
                #PerformIDEACluster(deepcopy(pruned30), deepcopy(test), title, "IDEACLUSTER-XOnly-PRUNED30%-"+str(accept),True)
                del clusters#, pruned10, pruned20, pruned30

                clustersX = GRIDCLUS(deepcopy(quadrantsX),accept)
                #prunedX10 = cluster_prune(deepcopy(clustersX),.1)
                #prunedX20 = cluster_prune(deepcopy(clustersX),.2)
                #prunedX30 = cluster_prune(deepcopy(clustersX),.3)
                PerformIDEACluster(deepcopy(clustersX), deepcopy(testLogX), title,"IDEACLUSTER-"+str(accept)+"-logX")
                #PerformIDEACluster(deepcopy(prunedX10), deepcopy(testLogX), title,"IDEACLUSTER-PRUNED10%-"+str(accept)+"-logX")
                #PerformIDEACluster(deepcopy(prunedX20), deepcopy(testLogX), title,"IDEACLUSTER-PRUNED20%-"+str(accept)+"-logX")
                #PerformIDEACluster(deepcopy(prunedX30), deepcopy(testLogX), title,"IDEACLUSTER-PRUNED30%-"+str(accept)+"-logX")
                PerformIDEACluster(deepcopy(clustersX), deepcopy(testLogX), title,"IDEACLUSTER-XOnly-"+str(accept)+"-logX",True)
                #PerformIDEACluster(deepcopy(prunedX10), deepcopy(testLogX), title,"IDEACLUSTER-XOnly-PRUNED10%-"+str(accept)+"-logX",True)
                #PerformIDEACluster(deepcopy(prunedX20), deepcopy(testLogX), title,"IDEACLUSTER-XOnly-PRUNED20%-"+str(accept)+"-logX",True)
                #PerformIDEACluster(deepcopy(prunedX30), deepcopy(testLogX), title,"IDEACLUSTER-XOnly-PRUNED30%-"+str(accept)+"-logX",True)
                del clustersX#, prunedX10, prunedX20, prunedX30
                              
                clustersY = GRIDCLUS(deepcopy(quadrantsY),accept)
                #prunedY10 = cluster_prune(deepcopy(clustersY),.1)
                #prunedY20 = cluster_prune(deepcopy(clustersY),.2)
                #prunedY30 = cluster_prune(deepcopy(clustersY),.3)
                PerformIDEACluster(deepcopy(clustersY), deepcopy(testLogY), title,"IDEACLUSTER-"+str(accept)+"-logY")
                #PerformIDEACluster(deepcopy(prunedY10), deepcopy(testLogY), title,"IDEACLUSTER-PRUNED10%-"+str(accept)+"-logY")
                #PerformIDEACluster(deepcopy(prunedY20), deepcopy(testLogY), title,"IDEACLUSTER-PRUNED20%-"+str(accept)+"-logY")
                #PerformIDEACluster(deepcopy(prunedY30), deepcopy(testLogY), title,"IDEACLUSTER-PRUNED30%-"+str(accept)+"-logY")
                del clustersY#, prunedY10, prunedY20, prunedY30

                clustersXY = GRIDCLUS(deepcopy(quadrantsXY),accept)
                #prunedXY10 = cluster_prune(deepcopy(clustersXY),.1)
                #prunedXY20 = cluster_prune(deepcopy(clustersXY),.2)
                #prunedXY30 = cluster_prune(deepcopy(clustersXY),.3)
                PerformIDEACluster(deepcopy(clustersXY), deepcopy(testLogXY), title,"IDEACLUSTER-"+str(accept)+"-logXY")
                #PerformIDEACluster(deepcopy(prunedXY10), deepcopy(testLogXY), title,"IDEACLUSTER-PRUNED10%-"+str(accept)+"-logXY")
                #PerformIDEACluster(deepcopy(prunedXY20), deepcopy(testLogXY), title,"IDEACLUSTER-PRUNED20%-"+str(accept)+"-logXY")
                #PerformIDEACluster(deepcopy(prunedXY30), deepcopy(testLogXY), title,"IDEACLUSTER-PRUNED30%-"+str(accept)+"-logXY")
                del clustersXY#, prunedXY10, prunedXY20, prunedXY30

            del quadrants
            del trainLogX, testLogX, quadrantsX
            del trainLogY, testLogY, quadrantsY
            del trainLogXY, testLogXY, quadrantsXY
                
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
