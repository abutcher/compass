#!/usr/bin/env python

import argparse
from arff import *
from statistics import *
from gridclus2 import *
from instance import *
from quadrant import *
from util import *
from copy import deepcopy
from bore import *
from NaiveBayes import *

def main():
    args = parse_options()

    title = args.train.split("/")[-1].split(".")[0]
    arff = Arff(args.train)

    print "dataset, %s" % (title)

    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    train, test = chopInTwo(ic.instances)

    trainXY = log_y(log_x(deepcopy(train)))
    testXY = log_y(log_x(deepcopy(test)))

    quadrants = QuadrantTree(trainXY).leaves()
    clusters = GRIDCLUS(quadrants, args.accept)

    clusters, culled_clusters = prune_clusters_classic(deepcopy(clusters), args.cull)

    print len(clusters)
    print len(culled_clusters)

    print len(squash([clus.datums() for clus in clusters]))
    print len(squash([clus.datums() for clus in culled_clusters]))

    print "Kept"
    Bore(squash([clus.datums() for clus in clusters]), arff.headers, "trueyes")
    print ""
    print "Culled"
    Bore(squash([clus.datums() for clus in culled_clusters]), arff.headers, "trueyes")

    
def parse_options():
    """Place new options that you would like to be parsed here."""
    parser = argparse.ArgumentParser(description='Perform IDEA on given train and test sets.')
    parser.add_argument('--train',
                        dest='train',
                        metavar='FILE',
                        type=str,
                        help='Specify arff file[s] from which to construct the training set.')
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
    parser.add_argument('--cull',
                        dest='cull',
                        default=None,
                        metavar='FLOAT 0-1',
                        type=float,
                        help='Specify the percentage of clusters to cull.')
    parser.add_argument('--xval',
                        dest='xval',
                        metavar='INT',
                        type=int,
                        help='Specify the number of folds for cross validation.')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
