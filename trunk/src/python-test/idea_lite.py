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


def main():
    args = parse_options()

    title = args.train[0].split("/")[-1].split(".")[0]
    arff = Arff(args.train[0])

    print "dataset: %s" % (title)
    print "accept: %.2f" % (args.accept[0])
    print "cull: %.2f" % (args.cull[0])

    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    train, test = ic.stratified_cross_val(args.stratified)

    trainXY = log_y(log_x(deepcopy(train)))
    testXY = log_y(log_x(deepcopy(test)))

    quadrants = QuadrantTree(trainXY).leaves()
    clusters = GRIDCLUS(quadrants, args.accept[0])

    train_stats = list((DefectStats(), cluster) for cluster in clusters)

    for instance in trainXY:

        # Find closest cluster to test instance.
        closest_cluster = [sys.maxint, None]
        for i in range(len(clusters)):
            for quadrant in clusters[i].quadrants:
                tmp_distance = distance(instance.Coord(), quadrant.center())
                if tmp_distance < closest_cluster[0]:
                    closest_cluster[0] = tmp_distance
                    closest_cluster[1] = i

        # Points and associated classes for the closest cluster's quadrants.
        modified_train = []
        for quadrant in clusters[closest_cluster[1]].quadrants:
            modified_train.extend(quadrant.ClassCoords())
        
        # Classify the test instance
        got = classify(instance.Coord(), modified_train, "DEFECT")
        want = instance.datum[-1]
        
        # Increment the stat object appropriately using Evaluate
        train_stats[closest_cluster[1]][0].Evaluate(got, want)

    print "size, hm"

    for i in range(len(train_stats)):
        print "%d, %.2f" % (len(clusters[i].datums()),train_stats[i][0].HarmonicMean("TRUE"))

    print ""

    train_stats = sorted(train_stats, key=lambda stat: stat[0].HarmonicMean("TRUE"))

    culled_clusters = []
    for i in range(len(train_stats)):
        # Cull X % of the poorest performing clusters.
        if i < int(math.ceil(len(train_stats) * args.cull[0])):
            culled_clusters.append(clusters[clusters.index(train_stats[i][1])])
            clusters.remove(clusters[clusters.index(train_stats[i][1])])

    culled_datums = []
    for cluster in culled_clusters:
        culled_datums.extend(cluster.datums())

    kept_datums = []
    for cluster in clusters:
        kept_datums.extend(cluster.datums())

    pct = 100-(float(len(kept_datums)) / (float(len(culled_datums)) + float(len(kept_datums))))*100

    print "Culled %.2f pct of the data." % (pct)

    print ""
    
    test_stats = list(DefectStats() for cluster in clusters)
    
    for instance in testXY:

        # Find closest cluster to test instance.
        closest_cluster = [sys.maxint, None]
        for i in range(len(clusters)):
            for quadrant in clusters[i].quadrants:
                tmp_distance = distance(instance.Coord(), quadrant.center())
                if tmp_distance < closest_cluster[0]:
                    closest_cluster[0] = tmp_distance
                    closest_cluster[1] = i

        # Points and associated classes for the closest cluster's quadrants.
        modified_train = []
        for quadrant in clusters[closest_cluster[1]].quadrants:
            modified_train.extend(quadrant.ClassCoords())
        
        # Classify the test instance
        got = classify(instance.Coord(), modified_train, "DEFECT")
        want = instance.datum[-1]
        
        # Increment the stat object appropriately using Evaluate
        test_stats[closest_cluster[1]].Evaluate(got, want)
        

    print "size, hm"
    for i in range(len(test_stats)):
        print "%d, %.2f" % (len(clusters[i].datums()),test_stats[i].HarmonicMean("TRUE"))

    print ""
    #fig = Figure(title, trainXY, quadrants, clusters)
    #fig.write_png()

    print "Kept"
    Bore(kept_datums, arff.headers, "trueyes")
    print ""
    print "Culled"
    Bore(culled_datums, arff.headers, "trueyes")
    print ""
    
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
    parser.add_argument('--cull',
                        dest='cull',
                        default=None,
                        metavar='FLOAT 0-1',
                        type=float,
                        nargs=1,
                        help='Specify the percentage of clusters to cull.')

    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
