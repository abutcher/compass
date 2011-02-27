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

    cluster_stats = EffortStats() # Closest cluster classification
    quadrant_stats = EffortStats() # Closest quadrant classification
    tree_stats = EffortStats() # Trickle down tree classification
    onenn_stats = EffortStats() # 1nn classification
    
    for i in range(len(ic.instances)):
        train = deepcopy(ic.instances)
        test = train[i]
        train.remove(test)
            
        trainXY = log_y(log_x(train))
        testXY = log_y(log_x(test))

        qtree = QuadrantTree(trainXY)
        quadrants = qtree.leaves()
        clusters = GRIDCLUS(quadrants, args.accept)
        #clusters, culled_clusters = prune_clusters_classic_within_max(deepcopy(clusters), args.cull, "VARIANCE")

        #qtree.prune(1.1, 1.1)

        # Trickle down tree classification
        tree_stats.Evaluate(qtree.classify(ic.instances[i]), testXY.klass())


        # Closest cluster classification
        closest_cluster = [sys.maxint, None]
        for cluster in clusters:
            for quadrant in cluster.quadrants:
                tmp_distance = distance(testXY.Coord(), quadrant.center())
                if tmp_distance < closest_cluster[0]:
                    closest_cluster[0] = tmp_distance
                    closest_cluster[1] = cluster
        cluster_stats.Evaluate(median(transpose(closest_cluster[1].datums())[-1]), testXY.klass())

        closest_datum = closest_to(testXY.datum, [inst.datum for inst in trainXY])
        onenn_stats.Evaluate(closest_datum[-1], testXY.klass())

        """
        maxv = -sys.maxint
        for quadrant in quadrants:
            if quadrant.qvariance() > maxv:
                maxv = quadrant.qvariance()

        print len(quadrants)
        quadrants = sorted(quadrants, key=lambda quadrant: quadrant.qvariance())

        quadrants = quadrants[0:int(len(quadrants)/4)]
                
        print len(quadrants)
        print ""
        """

        # Closest quadrant classification
        closest_quadrant = [sys.maxint, None]
        for quadrant in quadrants:
            tmp_distance = distance(testXY.Coord(), quadrant.center())
            if tmp_distance < closest_quadrant[0]:
                closest_quadrant[0] = tmp_distance
                closest_quadrant[1] = quadrant
        quadrant_stats.Evaluate(median(transpose(closest_quadrant[1].datums())[-1]), testXY.klass())

        #print "Default to the root node: %.2f" % qtree.root.qmedian()

        #print "Got: %.2f" % got
        #print "Want: %.2f" % testXY.klass()

    print "treatment, mdmre, pred25, pred30"
    print "closest_cluster, %.2f, %.2f, %.2f" % (cluster_stats.MDMRE(), cluster_stats.PRED25(), cluster_stats.PRED30())
    print "closest_quadrant, %.2f, %.2f, %.2f" % (quadrant_stats.MDMRE(), quadrant_stats.PRED25(), quadrant_stats.PRED30())
    print "tree_classification, %.2f, %.2f, %.2f" % (tree_stats.MDMRE(), tree_stats.PRED25(), tree_stats.PRED30())
    print "1nn, %.2f, %.2f, %.2f" % (onenn_stats.MDMRE(), onenn_stats.PRED25(), onenn_stats.PRED30())

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
    parser.add_argument('--cull',
                        dest='cull',
                        default=None,
                        metavar='FLOAT 0-1',
                        type=float,
                        help='Specify the percentage of clusters to cull.')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()

