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

    print "dataset, %s" % (title)
    #print "accept: %.2f" % (args.accept[0])
    print "treatment, pct_clus_culled, pct_data_culled, hm_true"

    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    for i in range(args.xval):
        k_fold = ic.k_fold_stratified_cross_val(args.xval)

        for by in range(args.xval):
            test = k_fold[0]
            k_fold.remove(test)
            train = squash(deepcopy(k_fold))
            k_fold.append(test)
    
            trainXY = log_y(log_x(deepcopy(train)))
            testXY = log_y(log_x(deepcopy(test)))

            quadrants = QuadrantTree(trainXY).leaves()
            clusters = GRIDCLUS(quadrants, args.accept[0])
        
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
                clusters[closest_cluster[1]].stats.Evaluate(got, want)

            global_pre_test = DefectStats()
            pre_test_stats = list(DefectStats() for cluster in clusters)
            global_nb = DefectStats()

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
        
                # Increment the stat object appropriately using Evaluate
                pre_test_stats[closest_cluster[1]].Evaluate(got, want)
                global_pre_test.Evaluate(got, instance.klass())

                global_nb.Evaluate(NaiveBayesClassify(instance.datum, [inst.datum for inst in trainXY], "DEFECT"), instance.klass())
                
            print "IDEA, 0, 0, %.2f" % (global_pre_test.HarmonicMean("TRUE"))
            print "NONE, 0, 0, %.2f" % (global_nb.HarmonicMean("TRUE"))
    
            culls = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
        
            for cull in culls:
                """
                culled_clusters = []
                for i in range(len(clusters)):
                    # Cull X % of the poorest performing clusters.
                    if i < int(math.ceil(len(clusters) * cull)) and len(clusters) > 1:
                        culled_clusters.append(clusters[i])
                        clusters.remove(clusters[i])
                """
                #clusters, culled_clusters = prune_rogue_clusters(deepcopy(clusters), cull)
                clusters, culled_clusters = prune_clusters_classic(deepcopy(clusters), cull)

                culled_datums = []
                for cluster in culled_clusters:
                    culled_datums.extend(cluster.datums())

                kept_datums = []
                for cluster in clusters:
                    kept_datums.extend(cluster.datums())

                pct = 100-(float(len(kept_datums)) / (float(len(culled_datums)) + float(len(kept_datums))))*100

                global_test = DefectStats()
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
                    global_test.Evaluate(got, want)

                print "IDEA-%.2f, %.2f, %.2f, %.2f" % (cull, cull, pct, global_test.HarmonicMean("TRUE"))

                for cluster in culled_clusters:
                    clusters.append(cluster)

    #fig = Figure(title, trainXY, quadrants, clusters)
    #fig.write_png()

    #print "Kept"
    #Bore(kept_datums, arff.headers, "trueyes")
    #print ""
    #print "Culled"
    #Bore(culled_datums, arff.headers, "trueyes")
    #print ""
    
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
    parser.add_argument('--xval',
                        dest='xval',
                        metavar='INT',
                        type=int,
                        help='Specify the number of folds for cross validation.')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
