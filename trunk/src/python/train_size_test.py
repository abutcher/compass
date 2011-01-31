#!/usr/bin/env python

import argparse
from arff import *
from statistics import *
from gridclus2 import *
from instance import *
from quadrant import *
from util import *
from copy import deepcopy
from bore2 import *
from NaiveBayes import *
import random

def main():
    random.seed(2)
    
    args = parse_options()

    title = args.train.split("/")[-1].split(".")[0]
    arff = Arff(args.train)

    dc = DataCollection(discretize(arff.data))
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    k_fold = ic.k_fold_stratified_cross_val(4)
    trains = [50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 650, 600, 700, 750, 800]#, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400, 1450, 1500]
    total_score_list = []

    for i in range(len(trains)):
        score_list = []
        train_grab_bag = squash(k_fold[1:-1])
        test = k_fold[0]
        
        for j in range(args.xval):
            random.shuffle(train_grab_bag, random.random)
            train = train_grab_bag[0:trains[i]]
            
            trainXY = log_y(log_x(deepcopy(train)))
            testXY = log_y(log_x(deepcopy(test)))
            
            quadrants = QuadrantTree(trainXY).leaves()
            clusters = GRIDCLUS(quadrants, args.accept)
            
            clusters, culled_clusters = prune_clusters_classic(deepcopy(clusters), args.cull)
            
            score = DefectStats()
            for instance in testXY:
                closest_cluster = [sys.maxint, None]
                for i in range(len(clusters)):
                    for quadrant in clusters[i].quadrants:
                        tmp_distance = distance(instance.Coord(), quadrant.center())
                        if tmp_distance < closest_cluster[0]:
                            closest_cluster[0] = tmp_distance
                            closest_cluster[1] = i
                            
                modified_train = []
                for quadrant in clusters[closest_cluster[1]].quadrants:
                    modified_train.extend(quadrant.ClassCoords())

                got = classify(instance.Coord(), modified_train, "DEFECT")
                score.Evaluate(got, instance.klass())
            score_list.append(score.HarmonicMean("TRUE"))
        del trainXY, testXY, train, quadrants, clusters, culled_clusters, score
        total_score_list.append(score_list)

    print len(trains)
    print len(total_score_list)
    for i in range(len(trains)):
        print "%.2f\t%.2f" % (trains[i], median(total_score_list[i]))

            
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
    parser.add_argument('--rules',
                        dest='rules',
                        metavar='INT',
                        type=int,
                        help='Specify the number of rules to generate from CULLED.')
    parser.add_argument('--trainsize',
                        dest='trainsize',
                        metavar='INT',
                        type=int,
                        help='Specify the size of train.')
    parser.add_argument('--xval',
                        dest='xval',
                        metavar='INT',
                        type=int,
                        help='Specify the number of x v x cross validations.')


    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
