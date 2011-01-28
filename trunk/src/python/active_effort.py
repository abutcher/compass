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
from discretize import *
from eras import *


def main():
    random.seed(1)
    
    args = parse_options()
    
    title = args.train.split("/")[-1].split(".")[0]
    arff = Arff(args.train)

    print "dataset=%s" % (title)

    dc = DataCollection(discretize(arff.data))
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    for i in range(10): # Hard coded 10 x 3
        k_fold = ic.k_fold_stratified_cross_val(3)
        
        for j in range(3):
            test = squash(k_fold[0:1])
            train = k_fold[2]
            
            trainXY = log_y(log_x(deepcopy(train)))
            testXY = log_y(log_x(deepcopy(test)))

            quadrants = QuadrantTree(trainXY).leaves()
            clusters = GRIDCLUS(quadrants, args.accept)

            clusters, culled_clusters = prune_clusters_classic(deepcopy(clusters), args.cull)
            
            # Step through instances one at a time
            for instance in testXY:
                if instance_in_culled(instance, culled_clusters):
                    # Place in closest cluster with an effort score within 10% of it's original
                    closest_cluster = [sys.maxint, None, None]
                    for k in range(len(clusters)):
                        for quadrant in clusters[k].quadrants:
                            tmp_distance = distance(instance.Coord(), quadrant.center())
                            if tmp_distance < closest_cluster[0]:
                                closest_cluster[0] = tmp_distance
                                closest_cluster[1] = k
                                closest_cluster[2] = quadrant
                        # Guess from oracle
                        instance.datum[-1] = instance.klass() + instance.klass() * (random.randint(10)/10) * random_element([-1, 1])
                        # Place in closest cluster, quadrant instances
                        clusters[k].quadrants[index(closes_cluster[2])].instances.append(instance)
                else:
                    closest_cluster = [sys.maxint, None]
                    for k in range(len(clusters)):
                        for quadrant in clusters[k].quadrants:
                            tmp_distance = distance(instance.Coord(), quadrant.center())
                            if tmp_distance < closest_cluster[0]:
                                closest_cluster[0] = tmp_distance
                                closest_cluster[1] = k

                    got = median([inst.klass() for inst in cluster.instances()])
                    want = instance.klass()
                    # Do some scoring here
                
def instance_in_culled(instance, culled_clusters):
    within = False
    for cluster in culled_clusters:
        for quadrant in cluster.quadrants:
            if instance.coord.x > quadrant.xmin and instance.coord.x < quadrant.xmax and instance.coord.y > quadrant.ymin and instance.coord.y < quadrant.ymax:
                within = True
    return within


            
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

            
