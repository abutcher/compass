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

def main():
    random.seed(1)
    
    args = parse_options()
    
    title = args.train.split("/")[-1].split(".")[0]
    arff = Arff(args.train)

    print "dataset, %s" % (title)

    dc = DataCollection(discretize(arff.data)) # Discretize data.
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    train, test = ic.two_bins()

    trainXY = log_y(log_x(deepcopy(train)))
    testXY = log_y(log_x(deepcopy(test)))

    quadrants = QuadrantTree(trainXY).leaves()
    clusters = GRIDCLUS(quadrants, args.accept[0])

    clusters, culled_clusters = prune_clusters_classic(clusters, args.cull)

#    print "KEPT"
#    kept_rules =  Bore(squash([clus.datums() for clus in clusters]), arff.headers, "trueyes").top_rules(5)
#    print kept_rules
#    print ""
    print "Top %d collected from CULLED" % (args.rules)
    culled_rules = Bore(squash([clus.datums() for clus in culled_clusters]), arff.headers, "trueyes").top_rules(args.rules)
    print culled_rules
    print ""
    
    print "Collected %d instances that resembled CULLED out of the %d remaining instances."  % (len([inst.datum for inst in data_matching_rules(testXY, culled_rules)]), len(testXY))

def data_matching_rules(data, rules):
    matching_instances = []
    for instance in data:
        majority_vote = []
        for rule in rules:
            if instance_matches_rule(instance.datum, rule):
                majority_vote.append(True)
        if len(majority_vote) > len(rules)/2:
            matching_instances.append(instance)
    return matching_instances

def instance_matches_rule(inst, rule):
    i = int(rule.split(" ")[0])
    name = rule.split(" ")[1]
    value = int(rule.split(" ")[2])
    if inst[i] == value:
        return True
    else:
        return False
            
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
    parser.add_argument('--rules',
                        dest='rules',
                        metavar='INT',
                        type=int,
                        help='Specify the number of rules to generate from CULLED.')


    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
