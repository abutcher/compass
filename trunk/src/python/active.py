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

    print "dataset, %s" % (title)

    dc = DataCollection(discretize(arff.data))
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    era_list = ic.k_fold_stratified_cross_val(int(len(arff.data)/args.era))
    for era in era_list:
        era = log_y(log_x(deepcopy(era)))
    score_list = []

    train = deepcopy(squash(era_list[0:1]))
    era_list.remove(era_list[0])
    era_list.remove(era_list[0])
    
    for i in range(len(era_list)):
        quadrants = QuadrantTree(train).leaves()
        clusters = GRIDCLUS(quadrants, args.accept)

        clusters, culled_clusters = prune_clusters_classic(deepcopy(clusters), args.cull)
        
        #culled_rules = Bore(squash([clus.datums() for clus in culled_clusters]), arff.headers, "trueyes").top_rules(args.rules)

        if i != len(era_list)-1:
            score = DefectStats()
            for instance in era_list[i+1]:
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

        """
        if i != len(era_list)-1:
            removed_instances = [inst for inst in data_matching_rules(era_list[i+1], culled_rules)]

            for instance in removed_instances:
                era_list[i+1].remove(instance)
        """
        
        if i != len(era_list)-1:
            train = []
            for cluster in clusters:
                train.extend(cluster.instances())
            train.extend(era_list[i+1])
    print score_list


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
    parser.add_argument('--era',
                        dest='era',
                        metavar='INT',
                        type=int,
                        help='Specify the size of the eras.')


    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
