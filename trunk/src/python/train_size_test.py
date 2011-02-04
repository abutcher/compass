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
import math

def main():
    random.seed(1)
    
    args = parse_options()

    title = args.train.split("/")[-1].split(".")[0]
    arff = Arff(args.train)

    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    k_fold = ic.k_fold_stratified_cross_val(4)
    train_grab_bag = squash(k_fold[1:-1])
    idea_hm_list = [[] for i in range(int(math.ceil(len(train_grab_bag)/50))+1)]
    idea_pd_list = [[] for i in range(int(math.ceil(len(train_grab_bag)/50))+1)]    
    idea_pf_list = [[] for i in range(int(math.ceil(len(train_grab_bag)/50))+1)]
    idea_prec_list = [[] for i in range(int(math.ceil(len(train_grab_bag)/50))+1)]    
    nb_hm_list = [[] for i in range(int(math.ceil(len(train_grab_bag)/50))+1)]
    nb_pd_list = [[] for i in range(int(math.ceil(len(train_grab_bag)/50))+1)]
    nb_pf_list = [[] for i in range(int(math.ceil(len(train_grab_bag)/50))+1)]
    nb_prec_list = [[] for i in range(int(math.ceil(len(train_grab_bag)/50))+1)]

    for x in range(args.xval):
        print "X: %d" % (x)
        train_grab_bag = squash(k_fold[1:-1])
        test = k_fold[0]    
        random.shuffle(train_grab_bag)

        train = []
        c = 0
        while len(train_grab_bag) > 0:
            idea_score = DefectStats()
            nb_score = DefectStats()
            if len(train_grab_bag) > 50:
                train.extend(train_grab_bag[0:50])
                for i in range(50):
                    train_grab_bag.remove(train_grab_bag[0])
            else:
                train.extend(train_grab_bag)
                for i in range(len(train_grab_bag)):
                    train_grab_bag.remove(train_grab_bag[0])

            print "TRAIN: %d" % (len(train))
            trainXY = log_y(log_x(deepcopy(train)))
            testXY = log_y(log_x(deepcopy(test)))
            
            quadrants = QuadrantTree(trainXY).leaves()
            clusters = GRIDCLUS(quadrants, args.accept)
            
            clusters, culled_clusters = prune_clusters_classic(deepcopy(clusters), args.cull)
            
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
                idea_score.Evaluate(got, instance.klass())
                got = classify(instance.datum, [inst.datum for inst in train], "DEFECT")
                nb_score.Evaluate(got, instance.klass())
            idea_hm_list[c].append(idea_score.HarmonicMean("TRUE"))
            idea_pd_list[c].append(idea_score.pd("TRUE"))
            idea_pf_list[c].append(idea_score.pf("TRUE"))
            idea_prec_list[c].append(idea_score.precision("TRUE"))            
            nb_hm_list[c].append(nb_score.HarmonicMean("TRUE"))
            nb_pd_list[c].append(nb_score.pd("TRUE"))
            nb_pf_list[c].append(nb_score.pf("TRUE"))
            nb_prec_list[c].append(nb_score.precision("TRUE"))            
            c += 1

    train_grab_bag = squash(k_fold[1:-1])        
    print "SIZE\tHM_TRUE\tPD_TRUE\tPF_TRUE\tPREC_TRUE"
    for i in range(len(idea_hm_list)):
        if idea_hm_list[i] != []:
            if i+1 * 50 > len(train_grab_bag):
                print "%.2f\t%.2f\t%.2f\t%.2f\t%.2f" % (len(train_grab_bag), median(idea_hm_list[i]), median(idea_pd_list[i]), median(idea_pf_list[i]), median(idea_prec_list[i]))
            else:
                print "%.2f\t%.2f\t%.2f\t%.2f\t%.2f" % ((i+1)*50, median(idea_hm_list[i]), median(idea_pd_list[i]), median(idea_pf_list[i]), median(idea_prec_list[i]))

    print "SIZE\tHM_TRUE\tPD_TRUE\tPF_TRUE\tPREC_TRUE"
    for i in range(len(nb_hm_list)):
        if nb_hm_list[i] != []:
            if i+1 * 50 > len(train_grab_bag):
                print "%.2f\t%.2f\t%.2f\t%.2f\t%.2f" % (len(train_grab_bag), median(nb_hm_list[i]), median(nb_pd_list[i]), median(nb_pf_list[i]), median(nb_prec_list[i]))
            else:
                print "%.2f\t%.2f\t%.2f\t%.2f\t%.2f" % ((i+1)*50, median(nb_hm_list[i]), median(nb_pd_list[i]), median(nb_pf_list[i]), median(nb_prec_list[i]))            
            
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
