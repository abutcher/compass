#!/usr/bin/env python

import argparse
from arff import *
from gac import *
import numpy
from util import *
from statistics import *
import random

def classify(instance, tree):
    if type(instance[-1]) is str:
        t = "DEFECT"
    else:
        t = "EFFORT"
    def walk(node):
        if (node.variance < node.weightedvariance()) or (node.left == None and node.right == None):
            if t == "DEFECT":
                return node.majorityclass()
            else:
                return numpy.median(transpose(node.data)[-1])                
        else:
            if node.right == None or node.left == None:
                if node.right == None:
                    return walk(node.left)
                else:
                    return walk(node.right)
            else:
                if node.right.weightedvariance() > node.left.weightedvariance():
                    return walk(node.left)
                else:
                    return walk(node.right)
    return walk(tree)

def main ():
    parser = argparse.ArgumentParser(description="Perform classifications using the TEAK method on a GAC tree")
    parser.add_argument('--arff', dest='arff', metavar='FILE', type=str, help='Specify arff file to use.')
    parser.add_argument('--loo', dest='loo', default=False, action='store_true', help='Enables leave one out cross-validation')
    parser.add_argument('--xval', dest='xval', default=None, type=int, help='Enables stratified cross-validation.  Accepts a number to indicate the number of folds of the cross-validation.')
    options = parser.parse_args()

    if options.arff == None:
        print "Your options don't please us."
        print "Reformatting /dev/sda1"
        exit(0)

    data = Arff(options.arff)
    random.shuffle(data.data,random.random)

    if options.xval is not None:
        MDMRE=[]
        probd=[]
        probf=[]
        HarmonicMeans=[]
        Sets = kFoldStratifiedCrossVal(data,options.xval)
        for i in range(len(Sets)):
            train = Sets[0]
            Sets.remove(train)
            test = []
            for i in range(len(Sets)):
                test.extend(Sets[i])
                
            if type(test[0][-1]) is str:
                Stats = DefectStats()
                for instance in test:
                    tree = gac(train)
                    tree.varianceprune(1.1,1.1)
                    Got = classify(instance, tree.nodes[0])
                    Want = instance[-1]
                    if Got == Want:
                        if Got == "true":
                            Stats.incf("TRUE","d")
                            Stats.incf("FALSE","a")
                        elif Got == "false":
                            Stats.incf("FALSE","d")
                            Stats.incf("TRUE","a")
                    elif Got != Want:
                        if Got == "true":
                            Stats.incf("TRUE","c")
                            Stats.incf("FALSE","b")
                        elif Got == "false":
                            Stats.incf("FALSE","c")
                            Stats.incf("TRUE","b")
                probd.append(Stats.pd("TRUE"))
                probf.append(Stats.pf("TRUE"))
                HarmonicMeans.append(Stats.HarmonicMean("TRUE"))
            else:
                MREs = []
                for datum in test:
                    tree = gac(train)
                    tree.varianceprune(1.1,1.1)
                    predicted = classify(datum, tree.nodes[0])
                    MREs.append(MRE(datum[-1], predicted))
                MDMRE.append(np.median(MREs))


            Sets.append(train)
            del train, test
        
        if len(MDMRE) is not 0:
            print MDMRE
            print median(MDMRE)
        else:
            print "pd"
            print probd
            print "PF"
            print probf
            print "Harmonic Means"
            print HarmonicMeans

    elif options.loo is True:
        if type(data.data[0][-1]) is str:
            Stats = DefectStats()
            for datum in data.data:
                DataSet = list(data.data)
                DataSet.remove(datum)
                tree = gac(DataSet)
                tree.varianceprune(1.1,1.1)
                Got = classify(datum, tree.nodes[0])
                Want = datum[-1]
                if Got == Want:
                    if Got == "true":
                        Stats.incf("TRUE","d")
                        Stats.incf("FALSE","a")
                    elif Got == "false":
                        Stats.incf("FALSE","d")
                        Stats.incf("TRUE","a")
                elif Got != Want:
                    if Got == "true":
                        Stats.incf("TRUE","c")
                        Stats.incf("FALSE","b")
                    elif Got == "false":
                        Stats.incf("FALSE","c")
                        Stats.incf("TRUE","b")
            print "pd"
            print Stats.pd("FALSE")
            print "pf"
            print Stats.pf("FALSE")
            print "HarmonicMean"
            print Stats.HarmonicMean("FALSE")

        else:
            MREs = []
            for datum in data.data:
                DataSet = list(data.data)
                while True:
                    if datum in DataSet:
                        DataSet.remove(datum)
                    else:
                        break
                tree = gac(DataSet)
                tree.varianceprune(1.1,1.1)
                predicted = classify(datum, tree.nodes[0])
                MREs.append(MRE(datum[-1],predicted))
            print np.median(MREs)
        
if __name__=='__main__':
    main()
