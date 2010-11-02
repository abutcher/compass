#!/usr/bin/env python

from arff import *
import argparse
import sys
from NaiveBayes import *
from cluster import *
from statistics import *
from knn import *
from util import *
import random

def main():
    ErrorState = "n"

    parser = argparse.ArgumentParser(description='Script that collectives baseline statistics using Naive Bayes for defect sets and k-NN for effort sets without the use of clustering provided by IDEA/Compass.')

    parser.add_argument('--data', dest='data',default = None, metavar='FILE', type=str, nargs='+', help='Specify the arff file(s) from which to load data.')
    parser.add_argument('--loo', dest='loo', default=False, action='store_true', help='Enables leave one out cross-validation')
    parser.add_argument('--xval', dest='xval', default=None, type=int, help='Enables stratified cross-validation.  Accepts a number to indicate the number of folds of the cross-validation.')
    parser.add_argument('--k', dest='k', default=10, type=int, help='Specify the value k for nearest neighbors.')

    options = parser.parse_args()

    if options.loo == False and options.xval is None:
        print "Must specify either stratified cross validation (--xval) or Leave-one-out (-loo)."
        ErrorState = 'y'

    if options.data is None or len(options.data) is 0:
        print "Must provide one or more files for input."
        ErrorState = 'y'

    if ErrorState == 'y':
        print "Aborting."
        sys.exit(-1)

    data = Arff(options.data)
    random.shuffle(data.data,random.random)

    if options.xval is not None:
        MDMRE = []
        HarmonicMeans = []
        Sets = kFoldStratifiedCrossVal(data,options.xval)
        for i in range(len(Sets)):
            # Pop the first item on the list off as the test.
            train = Sets[0]
            Sets.remove(train)
            test = []
            for i in range(len(Sets)):
                test.extend(Sets[i])
                
            if type(test[0][-1]) is str:
                Stats = DefectStats()
                for instance in test:
                    Got = Classify(instance, train, "DEFECT")
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
                            Stats.incf("FALSE","b")
                            Stats.incf("TRUE","c")
               #return [Stats.pd("TRUE"),Stats.pf("TRUE")]
                HarmonicMeans.append(Stats.HarmonicMean("TRUE"))

            else:
                MREs = []                    
                for datum in test:
                    predicted = median(kNearestNeighbors(datum, train, options.k))
                    MREs.append(MRE(datum[-1],predicted))
                MDMRE.append(median(MREs))
                
            
            # Append it back on the end before we continue with the cross-validation.
            Sets.append(test)

        if len(MDMRE) is not 0:
            print MDMRE
            print median(MDMRE)
            
        
    elif options.loo is True:
        if type(data.data[0][-1]) is str:
            print "do defect sets"

        else:
            MREs = []
            for datum in data.data:
                predicted = median(kNearestNeighbors(datum, data.data, options.k))
                MREs.append(MRE(datum[-1],predicted))
            print median(MREs)
        
    else:
        print "Not really sure what to do now. Aborting."
        sys.exit(-1)
    

if __name__ == '__main__':
    main()
