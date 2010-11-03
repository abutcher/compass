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
import numpy as np

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
        probd = []
        probf = []
        HarmonicMeans = []
        print len(data.data)
        Sets = kFoldStratifiedCrossVal(data,options.xval)
        for i in range(len(Sets)):
            print "tick"
            # Pop the first item on the list off as the test.
            test = Sets[0]
            Sets.remove(test)
            train = []
            for i in range(len(Sets)):
                train.extend(Sets[i])

            #print len(train)
            #print len(test)
                
            if type(test[0][-1]) is str:
                #print "desired"
                Stats = DefectStats()
                for instance in test:
                    Got = Classify(instance, train, "DEFECT")
                    Want = instance[-1]
                    #print Got
                    #print Want
                    if Got.lower() == Want.lower():
                        if Got.lower() == "true" or Got.lower() == "yes":
                            #print "true match"
                            Stats.incf("TRUE","d")
                            Stats.incf("FALSE","a")
                        elif Got == "false" or Got == "no":
                            #print "false match"
                            Stats.incf("FALSE","d")
                            Stats.incf("TRUE","a")
                    elif Got.lower() != Want.lower():
                        if Got.lower() == "true" or Got.lower() == "yes":
                            #print "got true mismatch"
                            Stats.incf("TRUE","c")
                            Stats.incf("FALSE","b")
                        elif Got == "false" or Got == "no":
                            #print "got false mismatch"
                            Stats.incf("FALSE","c")
                            Stats.incf("TRUE","b")
               #return [Stats.pd("TRUE"),Stats.pf("TRUE")]
                probd.append(Stats.pd("TRUE"))
                probf.append(Stats.pf("TRUE"))
                HarmonicMeans.append(Stats.HarmonicMean("TRUE"))
                

            else:
                print "tick2"
                MREs = []                    
                for datum in test:
                    neighbors = kNearestNeighbors(datum,train, options.k)
                    predicted = np.median(transpose(neighbors)[-1])
                    MREs.append(MRE(datum[-1],predicted))   
                MDMRE.append(np.median(MREs))
                
            
            # Append it back on the end before we continue with the cross-validation.
            Sets.append(test)
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
                Got = Classify(datum, DataSet, "DEFECT")
                Want = datum[-1]
                if Got.lower() == Want.lower():
                    if Got.lower() == "true" or Got.lower() == "yes":
                        #print "true match"
                        Stats.incf("TRUE","d")
                        Stats.incf("FALSE","a")
                    elif Got == "false" or Got == "no":
                        #print "false match"
                        Stats.incf("FALSE","d")
                        Stats.incf("TRUE","a")
                elif Got.lower() != Want.lower():
                    if Got.lower() == "true" or Got.lower() == "yes":
                        #print "got true mismatch"
                        Stats.incf("TRUE","c")
                        Stats.incf("FALSE","b")
                    elif Got == "false" or Got == "no":
                        #print "got false mismatch"
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
                neighbors = kNearestNeighbors(datum,DataSet, options.k)
                predicted = np.median(transpose(neighbors)[-1])
                MREs.append(MRE(datum[-1],predicted))
            print np.median(MREs)
        
    else:
        print "Not really sure what to do now. Aborting."
        sys.exit(-1)
    

if __name__ == '__main__':
    main()
