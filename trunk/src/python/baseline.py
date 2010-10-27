#!/usr/bin/env python

from arff import *
import argparse
import sys
import NaiveBayes
import knn
from util import kFoldStratifiedCrossVal, SortByClass

def main():
    ErrorState = "n"

    parser = argparse.ArgumentParser(description='Script that collectives baseline statistics using Naive Bayes for defect sets and k-NN for effort sets without the use of clustering provided by IDEA/Compass.')

    parser.add_argument('--data', dest='data',default = None, metavar='FILE', type=str, nargs='+', help='Specify the arff file(s) from which to load data.')
    parser.add_argument('--loo', dest='loo', default=False, action='store_true', help='Enables leave one out cross-validation')
    parser.add_argument('--xval', dest='xval', default=None, type=int, help='Enables stratified cross-validation.  Accepts a number to indicate the number of folds of the cross-validation.')

    options = parser.parse_args()

    if options.loo == False and options.xval is None:
        print "Must specify either stratified cross validation (--xval) or Leave-one-out (-loo).')
        ErrorState = 'y'

    if options.data is None or len(options.data) is 0:
        print "Must provide one or more files for input."
        ErrorState = 'y'

    if ErrorState == 'y':
        print "Aborting."
        sys.exit(-1)

    data = Arff(options.train)

    if options.xval is not None:
        Sets = kFoldStratifiedCrossVal(data,options.xval)
        for i in len(Sets):
            # Pop the first item on the list off as the test.
            test = Sets[0]
            Sets.remove(test)

            # Append it back on the end before we continue with the cross-validation.
            Sets.append(test)
            
        
    elif options.loo is True:
        print "Do leave-one-out."
    else:
        print "Not really sure what to do now. Aborting."
        sys.exit(-1)
    

if __name__ == '__main__':
    main()
