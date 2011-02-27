#!/usr/bin/env python

import argparse
import random
from copy import deepcopy
from math import floor,log
from arff import *
from cliffBORE import *
from discretize import *
from statistics import *


def main():
    args = parse_options()
    try:
        arff = Arff(args.data[0])
    except:
        print "File could not be loaded."
        return

    if (args.log == True):
        data = log_data(arff.data)
    if (args.equalwidth != 0):
        data = equal_width_discretize(arff.data,args.equalwidth[0])
    elif (args.equalfreq != 0):
        data = equal_width_discretize(arff.data,args.equalfreq[0])
    else:
        data = arff.data

    klasses = []
    
    for datum in data:
        if datum[-1] not in klasses:
            klasses.append(datum[-1])

    setname = args.data[0]
    print "Set: "+setname

    Oracle = []
    Train = []
    Test = []

    [Oracle, Test] = stratified_cross_val(data,[3,1])

    N_alpha = 50
    
    for i in range(N_alpha/len(klasses)):
        for j in range(len(klasses)):
            Train.append(random.choice(filter(lambda datum: datum[-1] == klasses[j],Oracle)))

    while True:
        print "\n"
        print "N: "+ str(len(Train)) + (" (All)" if len(Oracle) == 0 else "")
        print "Oracle (Remaining): "+str(len(Oracle))+"\tTrain: "+str(len(Train))+"\tTest: "+str(len(Test))
        PrintHeaderLine()
        if (args.cliff):
            TrainSet = CliffBORE(deepcopy(Train))
            PerformBaseline(TrainSet,Test,setname.split('/')[-1],"nb+cliff",True)
        elif (args.equalfreq != 0):
            PerformBaseline(Train,Test,setname.split('/')[-1],"nb+eqfq"+str(args.equalfreq[0])+"\t",True)
        elif(args.equalwidth != 0):
            PerformBaseline(Train,Test,setname.split('/')[-1],"nb+eqwd"+str(args.equalwidth[0])+"\t",True)
        else:
            PerformBaseline(Train,Test,setname.split('/')[-1],"nb\t",False)

        if (len(Oracle) == 0):
            break
        elif (len(Oracle) < N_alpha):
            ToAdd = len(Oracle)
        else:
            ToAdd = N_alpha

        for j in range(ToAdd):
            RandomItem = random.choice(Train)
            Closest = DiscreteClosestTo(RandomItem, GetInstancesForOtherClass(RandomItem[-1],Train))
            NewPoint = DiscreteClosestTo(Midpoint(RandomItem,Closest),Oracle)
            Oracle.remove(NewPoint)
            Train.append(NewPoint)

def log_data(data):
    for datum in data:
        for i in range(len(datum) - 1):
            datum[i] = log(datum[i] + 0.00001)
    return data

def stratified_cross_val(data, option):
    if type(data[0][-1]) is str:
        data = sorted(data,key=lambda instance: instance[-1])
    train_count = 0
    test_count = 0
    train = []
    test = []
    for datum in data:
        if train_count < option[0]:
            train_count = train_count + 1
            train.append(datum)
        elif test_count < option[1]:
            test_count = test_count + 1
            test.append(datum)
        if train_count == option[0] and test_count == option[1]:
            train_count = 0
            test_count = 0
    return train,test

def Midpoint(x,y):
    result = []
    for i in range(len(x) - 1):
        result.append( int((x[i] + y[i]) / 2) )
    return result

def GetInstancesForOtherClass(klass,data):
    return filter(lambda instance: instance[-1] != klass, data)

def GetInstancesPerClass(data):
    Classes = []
    for instance in data:
        if instance[-1] not in Classes:
            Classes.append(instance[-1])
    return map(lambda klass: filter(lambda instance: instance[-1] == klass, data), Classes)

def DiscreteClosestTo(this,these):
    result = []
    for that in these:
        distance = 0
        for i in range(len(this) - 1):
            if this[i] != that[i]:
                distance = distance + 1
        result.append([that,distance])
    return sorted(result, key=lambda instance: instance[1])[0][0]

def parse_options():
    """Place new options that you would like to be parsed here."""
    parser = argparse.ArgumentParser(description='Perform Active Learning on given train and test sets.')
    parser.add_argument('--data',
                        dest='data',
                        metavar='FILE',
                        type=str,
                        nargs=1,
                        help='Specify arff file from which to construct the training set.')
    parser.add_argument('--log',
                        dest='log',
                        default=False,
                        action='store_true',
                        help='Enable logging on numeric data before discretization.')
    parser.add_argument('--equalwidth',
                        dest='equalwidth',
                        metavar='N',
                        type=int,
                        nargs=1,
                        default=0,
                        help='Specify number of bins for equal-width discretization.')
    parser.add_argument('--equalfreq',
                        dest='equalfreq',
                        metavar='N',
                        type=int,
                        nargs=1,
                        default=0,
                        help='Specify number of bins for equal-frequency discretization.')
    parser.add_argument('--cliff',
                        dest='cliff',
                        default=False,
                        action='store_true',
                        help='Enable cliff instance selection on the training test.')
    return parser.parse_args()

if __name__ == "__main__":
    main()
