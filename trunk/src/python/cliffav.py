import argparse
import random
from math import floor
from arff import *
from cliffBORE import *
from discretize import *
from statistics import *


def main():
    args = parse_options()
    arff = Arff(args.train)
    data = discretize(arff.data)

    klasses = []
    
    for datum in data:
        if datum[-1] not in klasses:
            klasses.append(datum[-1])

    setname = args.train[0]
    print "Set: "+setname

    Oracle = []
    Train = []
    Test = []

    [Oracle, Test] = stratified_cross_val(data,[4,1])

    N_alpha = 50
    
    for i in range(N_alpha/len(klasses)):
        for j in range(len(klasses)):
            Train.append(random.choice(filter(lambda datum: datum[-1] == klasses[j],Oracle)))

    while True:
        print "\n"
        print "N: "+ str(len(Train)) + (" (All)" if len(Oracle) == 0 else "")
        print "Oracle (Remaining): "+str(len(Oracle))+"\tTrain: "+str(len(Train))+"\tTest: "+str(len(Test))
        PrintHeaderLine()
        PerformBaseline(Train,Test,setname.split('/')[-1],"nb\t",True)
        TrainingPrototypes = CliffBORE(Train)
        PerformBaseline(TrainingPrototypes.prototypes,Test,setname.split('/')[-1],"nb+cliff",True)
        Perform1NN(TrainingPrototypes.prototypes,Test,setname.split('/')[-1],"1NN+cliff")

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
    parser = argparse.ArgumentParser(description='Perform IDEA on given train and test sets.')
    parser.add_argument('--train',
                        dest='train',
                        metavar='FILE',
                        type=str,
                        nargs='+',
                        help='Specify arff file[s] from which to construct the training set.')
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()