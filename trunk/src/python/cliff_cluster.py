#!/usr/bin/env python

import argparse
from arff import *
from instance import *
from quadrant import *
from copy import deepcopy
from NaiveBayes import *
from discretize2 import *
from cliffBORE import *
from util import *
from statistics import *
from gridclus2 import *

def main():
    # Control our seed for testing purposes.
    random.seed(1)

    # Function to parse the command line options.
    args = parse_options()

    title = args.train.split("/")[-1].split(".")[0]
    arff = Arff(args.train)

    print "dataset, %s" % (title)

    # Compute the projected 2 dimensional coordinates and normalize.
    dc = DataCollection(arff.data)
    ic = InstanceCollection(dc)
    ic.normalize_coordinates()

    # Set our sampling size for each round.
    N_alpha = 50

    # Discover the unique classes for each.
    klasses = []
    for inst in ic.instances:
        if inst.klass() not in klasses:
            klasses.append(inst.klass())

    # Separate the data into two parts.  75% to the oracle, 25% left over as a test set.    
    k_fold = ic.k_fold_stratified_cross_val(4)

    test = deepcopy(k_fold[0])
    oracle = deepcopy(squash(k_fold[1:-1]))
    train = []
    
    # Randomly place an equal amount of klasses in the train set to make sure we have classes to find the decision boundary with (especially necessary with smaller sets).
    for i in range(N_alpha/len(klasses)):
        for j in range(len(klasses)):
            train.append(random.choice(filter(lambda datum: datum.klass() == klasses[j],oracle)))

    # Break when we have no more items in the oracle.
    while True:
        print "\n"
        print "N: "+ str(len(train)) + (" (All)" if len(oracle) == 0 else "")
        print "Oracle (Remaining): "+str(len(oracle))+"\tTrain: "+str(len(train))+"\tTest: "+str(len(test))

        # Perform equal frequency discretization required by cliff (see discretize2.py)
        DiscOb = CliffDiscretize([inst.datum for inst in train], args.BINS)

        # Get our reduced training set back from Cliff.
        cliffTrain = DiscOb.GetQualifyingInstances(CliffBORE(DiscOb.DiscretizeSet(deepcopy([inst.datum for inst in train]))).prototypes,deepcopy([inst.datum for inst in train]))
        
        # Using the prototypes we received back from Cliff, we find the instance objects in our train data.  Not necessary if you don't use our instance object
        # See instance.py
        cliffInstTrain = []
        for inst in train:
            if inst.datum in cliffTrain:
                cliffInstTrain.append(inst)

        # Log the two dimensional coordinates.
        cliffInstTrainXY = log_y(log_x(deepcopy(cliffInstTrain)))
        testXY = log_y(log_x(deepcopy(test)))

        # Create a QuadrantTree (k2-tree) and cluster them using the GRIDCLUS algorithm.
        quadrants = QuadrantTree(deepcopy(cliffInstTrainXY)).leaves()
        clusters = GRIDCLUS(quadrants, args.accept)

        # Prepare a DefectStats object to perform evaluation and scoring.
        cliffNB = DefectStats()
    
        # Classification stuff
        for instance in testXY:
            # Find closest cluster to test instance.
            closest_cluster = [sys.maxint, None]
            for i in range(len(clusters)):
                for quadrant in clusters[i].quadrants:
                    tmp_distance = distance(instance.Coord(), quadrant.center())
                    if tmp_distance < closest_cluster[0]:
                        closest_cluster[0] = tmp_distance
                        closest_cluster[1] = i
                    
            # Points and associated classes for the closest cluster's quadrants.
            modified_train = []
            for quadrant in clusters[closest_cluster[1]].quadrants:
                modified_train.extend(quadrant.ClassCoords())
            
            # Classify the test instance
            got = classify(instance.Coord(), modified_train, "DEFECT")
            want = instance.datum[-1]
            # Increment the stat object appropriately using Evaluate
            cliffNB.Evaluate(got, want)

        cliffNB.StatsLine(title, "Cliff+NB")

        # Decide how many items to add from the oracle.  If we don't have any left, we break from the loop and exit the program.
        if (len(oracle) == 0):
            break
        elif (len(oracle) < N_alpha):
            ToAdd = len(oracle)
        else:
            ToAdd = N_alpha

        # Sample N_alpha points to add to the training set.
        # We sample by:
        # Pick a random point in the train set.
        # Find the closest point belonging to the other class
        # Find the midpoint of these two points and ask the oracle for the point closest to this midpoint.
        for j in range(ToAdd):
            RandomItem = random.choice(train)
            Closest = closest_to(RandomItem,GetInstancesForOtherClass(RandomItem.klass(),train))
            NewPoint = closest_to(Midpoint(RandomItem,Closest),oracle)
            oracle.remove(NewPoint)
            train.append(NewPoint)

def Midpoint(x,y):
    result = []
    for i in range(len(x.Coord()) - 1):
        result.append( int( (x.Coord()[i] + y.Coord()[i]) / 2 ))
    return result

def closest_to(this,these):
    result = []
    for that in these:
        distance = 0
        try:
            for i in range(len(this.Coord()) - 1):
                distance = distance + (this.Coord()[i] - that.Coord()[i])
            result.append([that,distance])
        except AttributeError:
            for i in range(len(this)):
                distance = distance + (this[i] - that.Coord()[i])
            result.append([that,distance])
    return sorted(result, key=lambda instance: instance[1])[0][0]

def GetInstancesForOtherClass(klass,data):
    return filter(lambda instance: instance.klass() != klass, data)

def parse_options():
    """Place new options that you would like to be parsed here."""
    parser = argparse.ArgumentParser(description='Perform IDEA on given train and test sets.')
    parser.add_argument('--train',
                        dest='train',
                        metavar='FILE',
                        type=str,
                        help='Specify arff file[s] from which to construct the training set.')
    parser.add_argument('--bins',
                        dest='BINS',
                        metavar='BINS',
                        type=int,
                        help='Specify the number of bins')
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
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    main()
