from util import MRE
from knn import *
from NaiveBayes import *
from statistics import *

""" Statistics for a list of quadrants """

def PerformanceScore(cluster,Test=None):
    Data = []
    for quadrant in cluster:
        for instance in quadrant.Data:
            Data.append(instance.datum)
    if Test is None:
        Test = Data

    if type(Data[0][-1]) is str:
        # This is a DEFECT set, so we calculate PD/PF for the true class
        Stats = DefectStats()
        for instance in Test:
            print "instance"
            Got = Classify(instance, Data, "DEFECT")
            Want = instance[len(instance)-1]
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
        return Stats.HarmonicMean("TRUE")
    else:
        # This is likely an EFFORT set, so we calculate Median Magnitude of Relative Error
        MDMRE = []
        for instance in Data:
            Data.remove(instance)
            MDMRE.append(MRE(instance[-1],Classify(instance, Data, "EFFORT")))
            Data.append(instance)
        return median(MDMRE)

def Classify(instance, Data, InputType=None):
    if InputType is None:
        if type(Data[0][-1]) is str:
            InputType="DEFECT"
        else:
            InputType="EFFORT"
    if InputType == "DEFECT":
        Neighbors = kNearestNeighbors(instance,Data)
        return NaiveBayesClassify(instance,Neighbors)
    elif InputType == "EFFORT":
        Neighbors = kNearestNeighbors(instance,Data)
        return median(Neighbors)
    

        
