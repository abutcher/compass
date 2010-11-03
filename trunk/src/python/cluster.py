import csv
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
            Got = Classify(instance, Data, "DEFECT")
            Want = instance[len(instance)-1]
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
    
def writeClusters(clusters, filename):        
    csvWriter = csv.writer(open(filename + '.csv', 'wb'), delimiter=',')
    for cluster in clusters:
        for quadrant in cluster:
            for datum in quadrant.Datums():
                csvWriter.writerow(datum)
