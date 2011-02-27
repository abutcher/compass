from copy import deepcopy
from util import *
import math

class CliffBORE:

    def __init__(self,data):
        self.data = data
        self.classes = self.PopulateClasses(data)
        self.crits = self.GetCriteria(self.data)
        self.inst = self.GetInstancesPerClass(self.data)
        self.prototypes = self.ExtractUniques(map(self.SelectInstances,self.crits,self.inst))
        

    def GetCriteria(self,data):
        results = []
        BestAndRestSets = self.BestsAndRests()
        map(lambda br: results.append(self.rankvals(br[0],br[1])), BestAndRestSets)
        return results

    #------------------------------------
    #- Functions to get instances
    #------------------------------------

    def GetInstancesPerClass(self,data):
        return map(lambda klass: filter(lambda instance: instance[-1] == klass, data), self.classes)

    def SelectInstances(self, crit, inst):
        return map(lambda criteria: filter(lambda instance: instance[criteria[-1]] == criteria[0],inst),crit)

    #------------------------------------
    #- Functions below to calculate ranks
    #------------------------------------
    def rankvals(self,best,rest):

        def getranks1(col):            
            vals = self.GetUniques(transpose(self.data)[col])
            return map(lambda val: self.binrank(best,rest,val,col),vals)

        ranks2 = []
        ranks2 = map(lambda i: getranks1(i),range(len(transpose(self.data)) - 1))

        ranks3 = []
        for rank in ranks2:
            inputrank = sorted(rank, key=lambda instance: instance[1])
            inputrank.reverse()
            ranks3.append(inputrank)

        tmpRank = sorted(map(lambda rank: rank[0], ranks3),key=lambda instance: instance[1])
        tmpRank.reverse()
        return tmpRank

            

    def binrank(self,best,rest, val, col):
        DatasetLength = len(self.data)
        pbest = float(len(best)) / float(DatasetLength)
        prest = float(len(rest)) / float(DatasetLength)

        def freqE(data, val, col):
            One = filter(lambda instance: instance[col] == val, data)
            if len(One) == 0:
                return 0.0
            else:
                return float(len(One)) / float(len(best))

        likeBest = float(freqE(best,val,col)) * pbest
        likeRest = float(freqE(rest,val,col)) * prest
        rank = math.pow(likeBest,2.0) / (likeBest+likeRest)

        return [val, rank, col]

    #----------------------------------------------------------
    #- Functions below to grab best and rest sets for each klass
    #----------------------------------------------------------
        
    # Mappin' the crap out of these lists.
    def BestsAndRests(self):
        self.SortedData = sorted(deepcopy(self.data),key=lambda instance: instance[-1])
        return map(self.MakeBestRestSet,self.classes)

    def MakeBestRestSet(self,klass):
        best = []
        rest = []

        map(lambda instance: best.append(instance) if instance[-1] == klass else rest.append(instance),self.data)

        return [best,rest]
    
    # Used for adding unique classes to a list.
    def PopulateClasses(self,data):
        ReturnClasses = []
        for instance in self.data:
            if instance[-1] not in ReturnClasses:
                ReturnClasses.append(instance[-1])
        return ReturnClasses

    def GetUniques(self, ListOfItems):
        uniques = []
        for item in ListOfItems:
            if item not in uniques:
                uniques.append(item)
        return uniques

    def ExtractUniques(self,ListOfListOfItems):
        uniques = []
        for listOfItems in ListOfListOfItems:
            for Items in listOfItems:
                for item in Items:
                    if item not in uniques:
                        uniques.append(item)
        return uniques
