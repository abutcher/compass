from NaiveBayes import *
from util import distance
import sys

# This contains functions for defect prediction statistics.  If you're looking
# for related functions such as MRE() that aren't in this file, check util.py.

def PerformIDEACluster(clusters,test,dataset="Unknown", treatment="IDEACLUSTER"):
    Stats = DefectStats()
    if type(test[0].Coord()[-1]) is str:
        for datum in test:
            Closest = [sys.maxint, None]
            for cluster in clusters:
                for quadrant in cluster.quadrants:
                    if distance(datum.Coord(), quadrant.center()) < Closest[0]:
                        Closest[0] = distance(datum.Coord(), quadrant.center())
                        Closest[1] = cluster
            train = []
            for quadrant in Closest[1].quadrants:
                train.extend(quadrant.ClassCoords())
            Stats.Evaluate(NaiveBayesClassify(datum.Coord(),train,"DEFECT"), datum.klass())
        Stats.StatsLine(dataset, treatment)
                

def PerformBaseline(data,test,dataset="Unknown",treatment="None"):
    Stats = DefectStats()
    if type(test[0].Coord()[-1]) is str:
        train = []
        for instance in data:
            train.append(instance.Coord())
        for datum in test:
            Stats.Evaluate(NaiveBayesClassify(datum.Coord(), train, "DEFECT"), datum.klass())
        Stats.StatsLine(dataset,treatment)

def PrintHeaderLine():
    print "dataset"," ","treatment"," ","CLASS ","A"," ","B"," ","C"," ","D"," ","pd"," ","pf"," ","precision"," ","accuracy"," ","HarmonicMean"

            

class DefectStats:
    # [a,b,c,d]
    TRUE = [0,0,0,0]
    FALSE = [0,0,0,0]

    def __init__(self):
        self.TRUE = [0,0,0,0]
        self.FALSE = [0,0,0,0]

    def Evaluate(self,Got, Want):
        if Got.lower() == Want.lower():
            if Got.lower() == "true" or Got.lower() == "yes":
                #print "true match"
                self.incf("TRUE","d")
                self.incf("FALSE","a")
            elif Got.lower() == "false" or Got.lower() == "no":
                #print "false match"
                self.incf("FALSE","d")
                self.incf("TRUE","a")
        elif Got.lower() != Want.lower():
            if Got.lower() == "true" or Got.lower() == "yes":
                #print "got true mismatch"
                self.incf("TRUE","c")
                self.incf("FALSE","b")
            elif Got.lower() == "false" or Got.lower() == "no":
                #print "got false mismatch"
                self.incf("FALSE","c")
                self.incf("TRUE","b")

    def incf(self,CLASS, pos):
        if CLASS is "TRUE":
            if pos is "a":
                self.TRUE[0] = self.TRUE[0] + 1
                #print(self.TRUE)
            elif pos is "b":
                self.TRUE[1] = self.TRUE[1] + 1
                #print(self.TRUE)
            elif pos is "c":
                self.TRUE[2] = self.TRUE[2] + 1
                #print (self.TRUE)
            elif pos is "d":
                self.TRUE[3] = self.TRUE[3] + 1
                #print (self.TRUE)
        elif CLASS is "FALSE":
            if pos is "a":
                self.FALSE[0] = self.FALSE[0] + 1
            elif pos is "b":
                self.FALSE[1] = self.FALSE[1] + 1
            elif pos is "c":
                self.FALSE[2] = self.FALSE[2] + 1
            elif pos is "d":
                self.FALSE[3] = self.FALSE[3] + 1

    def precision(self,CLASS):
        try:
            return float(self.__D__(CLASS)) / float((self.__C__(CLASS) + self.__D__(CLASS)))
        except:
            return 0.0

    def accuracy(self,CLASS):
        try:
            return float((self.__A__(CLASS) + self.__D__(CLASS))) / float((self.__A__(CLASS) + self.__B__(CLASS) + self.__C__(CLASS) + self.__D__(CLASS)))
        except:
            return 0.0

    def pd(self,CLASS):
        try:
            return float(self.__D__(CLASS)) / float((self.__B__(CLASS) + self.__D__(CLASS)))
        except:
            return 0.0

    def pf(self,CLASS):
        try:
            return float(self.__C__(CLASS)) / float((self.__A__(CLASS) + self.__C__(CLASS)))
        except:
            return 0.0

    def HarmonicMean(self,CLASS):
        try:
            return float((2 * (1 - self.pf(CLASS)) * self.pd(CLASS))) / float(((1 - self.pf(CLASS)) + self.pd(CLASS)))
        except:
            return 0.0

    def Count(self, CLASS):
        return self.__A__(CLASS) + self.__B__(CLASS) + self.__C__(CLASS) + self.__D__(CLASS)

    def StatsLine(self, dataset,treatment):
        print dataset," ",treatment," ","TRUE ",self.__A__("TRUE")," ",self.__B__("TRUE")," ",self.__C__("TRUE")," ", self.__D__("TRUE")," ", self.pd("TRUE")," ",self.pf("TRUE")," ",self.precision("TRUE")," ",self.accuracy("TRUE")," ",self.HarmonicMean("TRUE")
        print dataset," ",treatment," ","FALSE ",self.__A__("FALSE")," ",self.__B__("FALSE")," ",self.__C__("FALSE")," ", self.__D__("FALSE")," ", self.pd("FALSE")," ",self.pf("FALSE")," ",self.precision("FALSE")," ",self.accuracy("FALSE")," ",self.HarmonicMean("FALSE")


    # Private classes for grabbing A,B,C, and D
    def __A__(self,CLASS):
        if CLASS is "TRUE":
            return self.TRUE[0]
        elif CLASS is "FALSE":
            return self.FALSE[0]

    def __B__(self,CLASS):
        if CLASS is "TRUE":
            return self.TRUE[1]
        elif CLASS is "FALSE":
            return self.FALSE[1]

    def __C__(self,CLASS):
        if CLASS is "TRUE":
            return self.TRUE[2]
        elif CLASS is "FALSE":
            return self.FALSE[2]

    def __D__(self,CLASS):
        if CLASS is "TRUE":
            return self.TRUE[3]
        elif CLASS is "FALSE":
            return self.FALSE[3]

