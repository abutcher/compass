from math import *

class Normal:
    largest = []
    smallest = []
    n = []
    Sum = []
    SumSquared = []
    
    def __init__(self,data):
        for i in range(len(data[0])):
            self.largest.append(0)
            self.smallest.append(99999999)
            self.n.append(len(data))
            self.Sum.append(0)
            self.SumSquared.append(0)

        for instance in data:
            for i in range(len(instance)-1):
                if instance[i] > self.largest[i]:
                    self.largest[i] = instance[i]
                if instance[i] < self.smallest[i]:
                    self.smallest[i] = instance[i]
                self.Sum[i] = self.Sum[i] + instance[i]
                self.SumSquared[i] = self.SumSquared[i] + pow(instance[i],2)

    def mean(self,Index):
        return self.Sum[Index] / self.n[Index]

    def stdev(self,Index):
        try:
            return sqrt( (self.SumSquared[Index] - ( pow(self.Sum[Index],2) / self.n[Index] )) / (self.n[Index] - 1))
        except ValueError:
            return 0.00000001

    def GaussianPDF(self,Index,x):
        try:
            return ( 1 / sqrt(2 * pi * pow(self.stdev(Index),2) )) * pow(e,-1 * ( pow( x - self.stdev(Index),2) / (2 * pow(self.stdev(Index),2))))
        except:
            return 0.0
            
        

def NaiveBayesClassify(instance, data, m=2, k=1):
    data.remove(instance)
    Classification = ["blank", -100]
    (ClassList,ClassNormal) = GenerateNormalForClasses(data)
    for ClassIndex in range(len(ClassList)):
        # Calculate the prior
        tmp = log( ClassNormal[ClassIndex].n[0] + k ) / ( len(data) + ( k * len(ClassList)))
        for FeatureIndex in range(len(instance)-1):
            # Gaussian PDF function.  May fix for Discrete values later.
            tmp = tmp + ClassNormal[ClassIndex].GaussianPDF(FeatureIndex,instance[FeatureIndex])
        if Classification[1] < tmp:
            Classification[0] = ClassList[ClassIndex]
            Classification[1] = tmp
    return Classification[0]
            

def GenerateNormalForClasses(data):
    ClassInstances = []
    ClassIndex = []
    ClassNormal = []
    for instance in data:
        # Try to add an instance to the Class list
        try:
            ClassInstances[ClassIndex.index(instance[len(instance)-1])].append(instance)
        except ValueError:
            ClassIndex.append(instance[len(instance)-1])
            ClassInstances.append([instance])
    for ClassNorm in ClassInstances:
        ClassNormal.append(Normal(data))
    return ClassIndex, ClassNormal    


