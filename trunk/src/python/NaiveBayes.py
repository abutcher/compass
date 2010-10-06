class Normal:
    largest = []
    smallest = []
    n = []
    Sum = []
    SumSquared = []
    
    def __init__(self,data):
        for i in range(len(data)-1):
            largest.append(0)
            smallest.append(99999999)
            n.append(len(data))
            Sum.append(0)
            SumSquared.append(0)

        for instance in data:
            for i in range(len(instance)-1):
                if instance[i] > largest[i]:
                    largest[i] = instance[i]
                if instance[i] < smallest[i]:
                    smallest[i] = instance[i]
                Sum[i] = Sum[i] + instance[i]
                SumSquared = SumSquared[i] + pow(instance[i],2)

    def mean(self,Index):
        return self.Sum[Index] / self.n[Index]

    def stdev(self,Index):
        return math.sqrt( (self.SumSquared - ( math.pow(self.Sum[Index],2) / self.n[Index] ))
                   (self.n[Index] - 1))

    def GaussianPDF(self,Index,x):
        return ( 1 / math.sqrt(2 * math.pi * math.pow(self.stdev(self,Index),2) )) * math.pow(math.e,-1 * ( math.pow( x - self.stdev(Index),2) / (2 * math.pow(self.stdev(Index),2))))
            
        

def Naive-Bayes-Classify(instance, data, m=2, k=1):
    data.remove(instance)
    Classification = ["blank", -100]
    (ClassList,ClassNormal) = GenerateNormalForClasses(data)
    for ClassIndex in range(ClassList):
        # Calculate the prior
        tmp = math.log( ClassFrequency[ClassIndex] + k ) / ( len(data) + ( k * len(ListOfClasses)))
        for FeatureIndex in range(len(instance)-1):
            # Gaussian PDF function.  May fix for Discrete values later.
            tmp = tmp + math.log(ClassNormal[ClassIndex].GaussianPDF(FeatureIndex,instance[FeatureIndex]))
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
            ClassInstances.append = [instance]
    for ClassNorm in ClassInstances:
        ClassNormal.append(Normal(data))
    return ClassIndex, ClassNormal    


