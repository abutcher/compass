import math
from util import *
from copy import deepcopy

class CliffDiscretize:
    
    def __init__(self,data,bins=5):
        trans = transpose(deepcopy(data))
        self.ranges = []
        self.trans_width = len(trans)
        self.trans_height = len(trans)
        self.bin_size = len(data) / bins
        self.indices = []
        for i in range(bins):
            self.indices.append(i*self.bin_size)
        self.indices.append(len(trans[0]))
        for row in trans:
            srow = sorted(row)
            values = []
            for bin in range(bins):
                values.append([srow[self.indices[bin]],srow[self.indices[bin+1]-1]])
            self.ranges.append(values)

    def DiscretizeSet(self,train,klass=False):
        WorkingSet = deepcopy(train)
        DiscretizedSet = []
        if klass == False:
            Bound = len(WorkingSet[0]) - 1
        else:
            Bound = len(WorkingSet[0])
        for instance in WorkingSet:
            Discretized = deepcopy(instance)
            for i in range(Bound):
                ColumnRanges = self.ranges[i]
                for j in range(len(ColumnRanges)):
                    if ColumnRanges[j][0] <= instance[i] <= ColumnRanges[j][1]:
                        Discretized[i] = j
                        break
            DiscretizedSet.append(Discretized)
        return DiscretizedSet

    def GetQualifyingInstances(self,prototypes,data):
        ReturnData = []
        for prototype in prototypes:
            for datum in data:
                for i in range(len(datum)-1):
                    Qualified = True
                    ColumnRanges = self.ranges[i]
                    if ColumnRanges[prototype[i]][0] <= datum[i] <= ColumnRanges[prototype[i]][1]:
                        continue
                    else:
                        Qualified = False
                        break
                if Qualified == True:
                    ReturnData.append(datum)
                    break
        return ReturnData
              
            
            
        
        

def discretize(data, n=10):
    trans = transpose(deepcopy(data))
    for column in trans:
        if isnumeric(column[0]):
            scol = sorted(column)
            for i in range(len(column)):
                column[i] = (scol.index(column[i]) / (len(scol) / n))
    return transpose(trans)

def equal_freq_discretize(data,n=10):
    binSize = len(data) / ( n - 1 )
    WorkingData = deepcopy(data)
    columns = len(data[0])
    for i in range(columns):
        WorkingData = sorted(WorkingData, key=lambda datum: datum[i])
        for j in range(len(WorkingData)):
            WorkingData[j][i] = j / binSize
    return WorkingData

def equal_freq_discretize_bins(data,n=10):
    binSize = len(data) / (n - 1)
    WorkingData = deepcopy(data)
    columns = len(data[0])
    lumps = []
    for i in range(columns):
        lumps.append([])
        for j in range(n):
            lumps[i].append([])
        WorkingData = sorted(WorkingData, key=lambda datum: datum[i])
        for j in range(len(WorkingData) - 1):
            if WorkingData[j][i] not in lumps[i][j/binSize]:
                lumps[i][j/binSize].append(WorkingData[j][i])
    return lumps

def equal_width_discretize(data, n=10):
    WorkingData = deepcopy(data)
    columns = len(WorkingData[0]) - 1
    for i in range(columns):
        # Get max and min
        WorkingData = sorted(WorkingData, key=lambda datum: datum[i])
        Min = WorkingData[0][i]
        Max = WorkingData[-1][i]
#        print Max
        # compute ranges
        Ranges = (Max - Min) / n
        RangeList = [Min]
        for wut in range(n):
            RangeList.append(RangeList[wut] + Ranges)
#        print RangeList
        # do it.
        for j in range(len(WorkingData)):
            for k in range(len(RangeList) - 1):
                if math.floor(RangeList[k]) <= WorkingData[j][i] <= math.ceil(RangeList[k+1]):
                    WorkingData[j][i] = k
                    break
    return WorkingData
