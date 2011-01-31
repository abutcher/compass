import arff
import math
from util import *
from copy import deepcopy

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
    columns = len(data[0]) - 1
    for i in range(columns):
        WorkingData = sorted(WorkingData, key=lambda datum: datum[i])
        for j in range(len(WorkingData)):
            WorkingData[j][i] = j / binSize
    return WorkingData

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
