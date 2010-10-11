import os
import sys
import math
import random
import numpy as np

def variance(data):
    if len(data) == 1:
        return 0
    else:
        return stddev(transpose(data)[-1], None)

def median(data):
    Scores = []
    for i in range(len(data)):
        try:
            Scores.append(data[i][-1])
        except:
            Scores.append(data[i])
    Scores.sort()
    if len(Scores) is 1:
        return Scores[-1]
    elif len(Scores) is 2:
        return (Scores[0] + Scores[1]) / 2
    elif len(Scores) % 2 == 0:
        middle = int(math.floor(len(Scores) / 2))
        return (Scores[middle] + Scores[middle+1]) / 2
    else:
        return Scores[len(Scores) / 2]
    

def transpose(lists):
   if not lists: 
       return []
   return map(lambda *row: list(row), *lists)

def separate(these):
    thisgroup = []
    thatgroup = []
    this = randomelement(these)
    these.remove(this)
    that = farthestfrom(this, these)
    these.remove(that)
    these.append(this)
    this = farthestfrom(that, these)
    these.remove(this)
    thisgroup.append(this)
    thatgroup.append(that)
    for instance in these:
        if distance(instance, this) > distance(instance, that):
            thatgroup.append(instance)
        else:
            thisgroup.append(instance)
    these.append(this)
    these.append(that)
    return thisgroup, thatgroup

def randomelement(l):
    return l[random.randint(0,len(l) - 1)]

def closestto(this, these, d=sys.maxint):
    for instance in these:
        if distance(this, instance) < d:
            that = instance
            d = distance(this, instance)
    return that

def farthestfrom(this, these, d=0.0):
    for instance in these:
        if distance(this, instance) > d:
            that = instance
            d = distance(this, instance)
    return that

def distance(vecone, vectwo, d=0.0):
    for i in range(len(vecone) - 1):
        if isnumeric(vecone[i]):
            d = d + (vectwo[i] - vecone[i])**2
        elif vecone[i] == vectwo[i]:
            d += 1
    return math.sqrt(d)

def isnumeric(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

def stddev(values, meanval=None):
    if meanval == None: meanval = mean(values)
    return math.sqrt(sum([(x - meanval)**2 for x in values]) / (len(values)-1))

def mean(values):
    return sum(values) / float(len(values))

def MRE(actual, predicted):
    return abs(actual - predicted) / actual

def FindMinMax(data,indice):
    maxScore = 0
    minScore = 99999999999
    for i in range(len(data)):
        if data[i][indice] < minScore:
            minScore = data[i][indice]
        if data[i][indice] > maxScore:
            maxScore = data[i][indice]
    return minScore, maxScore

def FindMinMax(data):
    maxScore = 0
    minScore = 99999999999
    for i in range(len(data)):
        if data[i][len(data[i])-1] < minScore:
            minScore = data[i][len(data[i])-1]
        if data[i][len(data[i])-1] > maxScore:
            maxScore = data[i][len(data[i])-1]
    return minScore, maxScore

def Normalize(data):
    normalData=[]
    minScore, maxScore = FindMinMax(data)
    for i in range(len(data)):
        normalData.append((data[i][len(data[i])-1]-minScore)/(maxScore-minScore))
    return normalData

# indice will be 0 for x or 1 for y.
def EqualFrequencyTicks(data,indice,n):
    ticks = []
    SortedData = np.sort(data,axis=0)
    NumPerGrid = int(round(len(data) / (n+1.0)))
    for i in range(n):
        ticks.append(data[NumPerGrid*i][indice])
    ticks.append(data[len(data)-1][indice])
    return ticks
    
def EqualWidthTicks(data, indice, n):
    ticks = []
    (Min, Max) = FindMinMax(data,indice)
    Interval = (Max - Min)/ float(n)
    for i in range(n+1):
        ticks.append( ( float(i) ) * float(Interval))
    return ticks
