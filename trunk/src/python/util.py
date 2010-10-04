import os
import sys
import math
import random

def variance(data):
    if len(data) == 1:
        return 0
    else:
        return stddev(transpose(data)[-1], None)

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

