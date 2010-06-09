import csv
import random
from scipy import stats
import sys
import warnings
warnings.simplefilter('ignore', DeprecationWarning)

class Node:
    right = None
    left = None
    data = None
    variance = None
    
    def __init__(self, data):
        self.data = data
        self.variance = variance(data)

def compass(root):
    if len(root.data) >= 4:
        left, right = separate(root.data)
        root.left = Node(left)
        compass(root.left)
        root.right = Node(right)
        compass(root.right)
    else:
        return root

def variance(data):
    return stats.std(transpose(data)[-1], None)

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
    return thisgroup, thatgroup

def randomelement(l):
    return l[random.randint(0,len(l))]

def closestto(this, these, d=0.0):
    for instance in these:
        if distance(this, instance) > d:
            that = instance
            d = distance(this, instance)
    return that

def farthestfrom(this, these, d=sys.maxint):
    for instance in these:
        if distance(this, instance) < d:
            that = instance
            d = distance(this, instance)
    return that

def distance(vecone, vectwo, d=0.0):
    for i in range(len(vecone)):
        if isnumeric(vecone[i]):
            d = d + (vectwo[i] - vecone[i])**2
        elif vecone[i] == vectwo[i]:
            d += 1
    return d

def extract(path):
    data = []
    reader = csv.reader(open(path, "r"))
    for row in reader:
        if len(row) > 1 and '@' not in row[0]:
            for i in range(len(row)):
                if isnumeric(row[i]):
                    row[i] = float(row[i])
            data.append(row)
    return data

def isnumeric(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

#root = Node(extract("arff/telecom1.arff"))
#print compass(root)
