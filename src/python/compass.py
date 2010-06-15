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
        self.variance = variance(self.data)

def compass(data, minsize=3):
    root = Node(data)
    
    def walk(node, level=0):
        if len(node.data) >= minsize:
            left, right = separate(node.data)
            if len(left) >= minsize:
                node.left = Node(left)
            if len(right) >= minsize:
                node.right = Node(right)
            if node.left != None:
                walk(node.left, level + 1)
            if node.right != None:
                walk(node.right, level + 1)

    walk(root)
    return root

def treeprint(node, level=0):
    print "Node: level=%d"% (level)
    print "Variance: %6.4f"% (node.variance)
    print "Contents: " + str(node.data)
    if node.left != None:
        treeprint(node.left, level + 1)
    if node.right != None:
        treeprint(node.right, level + 1)

def variance(data):
    if len(data) == 1:
        return 0
    else:
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
    return l[random.randint(0,len(l) - 1)]

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
    for i in range(len(vecone) - 1):
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

treeprint(compass(extract("arff/telecom1.arff")))
