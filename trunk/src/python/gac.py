import arff
import math
from util import *

class gnode:
    right=None
    left=None
    data=None

    def __init__(self, right, left, data):
        self.right = right
        self.left = left
        self.data = data

    def median(self):
        if len(self.data) == 1:
            return self.data[0]
        else:
            best = 99999999
            center = None
            for instance in self.data:
                self.data.remove(instance)
                sumd = 0
                for other in self.data:
                    sumd += distance(instance, other)
                if best > sumd:
                    best = sumd
                    center = instance
                self.data.append(instance)
        return center

class gac:
    nodes=[]

    def __init__(self, data):
        for instance in data:
            self.nodes.append(gnode(None, None, [instance]))

        def combineall(nodes):
            if len(nodes) == 1:
                self.nodes = nodes
            else:
                newnodes=[]
                for i in range(int(math.ceil(float(len(nodes))/float(2)))):
                    node = randomelement(nodes)
                    nodes.remove(node)
                    if len(nodes) == 0:
                        newnodes.append(node)
                    else:
                        closest = self.closestnode(node, nodes)
                        nodes.remove(closest)
                        newnodes.append(self.combine(node, closest))
                combineall(newnodes)

        combineall(self.nodes)
        self.describe()

    def combine(self, one, two):
        combined = one.data + two.data
        return gnode(one,two,combined)

    def closestnode(self, this, those):
        best = 999999999
        closest = None
        for other in those:
            if distance(this.median(), other.median()) < best:
                best = distance(this.median(), other.median())
                closest = other
        return closest

    def describe(self):
        root = self.nodes[0]
        def walk(node, level=0):
            print "%sNode => Level=%d, Size=%d" % ("\t"*level, level, len(node.data))
            if node.right != None:
                walk(node.right, level+1)
            if node.left != None:
                walk(node.left, level+1)
        walk(root)

arff = arff.Arff("arff/defect/ar3.arff")
gac(arff.data)
