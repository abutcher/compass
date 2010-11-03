import math
from util import *

class gnode:
    right=None
    left=None
    data=None
    variance=None

    def __init__(self, right, left, data):
        self.right = right
        self.left = left
        self.data = data
        self.variance = variance(self.data)

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

    def weightedvariance(self):
        if self.right == None and self.left == None:
            return self.variance
        else:
            if self.right == None or self.left == None:
                if self.right == None:
                    return self.left.variance
                else:
                    return self.right.variance
            else:
                return (((self.right.variance*len(self.right.data))+(self.left.variance*len(self.left.data)))/(len(self.right.data)+len(self.left.data)))

    def majorityclass(self):
        freqs = {}
        for klass in transpose(self.data)[-1]:
            if klass not in freqs.keys():
                freqs[klass] = 1
            else:
                freqs[klass] += 1
        best = 0
        majority = None
        for k in freqs.keys():
            if freqs[k] > best:
                best = freqs[k]
                majority = k
        return k

class gac:
    nodes=[]
    maxv = 0

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
            print "%sNode => Level=%d, Size=%d, Variance=%.2f" % ("\t"*level, level, len(node.data), node.variance)
            if node.right != None:
                walk(node.right, level+1)
            if node.left != None:
                walk(node.left, level+1)
        walk(root)

    def maxva(self):
        root = self.nodes[0]
        def walk(node):
            if node.variance > self.maxv:
                self.maxv = node.variance
            if node.right != None:
                walk(node.right)
            if node.left != None:
                walk(node.left)
        walk(root)
        
    def varianceprune(self, alpha, beta):
        self.maxva()
        def walk(node, level=0):
            if level > 3:
                if node.right != None:
                    if ((alpha * node.variance) < node.right.variance) or ((beta * self.maxv) < node.right.variance):
                        node.right = None
                if node.left != None:
                    if ((alpha * node.variance) < node.left.variance) or ((beta * self.maxv) < node.left.variance):
                        node.left = None
            if node.right != None:
                walk(node.right, level+1)
            if node.left != None:
                walk(node.left, level+1)
        walk(self.nodes[0])
