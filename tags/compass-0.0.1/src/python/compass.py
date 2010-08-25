#!/opt/local/bin/python
from arff import *
import csv
import networkx as nx
import matplotlib.pylab as plt
from optparse import OptionParser
import pydot
import uuid
from util import *
import wx

class CompassTree:
    node_list=[]
    node_sizes=[]
    node_labels={}

    def __init__(self, data, minsize=4):
        self.minsize = minsize
        self.root = self.Node(data)
        self.Walk(self.root)
        self.ConstructNodeList(self.root)

    def Walk(self, node, level=0):
        left, right = separate(node.data)
        if len(left) > 1:
            node.left = self.Node(left)
        if len(right) > 1:
            node.right = self.Node(right)
        if node.left != None and node.left.size() >= self.minsize:
            self.Walk(node.left, level + 1)
        if node.right != None and node.right.size() >= self.minsize:
            self.Walk(node.right, level + 1)

    def Print(self, node=None, level=0):
        if node == None:
            node = self.root
        print "Node Level: %d" % (level)
        print "Variance: %6.4f" % (node.variance)
        print "Contents: " + str(node.data)
        if node.left != None:
            self.Print(node.left, level + 1)
        if node.right != None:
            self.Print(node.right, level + 1)

    def DrawNx(self, pngname, node):
        G=nx.Graph()
        self.ConstructNx(G, self.root)
        self.ConstructNodeSizes(G)
        self.ConstructNodeLabels(G)
        # Cool blue #AOCBE2
        nx.draw(G, node_color='#A0CBE2', node_size=self.node_sizes, width=3, labels=self.node_labels, font_size=10, alpha=0.4) #with_labels=False)
        plt.title(pngname)
        plt.savefig("%s.png" % pngname)

    def ConstructNx(self, G, node, parentid=None):
        G.add_node(node.nid)
        if parentid != None:
            G.add_edge(parentid, node.nid)
        if node.right != None:
            self.ConstructNx(G, node.right, node.nid)
        if node.left != None:
            self.ConstructNx(G, node.left, node.nid)

    def DrawPyDot(self, pngname, node):
        G=pydot.Dot(graph_type='graph')
        self.ConstructPyDot(G, self.root)
        G.write_png("%s.png" % pngname)

    def ConstructPyDot(self, G, node, parentid=None):
        newid="Size %d, Variance %6.2f" % (node.size(), node.variance)
        if parentid != None:
            G.add_edge(pydot.Edge(parentid, newid))
        self.node_sizes.append(node.size()*20)
        if node.right != None:
            self.ConstructPyDot(G, node.right, newid)
        if node.left != None:
            self.ConstructPyDot(G, node.left, newid)

    def ConstructNodeList(self, node):
        self.node_list.append(node)
        if node.right != None:
            self.ConstructNodeList(node.right)
        if node.left != None:
            self.ConstructNodeList(node.left)

    def ConstructNodeSizes(self, G):
        for nid in G.nodes():
            self.node_sizes.append(self.NidSize(nid, self.node_list)*25)

    def NidSize(self, nid, node_list):
        for n in node_list:
            if n.nid == nid:
                return n.size()

    def NidVariance(self, nid, node_list):
        for n in node_list:
            if n.nid == nid:
                return n.variance

    def ConstructNodeLabels(self, G):
        for nid in G.nodes():
            self.node_labels[nid]=r'$\sigma$ %6.1f' % self.NidVariance(nid, self.node_list)

    class Node:
        data = None
        left = None
        nid = None
        right = None
        variance = None

        def __init__(self, data):
            self.data = data
            self.nid = str(uuid.uuid4())
            self.variance = variance(self.data)

        def size(self):
            return len(self.data)

def main():
    parser = OptionParser()
    parser.add_option("--arff", dest="arff", default=None, metavar="FILE",
                      help="An arff file to draw for you.")

    (options, args) = parser.parse_args()

    arff=Arff(options.arff)
    compasstree=CompassTree(arff.data)
    compasstree.DrawNx(options.arff, compasstree)
#    compasstree.DrawPyDot("telecom1-dot", compasstree)

if __name__ == "__main__":
    main()
