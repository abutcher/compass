from util import *
from NaiveBayes import *

class Cluster:

    def __init__(self, quadrants):
        self.quadrants = quadrants

    def __init__(self):
        self.quadrants = []
        self.mark = False
    
    def add_quadrant(self, quadrant):
        self.quadrants.append(quadrant)

    def mark(self):
        self.mark = True
        
    def datums(self):
        instances = []
        for quadrant in self.quadrants:
            instances.extend(quadrant.instances)
        return [ inst.datum for inst in instances ]

    def is_neighbor(self, other_cluster):
        result = False
        for i in range(len(self.quadrants)):
            for j in range(len(other_cluster.quadrants)):
                a = self.quadrants[i]
                b = other_quadrant
                if ((a.xmin == b.xmax) or (a.xmax == b.xmin)) or ((a.ymin == b.ymax) or (a.ymax == b.ymin)):
                    result = True
        return result
        
def cluster_prune(clusters, pct):
    if len(clusters) == 1:
        return clusters
    cvars = []
    for cluster in clusters:
        cvars.append((entropy(cluster.datums()), cluster))
    cvars = sorted(cvars, key=lambda cluster: cluster[0])
    return [ cvar[1] for cvar in cvars[0:(len(cvars))-int(round((len(cvars))*pct))] ]

def classify(datum, datums, input_type=None):
    if input_type is None:
        if type(datums[0][-1]) is str:
            input_type="DEFECT"
        else:
            input_type="EFFORT"
    if input_type == "DEFECT":
        return NaiveBayesClassify(datum,datums)
    elif InputType == "EFFORT":
        neighbors = kNearestNeighbors(datum,datums)
        return median(neighbors)
