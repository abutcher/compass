from util import *

class Cluster:
    # scoring something something

    def __init__(self, quadrants):
        self.quadrants = quadrants

    def __init__(self):
        self.quadrants = []
    
    def add_quadrant(self, quadrant):
        self.quadrants.append(quadrant)
        
    def datums(self):
        instances = []
        for quadrant in self.quadrants:
            instances.extend(quadrant.instances)
        return [ inst.datum for inst in instances ]
        
def cluster_prune(clusters, pct):
    if len(clusters) == 1:
        return clusters
    cvars = []
    for cluster in clusters:
        cvars.append((entropy(cluster.datums()), cluster))
    cvars = sorted(cvars, key=lambda cluster: cluster[0])
    return [ cvar[1] for cvar in cvars[0:(len(cvars))-int(round((len(cvars))*pct))] ]
