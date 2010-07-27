#!/opt/local/bin/python

import arff
import math
import random
import scipy

DISTANCE_ITERATIONS=1

class FastMap: 

    def __init__(self, dist,verbose=False): 
        if dist.max()>1:
            dist/=dist.max()

        self.dist=dist
        self.verbose=verbose

    def _furthest(self, o): 
        mx=-1000000
        idx=-1
        for i in range(len(self.dist)): 
            d=self._dist(i,o, self.col)
            if d>mx: 
                mx=d
                idx=i

        return idx

    def _pickPivot(self):
        """Find the two most distant points"""
        o1=random.randint(0, len(self.dist)-1)
        o2=-1

        i=DISTANCE_ITERATIONS

        while i>0: 
            o=self._furthest(o1)
            if o==o2: break
            o2=o
            o=self._furthest(o2)
            if o==o1: break
            o1=o
            i-=1

        self.pivots[self.col]=(o1,o2)
        return (o1,o2)


    def _map(self, K): 
        if K==0: return 
    
        px,py=self._pickPivot()

        if self.verbose: print "Picked %d,%d at K=%d"%(px,py,K)

        if self._dist(px,py,self.col)==0: 
            return 
        for i in range(len(self.dist)):
            self.res[i][self.col]=self._x(i, px,py)

        self.col+=1
        self._map(K-1)

    def _x(self,i,x,y):
        """Project the i'th point onto the line defined by x and y"""
        dix=self._dist(i,x,self.col)
        diy=self._dist(i,y,self.col)
        dxy=self._dist(x,y,self.col)
        return (dix + dxy - diy) / 2*math.sqrt(dxy)

    def _dist(self, x,y,k): 
        """Recursively compute the distance based on previous projections"""
        if k==0: return self.dist[x,y]**2
    
        rec=self._dist(x,y, k-1)
        resd=(self.res[x][k] - self.res[y][k])**2
        return rec-resd


    def map(self, K): 
        self.col=0
        self.res=scipy.zeros((len(self.dist),K))
        self.pivots=scipy.zeros((K,2),"i")
        self._map(K)
        return self.res

def fastmap(dist, K):
    """dist is a NxN distance matrix
    returns coordinates for each N in K dimensions
    """
    
    return FastMap(dist,True).map(K)

# Below here are methods for testing

def vlen(x,y):
    return math.sqrt(sum((x-y)**2))                  

def distmatrix(p, c=vlen): 
   dist=scipy.zeros((len(p),len(p)))
   for x in range(len(p)):
       for y in range(x,len(p)):
           if x==y: continue
           dist[x,y]=c(p[x], p[y])
           dist[y,x]=dist[x,y]

   return dist

def distortion(d1,d2): 
    return scipy.sum(((d1/d1.max())-(d2/d2.max()))**2)/d1.size

def distortiontest(): 
    points=[]

    n=10
    mean=10
    dim=5

    print "Generating %d %d-D points randomly distributed between [0-%d]"%(n,dim,mean)
    for i in range(n):
        points.append(scipy.array([random.uniform(0,mean) for x in range(dim)]))    
    print "Computing distance matrix"
    dist=distmatrix(points)
    
    print "Mapping"

    p1=fastmap(dist,1)
    print "K=1"    
    print "Distortion: ", distortion(distmatrix(p1),dist)

    p2=fastmap(dist,2)
    print "K=2"
    print "Distortion: ", distortion(distmatrix(p1),dist)

    p3=fastmap(dist,3)
    print "K=3"
    print "Distortion: ", distortion(distmatrix(p3),dist)

    import pylab
    pylab.scatter([x[0]/mean for x in points], [x[1]/mean for x in points],s=50)
    pylab.scatter([x[0] for x in p2], [x[1] for x in p2], c="r")
    pylab.show()


def stringtest():

    import Levenshtein

    strings=[ "King Crimson", "King Lear", "Denis Leary", "George Bush", "George W. Bush", "Barack Hussein Obama", "Saddam Hussein", "George Leary" ] 
    
    dist=distmatrix(strings, c=lambda x,y: 1-Levenshtein.ratio(x,y))
    
    p=fastmap(dist,2)
    import pylab    
    pylab.scatter([x[0] for x in p], [x[1] for x in p], c="r")
    for i,s in enumerate(strings):
        pylab.annotate(s,p[i])

    pylab.title("Levenshtein distance mapped to 2D coordinates")
    pylab.savefig("strings.png")

def vecTest():
    vectors=arff.Arff("arff/telecom1.arff").data
    
    print vectors

    dist=distmatrix(vectors, c=euclidean)
    p=fastmap(dist,2)

    import pylab
    pylab.scatter([x[0] for x in p], [x[1] for x in p], c="r")

    pylab.title("telecom1, euclidean distance")
    pylab.savefig("vectors.png")

def euclidean(vecone, vectwo, d=0.0):
    for i in range(len(vecone) - 1):
        if isnumeric(vecone[i]):
            d = d + (vectwo[i] - vecone[i])**2
        elif vecone[i] == vectwo[i]:
            d += 1
    return d
    
def isnumeric(s):
    try:
        float(s)
        return True
    except ValueError:
        return False
    
if __name__=='__main__':
    
#    stringtest()
    vecTest()
