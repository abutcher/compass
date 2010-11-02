import arff
from discretize import *
from shuffle import *

class Bore:
    data=[]
    best=[]
    rest=[]
    bfreq=[]
    rfreq=[]

    def __init__(self, data, goal, b=0.2):
        for i in range(20):
            data = shuffle(data)
        self.data = discretize(data)
        self.goal = goal.lower()
        self.brsplit(b)
        self.bfreq = self.freqtable(self.best)
        self.rfreq = self.freqtable(self.rest)
        self.score()

    def brsplit(self, x):
        for i in range(len(self.data)):
            if (self.data[i][-1].lower() == self.goal) and (len(self.best) < round((x*len(self.data)))):
                self.best.append(self.data[i])
            else:
                self.rest.append(self.data[i])

    def freqtable(self, data):
        freqtable=[]
        trans = transpose(data)
        for column in trans:
            d = {}
            for item in column:
                if item not in d.keys():
                    d[item] = 1
                else:
                    d[item] += 1
            freqtable.append(d)
        return freqtable

    def score(self):
        bscores=[]
        for d in self.bfreq:
            dscores=[]
            for k in d.keys():
                dscores.append((k, self.like(k, d)**2/(self.like(k,d)+self.like(k, self.rfreq[self.bfreq.index(d)])))) # b^2/b+r
            bscores.append(sorted(dscores, key=lambda dscore: dscore[1], reverse=True))
        for i in range(len(bscores)):
            print "Best in column %d is %s with score %.2f" % (i, str(bscores[i][0][0]), bscores[i][0][1])
                
    def like(self, item, d):
        if item not in d.keys():
            return 0
        p = d[item]
        pn = 0.0
        for k in d.keys():
            pn += d[k]
        return p/pn

arff = arff.Arff("arff/defect/ar3.arff")
Bore(arff.data, 'true', 0.2)
