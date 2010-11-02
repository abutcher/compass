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
        trans = transpose(self.data)
        colscores = []
        for i in range(len(trans)):
            scores = []
            for j in range(len(trans[i])):
                scores.append(self.like(trans[i][j], self.bfreq[i])**2/(self.like(trans[i][j],self.bfreq[i])+self.like(trans[i][j], self.rfreq[i])))
            colscores.append((i, median(scores)))
        print "Top X columns:"
        for sortedcol in sorted(colscores, key=lambda score: score[1], reverse=True):
            if sortedcol[1] != 0:
                print "\tColumn: %d, Score: %.2f" % (sortedcol[0], sortedcol[1])
            
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
