#!/usr/bin/env python

from arff import *
import csv
import numpy
import sys
import matplotlib.font_manager as fm
import matplotlib.lines as lines
import matplotlib.pyplot as plt
import matplotlib.collections as collections
import matplotlib.gridspec as gridspec
from matplotlib import cm, colors
import argparse
from util import *
from quadrant import *
from instance import *
from gridclus import *
from cluster import *

class Idea:
	East = None
	West = None
	data = []
	DataCoordinates = []
	#hack I'm not proud of. --Kel
	TestCoordinates = []
	Classes = []

	def __init__(self, InputData,Parameters):
		self.data = InputData
		(self.West, self.East) = self.FindPoles(InputData)
		(self.DataCoordinates, self.Classes) = self.ComputeCoordinates(Parameters)
		if Parameters.Magnetic == True:
			EastSide = 0
			WestSide = 0
			# Check to see if more points are at the other
			# pole.  If so, we switch the poles.  Might be
			# a better way to do this.  Will think about
			# it.
			for datum in self.DataCoordinates :
				if datum[0] >= 0.5:
					EastSide = EastSide + 1
				else:
					WestSide = WestSide + 1
			if (EastSide > WestSide):
				print("switching poles")
				tmp = self.East
				self.East = self.West
				self.West = tmp
				(self.DataCoordinates, self.Classes) = self.ComputeCoordinates(Parameters)
		if Parameters.stratified is not None:
                        (self.TestCoordinates, Parameters.test) = StratifiedCrossVal(self.TestCoordinates,Parameters.stratified)
				
	def GenerateFigure(self, filename, Parameters):
		self.DataCoordinates = self.DataCoordinates.transpose()
		fig = plt.figure()
		
		ax1 = fig.add_axes([0.1,0.1,0.6,0.8])
		plt.title(filename)
		ax2 = fig.add_axes([0.75,0.7,0.2,0.2])
		plt.title('Legend')
		ax3 = fig.add_axes([0.75,0.3,0.2,0.3])
		plt.title('Distribution')

		ax1.set_xlabel('x')
		ax1.set_ylabel('y')

		plt.setp(ax2, xticks=[], yticks=[])
		plt.setp(ax3, xticks=[], yticks=[])

		for ax in fig.axes:
			ax.grid(True)

		xticks = []
		yticks = []

                # Set the ticks between two choices.
		# Needed for the initial splits in quadrant creation.
                if Parameters.EqualWidth == True:
                        xticks = EqualWidthTicks(self.DataCoordinates.transpose(),0,Parameters.m)
                        yticks = EqualWidthTicks(self.DataCoordinates.transpose(),1,Parameters.n)
                elif Parameters.EqualFrequency == True:
                        xticks = EqualFrequencyTicks(self.DataCoordinates.transpose(),0,Parameters.m)
                        yticks = EqualFrequencyTicks(self.DataCoordinates.transpose(),1,Parameters.n)

		if isinstance(self.Classes[0][0],str):
			# Discrete class values.
			for i in range(len(self.DataCoordinates[0])):
				if self.Classes[i][0].upper() == "TRUE":
					ax1.plot(self.DataCoordinates[0][i],self.DataCoordinates[1][i],"bo",markersize=2,alpha=0.5)
				else:
					ax1.plot(self.DataCoordinates[0][i],self.DataCoordinates[1][i],"ro",markersize=2,alpha=0.5)
		else:
			# Continuous class values.
			normalData = Normalize(self.data)

			# Draw t3h datars.
			for i in range(len(self.DataCoordinates[0])):
				ax1.plot(self.DataCoordinates[0][i],self.DataCoordinates[1][i],'o',color=[1-normalData[i],normalData[i],0],markersize=2,alpha=0.5)
			
		if Parameters.Cluster:
			self.DataCoordinates = self.DataCoordinates.transpose()
			Quadrants = self.MakeQuadrants(Parameters, xticks, yticks)

			Clusters = GRIDCLUS(Quadrants, Parameters.acceptance)
			
			# Gather performance scores for each cluster
			Scores = []
			if Parameters.test == None:
                                for Cluster in Clusters:
                                        Scores.append((PerformanceScore(Cluster), Cluster))
                        else:
                                StatList = []
                                for Cluster in Clusters:
                                        StatList.append(DefectStats())
                                for datum in Parameters.test:

                                        ClosestCluster = [sys.maxint, None]
                                        for i in range(len(Clusters)):
                                                for quad in Clusters[i]:
                                                        tmpDistance = distance(datum, quad.Center())
                                                        if tmpDistance < ClosestCluster[0]:
                                                               ClosestCluster[0] = tmpDistance
                                                               ClosestCluster[1] = i
							       
                                        train = []
                                        for quad in Clusters[ClosestCluster[1]]:
                                                train.extend(quad.DataCoordsAndClasses())
                                        Got = Classify(datum, train, "DEFECT")
                                        #print Got
                                        Want = datum[-1]
                                        if Got.lower() == Want.lower():
                                                if Got.lower() == "true" or Got.lower() == "yes":
                                                        #print "true match"
                                                        StatList[ClosestCluster[1]].incf("TRUE","d")
                                                        StatList[ClosestCluster[1]].incf("FALSE","a")
                                                elif Got == "false" or Got == "no":
                                                        #print "false match"
                                                        StatList[ClosestCluster[1]].incf("FALSE","d")
                                                        StatList[ClosestCluster[1]].incf("TRUE","a")
                                        elif Got.lower() != Want.lower():
                                                if Got.lower() == "true" or Got.lower() == "yes":
                                                        #print "got true mismatch"
                                                        StatList[ClosestCluster[1]].incf("TRUE","c")
                                                        StatList[ClosestCluster[1]].incf("FALSE","b")
                                                elif Got == "false" or Got == "no":
                                                        #print "got false mismatch"
                                                        StatList[ClosestCluster[1]].incf("FALSE","c")
                                                        StatList[ClosestCluster[1]].incf("TRUE","b")

				print filename
				print "acceptance=%.2f" % (Parameters.acceptance)
				print "Harmonic Mean, Count" 
                                for i in range(len(Clusters)):
					print "%.2f, %d" % (StatList[i].HarmonicMean("TRUE"),StatList[i].Count("TRUE"))
                                        Scores.append((StatList[i].HarmonicMean("TRUE"), Clusters[i]))
				print ""
					
				"""
				for i in range(len(Clusters)):
					size = 0
					for quadrant in Clusters[i]:
						size += len(quadrant.Datums())
					print "Cluster %d, Size %d" % (i, size)
				"""

			if type(self.data[0][-1]) is str:
				Scores = sorted(Scores, key=lambda score: score[0], reverse=True)
				TYPE = "DEFECT"
			else:
				Scores = sorted(Scores, key=lambda score: score[0])
				TYPE = "EFFORT"

			def make_N_colors(cmap_name, N):
				cmap = cm.get_cmap(cmap_name, N)
				return cmap(np.arange(N))
 
			def ColorQuadrants(score, quadrants, color):
				for i in range(len(quadrants)):
					xmin = quadrants[i].xmin
					xmax = quadrants[i].xmax
					ymin = quadrants[i].ymin
					ymax = quadrants[i].ymax
					ax1.bar(xmin, (ymax-ymin), width=(xmax-xmin), bottom=ymin, facecolor=color, visible=True, linewidth=0.2)

			for i in range(len(Scores)):
				if TYPE == "DEFECT":
					if (Scores[i][0] > .75):
						ColorQuadrants(Scores[i][0], Scores[i][1], '#44d241')
					elif (Scores[i][0] > 0.25):
						ColorQuadrants(Scores[i][0], Scores[i][1], '#e8f554')
					elif (Scores[i][0] < 0.25):
						ColorQuadrants(Scores[i][0], Scores[i][1], '#e24d4d')
				else:
					if (Scores[i][0] < 0.5):
						ColorQuadrants(Scores[i][0], Scores[i][1], '#44d241')
					else:
						ColorQuadrants(Scores[i][0], Scores[i][1], '#e24d4d')

			# Legend stuff
			try:				
				if (TYPE == "DEFECT"):
					ax1.bar(0,0, width=0, bottom=0, facecolor='#44d241', label="HM > 0.75")
					ax1.bar(0,0, width=0, bottom=0, facecolor='#e8f554', label="HM > 0.25")
					ax1.bar(0,0, width=0, bottom=0, facecolor='#e24d4d', label="HM < 0.25")
				else:
					ax1.bar(0,0, width=0, bottom=0, facecolor='#44d241', label="MDMRE < 0.5")
					ax1.bar(0,0, width=0, bottom=0, facecolor='#e24d4d', label="MDMRE < 0.5")

                                handles, labels = ax1.get_legend_handles_labels()
                                l = ax2.legend(handles, labels, loc='center', prop=dict(family='sans-serif',size=8))
				l.get_frame().set_linewidth(0)
                        except:
                                print "legend divided by zero 0.  No legend."

			# Pie Chart Stuff
			good = 0
			okay = 0
			bad = 0
			for i in range(len(Scores)):
				if TYPE == "DEFECT":
					if Scores[i][0] > 0.75:
						for quadrant in Scores[i][1]:
							good += len(quadrant.Datums())
					elif Scores[i][0] > 0.25:
						for quadrant in Scores[i][1]:
							okay += len(quadrant.Datums())
					elif Scores[i][0] < 0.25:
						for quadrant in Scores[i][1]:
							bad += len(quadrant.Datums())
				if TYPE == "EFFORT":
					if Scores[i][0] < 0.5:
						for quadrant in Scores[i][1]:
							good += len(quadrant.Datums())
					elif Scores[i][0] > 0.5:
						for quadrant in Scores[i][1]:
							bad += len(quadrant.Datums())
			
			clustered = 0
			for i in range(len(Scores)):
				for quadrant in Scores[i][1]:
					clustered += len(quadrant.Datums())
					
			
			unclustered = len(self.data) - clustered - len(Quadrants)
			if unclustered < 0: # Sometimes quadrant creation doesn't exclude a node per split
				unclustered = len(self.data) - clustered


			patches, texts, autotexts = ax3.pie([good,okay,bad,unclustered], colors=['#44d241','#e8f554','#e24d4d','#c6cdc6'], autopct='%1.1f%%')

			proptease = fm.FontProperties()
			proptease.set_size('xx-small')
			plt.setp(autotexts, fontproperties=proptease)
			plt.setp(texts, fontproperties=proptease)

			# Output clustered data in csv format.
			if (Parameters.clus2csv):
				writeClusters(Clusters, filename)
			
		return fig

	def WriteToPNG(self, fig, filename):
		plt.savefig("%s.png" % filename)

	def ComputeCoordinates(self,Parameters):
		d = distance(self.East,self.West)
		MaxX = 0
		MaxY = 0
		DataCoordinates = None
		# Compute (x,y) coordinates for each instance
		for instance in self.data:
			a = distance(self.East,instance)
			b = distance(self.West,instance)
			x = (b**2 - d**2 - a**2) / (-2 * d)
			if x > MaxX:
				MaxX = x
			y = math.sqrt(a**2 - x**2)
			if y > MaxY:
				MaxY = y
			self.TestCoordinates.append([x,y,instance[-1]])
			if DataCoordinates is None:
				DataCoordinates = numpy.array([x,y])
				Classes = numpy.array([instance[len(instance)-1]])
			else:
				DataCoordinates = numpy.vstack((DataCoordinates,numpy.array([x,y])))
				Classes = numpy.vstack((Classes,numpy.array([instance[len(instance)-1]])))
		# Normalize coordinates
		for datum in DataCoordinates:
                        if Parameters.Normalize == True:
				datum[0] = datum[0] / MaxX
				datum[1] = datum[1] / MaxY
			if Parameters.logX == True:
				datum[0] = math.log(datum[0]+0.0001)
			if Parameters.logY == True:
				datum[1] = math.log(datum[1]+0.0001)
		for datum in self.TestCoordinates:
                        if Parameters.Normalize == True:
				datum[0] = datum[0] / MaxX
				datum[1] = datum[1] / MaxY
			if Parameters.logX == True:
				datum[0] = math.log(datum[0]+0.0001)
			if Parameters.logY == True:
				datum[1] = math.log(datum[1]+0.0001)
		return DataCoordinates, Classes

	def FindPoles(self,data):
		this = randomelement(data)
		data.remove(this)
		East = farthestfrom(this,data)
		data.remove(East)
		data.append(this)
		West = farthestfrom(East,data)
		data.append(East)
		return East, West

	def MakeQuadrants(self, Parameters, xticks, yticks):
		Quadrants = []
		
		# Assume we're doing a 4 way split, then we only have
		# one point of interest to split on.
		
		x = xticks[0]
		y = yticks[0]

		minx = 9999999
		miny = 9999999
		for point in self.DataCoordinates:
			if point[0] < minx:
				minx = point[0]
			if point[1] < miny:
				miny = point[1]

		maxx = -999999
		maxy = -999999
		for point in self.DataCoordinates:
			if point[0] > maxx:
				maxx = point[0]
			if point[1] > maxy:
				maxy = point[1]

		minn = minx
		maxn = maxy

		# Bottom Left
		Quadrants.append(self.MakeQuadrant(minn, x, minn, y, self.DataCoordinates, self.data, self.Classes))
		# Bottom Right
		Quadrants.append(self.MakeQuadrant(x, maxn, minn, y, self.DataCoordinates, self.data, self.Classes))
		# Top Left
		Quadrants.append(self.MakeQuadrant(minn, x, y, maxn, self.DataCoordinates, self.data, self.Classes))
		# Top Right
		Quadrants.append(self.MakeQuadrant(x, maxn, y, maxn, self.DataCoordinates, self.data, self.Classes))

		# Treat quadrants as a tree, where root is the initial block.
		root = self.MakeQuadrant(minn,maxn,minn,maxn, self.DataCoordinates, self.data, self.Classes)
		root.children = Quadrants

		# Trying this...
		Parameters.q = math.sqrt(len(self.data)) / 2

		# Nested functions, how pleasant...
		if (Parameters.lives):
			def walk(quadrant, lives = 6):
				if (lives > 0):
					if quadrant.children != []:
						for child in quadrant.children:
							if len(child.Data) >= int(Parameters.q):
								child.children = self.SplitQuadrant(child, Parameters)
								for grandchild in child.children:
									if len(grandchild.Datums()) > 0:
										if (entropy(grandchild.Datums()) > entropy(child.Datums())):
											walk(grandchild, lives - 1)
										else:
											walk(grandchild)
					else:
						if len(quadrant.Data) >= int(Parameters.q):
							quadrant.children = self.SplitQuadrant(quadrant, Parameters)
							for child in quadrant.children:
								if len(child.Datums()) > 0:
									if (entropy(child.Datums()) > entropy(quadrant.Datums())):
										walk(child, lives - 1)
									else:
										walk(child)
		else:
			def walk(quadrant):
				if quadrant.children != []:
					for child in quadrant.children:
						if len(child.Data) >= int(Parameters.q):
							child.children = self.SplitQuadrant(child, Parameters)
							for grandchild in child.children:
								walk(grandchild)
				else:
					if len(quadrant.Data) >= int(Parameters.q):
						quadrant.children = self.SplitQuadrant(quadrant, Parameters)
						for child in quadrant.children:
							walk(child)

		walk(root)
		leaflist = []

		def collectleaves(quadrant):
			if quadrant.children == []:
				leaflist.append(quadrant)
			else:
				for child in quadrant.children:
					collectleaves(child)
				
		collectleaves(root)
		Quadrants = leaflist

		return Quadrants

	def MakeQuadrant(self, xmin, xmax, ymin, ymax, DataCoordinates, data, Classes):
		QuadrantData = []
		for i in range(len(DataCoordinates)):
			xcoord = DataCoordinates[i][0]
			ycoord = DataCoordinates[i][1]
			if ( xcoord > xmin and xcoord < xmax ) and ( ycoord > ymin and ycoord < ymax ):
				QuadrantData.append(Instance(DataCoordinates[i], data[i], Classes[i]))
		return Quadrant(xmin, xmax, ymin, ymax, QuadrantData)

	def SplitQuadrant(self, Quadrant, Parameters):
		Quadrants = []
		Data = Quadrant.Data
		Coords = Quadrant.DataCoordinates()

		xminBorder = Quadrant.xmin
		xmaxBorder = Quadrant.xmax
		yminBorder = Quadrant.ymin
		ymaxBorder = Quadrant.ymax
		
		xticks = EqualFrequencyTicks(Coords,0,Parameters.m)
		yticks = EqualFrequencyTicks(Coords,1,Parameters.n)

		x = xticks[1]
		y = yticks[1]
		
		# Bottom Left
		Quadrants.append(self.MakeQuadrant(xminBorder, x, yminBorder, y, self.DataCoordinates, self.data, self.Classes))
		# Bottom Right
		Quadrants.append(self.MakeQuadrant(x, xmaxBorder, yminBorder, y, self.DataCoordinates, self.data, self.Classes))
		# Top Left
		Quadrants.append(self.MakeQuadrant(xminBorder, x, y, ymaxBorder, self.DataCoordinates, self.data, self.Classes))
		# Top Right
		Quadrants.append(self.MakeQuadrant(x, xmaxBorder, y, ymaxBorder, self.DataCoordinates, self.data, self.Classes))

		return Quadrants

	def UpdateTicks(self, xticks, yticks):
		self.xticks += xticks
		self.yticks += yticks

class CompassGraphParameters:
        m = 4
	n = 4
	logX = False
	logY = False
	Magnetic = False
	Normalize = False
	Cluster = False
	
	def __init__(self, inputM, inputN, inputlogX, inputlogY, inputMagnetic,inputNormalize,inputEqualWidth,inputEqualFrequency, cluster, q, test,stratified,outputdir,lives, clus2csv, acceptance):
                self.m = int(inputM)
		self.n = int(inputN)
		self.logX = inputlogX
		self.logY = inputlogY
		self.Magnetic = inputMagnetic
		self.Normalize = inputNormalize
		self.EqualWidth = inputEqualWidth
		self.EqualFrequency = inputEqualFrequency
		self.Cluster = cluster
		self.q = q
		self.test = test
		self.stratified = stratified
		self.outputdir = outputdir
		self.lives = lives
		self.clus2csv = clus2csv
		self.acceptance = acceptance
	
def main():
	ErrorState = "n"

	# Parambeters under argparse
	parser = argparse.ArgumentParser(description='Perform IDEA on given train and test sets.')
	parser.add_argument('--train', dest='train', metavar='FILE', type=str, nargs='+', help='Specify arff file[s] from which to construct the training set.')
	parser.add_argument('--test', dest='test', metavar='FILE', type=str, nargs='+', help='Specify arff files[s] from which to construct a test set. Not specifying this results in a self-test that\'s only useful for quick tests of the software.')
	parser.add_argument('--stratified', dest='stratified',default=None, metavar='RATIO', type=int, nargs=2, help='Specify a ratio for a stratified cross-validation scheme.  This takes two arguments for a x train to x test ratio. Do not include a test set with this flag.)')
	parser.add_argument('--m', dest='m', default='1', metavar='M', type=int, help='Set m initial horizontal splits for clustering.')
	parser.add_argument('--n', dest='n', default='1', metavar='N', type=int, help='Set n initial vertical splits for clustering.')
	parser.add_argument('--logX', dest='logX', default=False, action='store_true', help='Apply a log transform to the X axis.')
	parser.add_argument('--logY', dest='logY', default=False, action='store_true', help='Apply a log transform to the Y axis.')
	parser.add_argument('--magnetic', dest='magnetic', default=False, action='store_true', help='Force the West pole to be more densely populated.')
	parser.add_argument('--nonormalize', dest='normalize', default=True, action='store_false', help='Prevents normalization of data in the range (0,1)')
	parser.add_argument('--equalwidth', dest='equalwidth', default=False, action='store_true', help='Instructs equally sized splits based on graph distance.')
	parser.add_argument('--equalfrequency', dest='equalfrequency', default=False, action='store_true', help='Instructs equally sized splits based on distribution of points on the graph.')
	parser.add_argument('--cluster', dest='cluster', default=False, action='store_true', help='Enables quadrant clustering.')
	parser.add_argument('--q', dest='q', default=6, type=int, metavar="Q", help='Minimum Quadrant size')
	parser.add_argument('--output', dest='output', type=str, metavar='CONCAT', default='', help='Specify an output dir.')
	parser.add_argument('--lives', dest='lives', default=False, action='store_true', help='Turn on 3 lives stopping policy.')
 	parser.add_argument('--clus2csv', dest='clus2csv', default=False, action='store_true', help='Save the clustered data to csv.')
	parser.add_argument('--acceptance', dest='acceptance', default=0.1, type=float, metavar="%", help='GRIDCLUS acceptance percentage.')


	options = parser.parse_args()
	
	if options.train == None:
		print "Didn't supply an arff.  *grumble grumble*"
		ErrorState = "y"

        if options.equalwidth == True and options.equalfrequency == True:
                print "Cannot use equal width and equal frequency flags together."
                ErrorState = "y"
	
	if ErrorState == "y":
		print "Missing critical arguments.  Aborting."
		sys.exit(-1)
		
	arff = Arff(options.train)

	# If the user uses --test, we set the test set to be used later.
	if options.test is not None:
                test = Arff(filename)
        else:
                test = None



	# Created a data structure CompassGraphParameters we can use to easily carry parameters between functions.  Better ideas are welcome.
	# Might be better to overload the constructor to accept a sequence.
	# Also, I'm not a fan of how many arguments this constructor is getting.  Must be a better way.
	parameters = CompassGraphParameters(options.m,options.n,options.logX, options.logY, options.magnetic, options.normalize,options.equalwidth,options.equalfrequency, options.cluster, options.q, test, options.stratified, options.output, options.lives, options.clus2csv, options.acceptance)

	filename = options.train[0].split('.')[0]
	ideaplot = Idea(arff.data,parameters)
	ideaplot.WriteToPNG(ideaplot.GenerateFigure(filename, parameters), filename+options.output)

if __name__ == '__main__':
	main()
