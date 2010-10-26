#!/usr/bin/env python

from arff import *
import csv
import numpy
import matplotlib.lines as lines
import matplotlib.pyplot as plt
import matplotlib.collections as collections
from matplotlib import cm, colors
# Need this instead of depreciated OptionParser for multiple file input
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
	Classes = []

	def __init__(self, InputData,Parameters):
		self.data = InputData
		(self.West, self.East) = self.FindPoles(InputData)
		(self.DataCoordinates, self.Classes) = self.ComputeCoordinates(Parameters)
		if Parameters.Magnetic is True:
			EastSide = 0
			WestSide = 0
			# check to see if more points are at the other pole.  If so, we switch the poles.  Might be a better way to do this.  Will think about it.
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
				
	def GenerateFigure(self, filename, Parameters):
		self.DataCoordinates = self.DataCoordinates.transpose()
		fig = plt.figure()
		ax1 = fig.add_axes([0.1,0.1,0.85,0.85])
		xticks = []
		yticks = []
                # Set the ticks between two choices.
                if Parameters.EqualWidth == True:
                        xticks = EqualWidthTicks(self.DataCoordinates.transpose(),0,Parameters.m)
                        yticks = EqualWidthTicks(self.DataCoordinates.transpose(),1,Parameters.n)
                elif Parameters.EqualFrequency == True:
                        xticks = EqualFrequencyTicks(self.DataCoordinates.transpose(),0,Parameters.m)
                        yticks = EqualFrequencyTicks(self.DataCoordinates.transpose(),1,Parameters.n)


		#ax1.set_xticks(xticks)
		#ax1.set_yticks(yticks)

		plt.title(filename)
		plt.xlabel("x")
		plt.ylabel("y")

		for ax in fig.axes:
			ax.grid(True)

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
			
#		if Parameters.Normalize == True:
#			ax1.set_xbound(0,1)
#			ax1.set_ybound(0,1)

		if Parameters.Cluster:
			self.DataCoordinates = self.DataCoordinates.transpose()
			Quadrants = self.MakeQuadrants(Parameters, xticks, yticks)

			# Display the cuts we made.  Since we don't care about unclustered regions, leaving this out.
			"""
			for i in range(len(Quadrants)):
				xmin = Quadrants[i].xmin
				xmax = Quadrants[i].xmax
				ymin = Quadrants[i].ymin
				ymax = Quadrants[i].ymax
				ax.broken_barh([ (xmin, (xmax - xmin)) ], (ymin, (ymax - ymin)) , facecolors='white', linewidth=0.2)
			"""

			# Color the clusters green.
			Clusters = GRIDCLUS(Quadrants)

			# Gather performance scores for each cluster
			Scores = []
			if Parameters.test == None:
                                for Cluster in Clusters:
                                        Scores.append((PerformanceScore(Cluster), Cluster))
                        else:
                                for Cluster in Clusters:
                                        #print "cluster go"
                                        Scores.append((PerformanceScore(Cluster,Parameters.test.data), Cluster))
                                
			print "finished performance score"
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
					if (i == 0):
						ax.bar(xmin, (ymax-ymin), width=(xmax-xmin), bottom=ymin, facecolor=color, visible=True, linewidth=0.2, label=label)
					else:
						ax.bar(xmin, (ymax-ymin), width=(xmax-xmin), bottom=ymin, facecolor=color, visible=True, linewidth=0.2)


			colors = make_N_colors(cm.summer, len(Scores)*20)
			colors = colors[::-1]

			for i in range(len(Scores)):
				if TYPE == "DEFECT":
					label = "HarMean: %.2f" % Scores[i][0]
				else:
					label = "MDMRE: %.2f" % Scores[i][0]
				ColorQuadrants(Scores[i][0], Scores[i][1], colors[i*20])

			# Turning off the legend for now.

			ax.legend()
			leg = plt.gca().get_legend()
			ltext  = leg.get_texts()
			plt.setp(ltext, fontsize='xx-small')
			leg.get_frame().set_alpha(0.5)

				
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
			if DataCoordinates is None:
				DataCoordinates = numpy.array([x,y])
				Classes = numpy.array([instance[len(instance)-1]])
			else:
				DataCoordinates = numpy.vstack((DataCoordinates,numpy.array([x,y])))
				Classes = numpy.vstack((Classes,numpy.array([instance[len(instance)-1]])))
		# Normalize coordinates
		for datum in DataCoordinates:
                        if Parameters.Normalize is True:
				datum[0] = datum[0] / MaxX
				datum[1] = datum[1] / MaxY
			if Parameters.logX is True:
				datum[0] = math.log(datum[0]+0.0001)
			if Parameters.logY is True:
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

		# xticks
		# [0.12950011803651956, 1.0]
		# yticks
		# [0.055597763327383122, 0.0]

		# Assume we're doing a 4 way split, then we only have one point of interest to split on.
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
		Quadrants.append(self.MakeQuadrant(minn, x, minn, y, self.DataCoordinates, self.data))
		# Bottom Right
		Quadrants.append(self.MakeQuadrant(x, maxn, minn, y, self.DataCoordinates, self.data))
		# Top Left
		Quadrants.append(self.MakeQuadrant(minn, x, y, maxn, self.DataCoordinates, self.data))
		# Top Right
		Quadrants.append(self.MakeQuadrant(x, maxn, y, maxn, self.DataCoordinates, self.data))

		# Treat quadrants as a tree, where root is the initial block.
		root = self.MakeQuadrant(minn,maxn,minn,maxn, self.DataCoordinates, self.data)
		root.children = Quadrants

		# Nested functions, how pleasant...
		
		if (Parameters.lives):
			def walk(quadrant, lives = 3):
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

	def MakeQuadrant(self, xmin, xmax, ymin, ymax, DataCoordinates, data):
		QuadrantData = []
		for i in range(len(DataCoordinates)):
			xcoord = DataCoordinates[i][0]
			ycoord = DataCoordinates[i][1]
			if ( xcoord > xmin and xcoord < xmax ) and ( ycoord > ymin and ycoord < ymax ):
				QuadrantData.append(Instance(DataCoordinates[i], data[i]))
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
		Quadrants.append(self.MakeQuadrant(xminBorder, x, yminBorder, y, self.DataCoordinates, self.data))
		# Bottom Right
		Quadrants.append(self.MakeQuadrant(x, xmaxBorder, yminBorder, y, self.DataCoordinates, self.data))
		# Top Left
		Quadrants.append(self.MakeQuadrant(xminBorder, x, y, ymaxBorder, self.DataCoordinates, self.data))
		# Top Right
		Quadrants.append(self.MakeQuadrant(x, xmaxBorder, y, ymaxBorder, self.DataCoordinates, self.data))

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
	
	
	def __init__(self, inputM, inputN, inputlogX, inputlogY, inputMagnetic,inputNormalize,inputEqualWidth,inputEqualFrequency, cluster, q, test,stratisfied, lives):
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
		self.stratisfied = stratisfied
		self.lives = lives
		
def main():
	ErrorState = "n"

	# Parambeters under argparse
	parser = argparse.ArgumentParser(description='Perform IDEA on given train and test sets.')
	parser.add_argument('--train', dest='train', metavar='FILE', type=str, nargs='+', help='Specify arff file[s] from which to construct the training set.')
	parser.add_argument('--test', dest='test', metavar='FILE', type=str, nargs='+', help='Specify arff files[s] from which to construct a test set. Not specifying this results in a self-test that\'s only useful for quick tests of the software.')
	parser.add_argument('--stratisfied', dest='stratisfied',default=None, metavar='RATIO', type=int, nargs=2, help='Specify a ratio for a stratisfied cross-validation scheme.  This takes two arguments for a x train to x test ratio. Do not include a test set with this flag.)')
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
	parser.add_argument('--lives', dest='lives', default=False, action='store_true', help='Turn on 3 lives splitting rule.')

	
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

        if options.stratisfied is not None:
                (arff,test) = StratisfiedCrossVal(arff,options.stratisfied)

        print "finished stratisfied"
	# Created a data structure CompassGraphParameters we can use to easily carry parameters between functions.  Better ideas are welcome.
	# Might be better to overload the constructor to accept a sequence.
	# Also, I'm not a fan of how many arguments this constructor is getting.  Must be a better way.
	parameters = CompassGraphParameters(options.m,options.n,options.logX, options.logY, options.magnetic, options.normalize,options.equalwidth,options.equalfrequency, options.cluster, options.q, test, options.stratisfied, options.lives)

	filename = options.train[0].split('.')[0]
	ideaplot = Idea(arff.data,parameters)
#	ideaplot.Quadrants(parameters)
	ideaplot.WriteToPNG(ideaplot.GenerateFigure(filename, parameters), filename)

if __name__ == '__main__':
	main()
