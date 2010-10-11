#!/usr/bin/env python

from arff import *
import csv
import numpy
import matplotlib.lines as lines
import matplotlib.pyplot as plt
import matplotlib.collections as collections
from optparse import OptionParser
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


		ax1.set_xticks(xticks)
		ax1.set_yticks(yticks)
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

			"""
			for i in range(len(Quadrants)):
			if (Quadrants[i].Data != []) and (len(Quadrants[i].Data) > 4):
					xmin = Quadrants[i].xmin
					xmax = Quadrants[i].xmax
					ymin = Quadrants[i].ymin
					ymax = Quadrants[i].ymax
					ax.broken_barh([ (xmin, (xmax - xmin)) ], (ymin, (ymax - ymin)) , facecolors='gold', alpha='0.5')

				else:
					xmin = Quadrants[i].xmin
					xmax = Quadrants[i].xmax
					ymin = Quadrants[i].ymin
					ymax = Quadrants[i].ymax
					ax.broken_barh([ (xmin, (xmax - xmin)) ], (ymin, (ymax - ymin)) , facecolors='gray', alpha='0.2')
			"""

			# Display the cuts we made
			for i in range(len(Quadrants)):
				xmin = Quadrants[i].xmin
				xmax = Quadrants[i].xmax
				ymin = Quadrants[i].ymin
				ymax = Quadrants[i].ymax
				ax.broken_barh([ (xmin, (xmax - xmin)) ], (ymin, (ymax - ymin)) , facecolors='white')
			
			# Color the clusters green.
			Clusters = GRIDCLUS(Quadrants)
			"""
			for Cluster in Clusters:
				for Quadrant in Cluster:
					xmin = Quadrant.xmin
					xmax = Quadrant.xmax
					ymin = Quadrant.ymin
					ymax = Quadrant.ymax
					ax.broken_barh([ (xmin, (xmax - xmin)) ], (ymin, (ymax - ymin)) , facecolors='green', alpha='0.5')
			"""
			# Gather performance scores for each cluster
			Scores = []
			for Cluster in Clusters:
				Scores.append((PerformanceScore(Cluster), Cluster))
			
			TYPE=""
			if type(self.data[0][-1]) is str:
				Scores = sorted(Scores, key=lambda score: score[0], reverse=True)
				TYPE = "DEFECT"
			else:
				Scores = sorted(Scores, key=lambda score: score[0])
				TYPE = "EFFORT"

			for i in range(len(Scores)):
				if TYPE == "DEFECT":
					print "Harmonic Mean: %.2f" % Scores[i][0]
				else:
					print "MDMRE: %.2f" % Scores[i][0]

				# Top 25%
				if (i < len(Scores)/4):
					for Quadrant in Scores[i][1]:
						xmin = Quadrant.xmin
						xmax = Quadrant.xmax
						ymin = Quadrant.ymin
						ymax = Quadrant.ymax
						ax.broken_barh([ (xmin, (xmax - xmin)) ], (ymin, (ymax - ymin)) , facecolors='green', alpha='0.5')
				# Middle 50%
				elif (i < len(Scores)/2) and (i > len(Scores)/4):
					for Quadrant in Scores[i][1]:
						xmin = Quadrant.xmin
						xmax = Quadrant.xmax
						ymin = Quadrant.ymin
						ymax = Quadrant.ymax
						ax.broken_barh([ (xmin, (xmax - xmin)) ], (ymin, (ymax - ymin)) , facecolors='yellow', alpha='0.5')
				# Bottom 25%
				elif (i < len(Scores)) and (i > len(Scores)/2):
					for Quadrant in Scores[i][1]:
						xmin = Quadrant.xmin
						xmax = Quadrant.xmax
						ymin = Quadrant.ymin
						ymax = Quadrant.ymax
						ax.broken_barh([ (xmin, (xmax - xmin)) ], (ymin, (ymax - ymin)) , facecolors='red', alpha='0.5')
				
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
		minn = yticks[1]
		maxn = xticks[1]

		# Bottom Left
		Quadrants.append(self.MakeQuadrant(minn, x, minn, y, self.DataCoordinates, self.data))
		# Bottom Right
		Quadrants.append(self.MakeQuadrant(x, maxn, minn, y, self.DataCoordinates, self.data))
		# Top Left
		Quadrants.append(self.MakeQuadrant(minn, x, y, maxn, self.DataCoordinates, self.data))
		# Top Right
		Quadrants.append(self.MakeQuadrant(x, maxn, y, maxn, self.DataCoordinates, self.data))

		# Treat quadrants as a tree, where root is the initial board.
		root = self.MakeQuadrant(minn,maxn,minn,maxn, self.DataCoordinates, self.data)
		root.children = Quadrants

		# Nested functions, how pleasant...
		
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

		"""
		# This is simply for --n integer and has not yet been implemented for equal freq etc.
		# Assuming axes are 1.0 -> 1.0 and split by Parameters.n
		for x in range(Parameters.n):
			for y in range(Parameters.n):
				xmin = float(x)/Parameters.n
				xmax = (float(x)/Parameters.n) + (1.0/Parameters.n)
				ymin = float(y)/Parameters.n
				ymax = (float(y)/Parameters.n) + (1.0/Parameters.n)
				print "Quadrant defined by\n\txmin: %f\n\txmax: %f\n\tymin: %f\n\tymax: %f\n" % (xmin, xmax, ymin, ymax)
				data = []
				for i in range(len(self.DataCoordinates)):
					xcoord = self.DataCoordinates[i][0]
					ycoord = self.DataCoordinates[i][1]
					if ( xcoord >= xmin and xcoord <= xmax ) and ( ycoord >= ymin and ycoord <= ymax ):
						data.append(Instance(self.DataCoordinates[i], self.data[i]))
					Quadrants.append(Quadrant(xmin, xmax, ymin, ymax, data))
				print "Has data:"
				for instance in data:
					print instance.datum
		"""
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
	
	def __init__(self, inputM, inputN, inputlogX, inputlogY, inputMagnetic,inputNormalize,inputEqualWidth,inputEqualFrequency, cluster, q):
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
	
def main():
	ErrorState = "n"
	parser = OptionParser()
	parser.add_option("--arff", dest="arff", default=None, metavar="FILE", help="Make arff file to graph.")
	parser.add_option("--m", dest="m", default=4, metavar="M", help="Set m to draw m x n grids.")
	parser.add_option("--n", dest="n", default=4, metavar="N", help="Set n to draw m x n grids.")
	parser.add_option("--logX", dest="logX", default=False, metavar="NONE", action="store_true", help="Apply a log to X axis values")
	parser.add_option("--logY", dest="logY",default=False, metavar="NONE", action="store_true", help="Apply a log to Y axis values")
	parser.add_option("--magnetic", dest="magnetic", default=False, metavar="NONE", action="store_true", help="Force the West pole to be more densely populated. Useful for --logX and --logY")
	parser.add_option("--nonormalize",dest="normalize",default=True, metavar="NONE", action="store_false", help="Prevents normalization of data between 0 and 1.")
	parser.add_option("--equalwidth",dest="equalwidth",default=False, metavar="NONE", action="store_true", help="Enables grid lines equally spaced between points.")
        parser.add_option("--equalfrequency", dest="equalfrequency", default=False, metavar="NONE", action="store_true", help="Enables grid lines of equal frequency between points.")
	parser.add_option("--cluster", dest="cluster", default=False, metavar="NONE", action="store_true", help="Enables quadrant clustering.")
	parser.add_option("--q", dest="q", default=6, metavar="Q", help="Min quadrant size.")
	
	(options, args) = parser.parse_args()
	
	if options.arff == None:
		print "Didn't supply an arff.  *grumble grumble*"
		ErrorState = "y"

        if options.equalwidth == True and options.equalfrequency == True:
                print "Cannot use equal width and equal frequency flags together."
                ErrorState = "y"
	
	if ErrorState == "y":
		print "Missing critical arguments.  Aborting."
		sys.exit(-1)
			
	arff = Arff(options.arff)

	# Created a data structure CompassGraphParameters we can use to easily carry parameters between functions.  Better ideas are welcome.
	# Might be better to overload the constructor to accept a sequence.
	# Also, I'm not a fan of how many arguments this constructor is getting.  Must be a better way.
	parameters = CompassGraphParameters(options.m,options.n,options.logX, options.logY, options.magnetic, options.normalize,options.equalwidth,options.equalfrequency, options.cluster, options.q)

	filename = options.arff.split('.')[0]
	ideaplot = Idea(arff.data,parameters)
#	ideaplot.Quadrants(parameters)
	ideaplot.WriteToPNG(ideaplot.GenerateFigure(filename, parameters), filename)

if __name__ == '__main__':
	main()
