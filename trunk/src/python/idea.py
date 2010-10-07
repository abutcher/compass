#!/opt/local/bin/python2.6

from arff import *
import csv
import numpy
import matplotlib.lines as lines
import matplotlib.pyplot as plt
import matplotlib.collections as collections
from optparse import OptionParser
from util import *

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

		print "XTICKS"
		print xticks
		print "YTICKS"
		print yticks

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
					ax1.plot(self.DataCoordinates[0][i],self.DataCoordinates[1][i],"bo",markersize=3)
				else:
					ax1.plot(self.DataCoordinates[0][i],self.DataCoordinates[1][i],"ro",markersize=3)
		else:
			# Continuous class values.
			normalData = Normalize(self.data)

			# Draw t3h datars.
			for i in range(len(self.DataCoordinates[0])):
				ax1.plot(self.DataCoordinates[0][i],self.DataCoordinates[1][i],'o',color=[1-normalData[i],normalData[i],0],markersize=3)
			
		if Parameters.Normalize == True:
			ax1.set_xbound(0,1)
			ax1.set_ybound(0,1)

		if Parameters.Cluster:
			self.DataCoordinates = self.DataCoordinates.transpose()
			Quadrants = self.Quadrants(Parameters)

			for i in range(len(Quadrants)):
				if (Quadrants[i].Data != []):
					#print Quadrants[i].Data
					xmin = Quadrants[i].xmin
					xmax = Quadrants[i].xmax
					ymin = Quadrants[i].ymin
					ymax = Quadrants[i].ymax
					ax.broken_barh([ (xmin, 1.0/Parameters.n) ], (ymin, 1.0/Parameters.n), facecolors='blue') 
		return fig

	def WriteToPNG(self, fig, filename):
		plt.show()
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
			if Parameters.logX is True:
				datum[0] = math.log(datum[0]+0.0001)
			if Parameters.logY is True:
				datum[1] = math.log(datum[1]+0.0001)
			if Parameters.Normalize is True:
				datum[0] = datum[0] / MaxX
				datum[1] = datum[1] / MaxY
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

	def Quadrants(self, Parameters):
		Quadrants = []
		# Logic here for building based on the type of gridding.

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
		return Quadrants

class Quadrant:
	xmin = None
	xmax = None
	ymin = None
	ymax = None
	Data=[] # Collection of instances which have coordinates and data information attached.

	def __init__(self, XMin, XMax, YMin, YMax, Data):
		self.xmin = XMin
		self.xmax = XMax
		self.ymin = YMin
		self.ymax = YMax
		self.Data = Data

class Instance:
	coords = None
	datum = None

	def __init__(self, coords, datum):
		self.coords = coords
		self.datum = datum

class CompassGraphParameters:
        m = 4
	n = 4
	logX = False
	logY = False
	Magnetic = False
	Normalize = False
	Cluster = False
	
	def __init__(self, inputM, inputN, inputlogX, inputlogY, inputMagnetic,inputNormalize,inputEqualWidth,inputEqualFrequency, cluster):
                self.m = int(inputM)
		self.n = int(inputN)
		self.logX = inputlogX
		self.logY = inputlogY
		self.Magnetic = inputMagnetic
		self.Normalize = inputNormalize
		self.EqualWidth = inputEqualWidth
		self.EqualFrequency = inputEqualFrequency
		self.Cluster = cluster
	
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
	parameters = CompassGraphParameters(options.m,options.n,options.logX, options.logY, options.magnetic, options.normalize,options.equalwidth,options.equalfrequency, options.cluster)

	filename = options.arff.split('.')[0]
	ideaplot = Idea(arff.data,parameters)
#	ideaplot.Quadrants(parameters)
	ideaplot.WriteToPNG(ideaplot.GenerateFigure(filename, parameters), filename)

if __name__ == '__main__':
	main()
