#!/usr/bin/python

from arff import *
import csv
import numpy
import matplotlib.pyplot as plt
from optparse import OptionParser
from util import *

class CompassGraph:
	East = None
	West = None
	data = []
	DataCoordinates = []
	Classes = []

	def __init__(self, InputData):
		self.data = InputData
		(self.East, self.West) = self.FindPoles(InputData)
		(self.DataCoordinates, self.Classes) = self.ComputeCoordinates()

	def WriteToPNG(self,filename):
		self.DataCoordinates = self.DataCoordinates.transpose()
		plt.axis([0,1,0,1])
		plt.title(filename)
		plt.xlabel("x")
		plt.ylabel("y")
		plt.grid(True)
		#check to see if our class is defect prediction or effort estimation.  Act accordingly.
		if isinstance(self.Classes[0][0],str):
			for i in range(len(self.DataCoordinates[0])):
				if self.Classes[i][0].upper() == "TRUE":
					plt.plot(self.DataCoordinates[0][i],self.DataCoordinates[1][i],"bo")
				else:
					plt.plot(self.DataCoordinates[0][i],self.DataCoordinates[1][i],"ro")
		else:
			# normalize the effort scores so we can change the color of the dots depending if it's a high or low effort score
			MaxScore = 0
			MinScore = 9999999
			for i in range(len(self.data)):
				if self.data[i][len(self.data)-1] > MaxScore:
					MaxScore = self.data[i][len(self.data)]
				if self.data[i][len(self.data)-1] < MinScore:
					MinScore = self.data[i][len(self.data)]
			
			
		plt.show()
				

	def ComputeCoordinates(self):
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
		return East, West
	
	
def main():
	parser = OptionParser()
	parser.add_option("--arff", dest="arff", default=None, metavar="FILE", help="Make arff file to graph.")
	parser.add_option("--gnuplot", dest="gnuplot", default=False, 	metavar="NONE", action="store_true", help="Use gnuplot to write graph to file")
	(options, args) = parser.parse_args()
	
	if options.arff == None:
		print "Didn't supply an arff.  *grumble grumble*"
		sys.exit(0)

	arff = Arff(options.arff)
	compassgraph = CompassGraph(arff.data)
	compassgraph.WriteToPNG(options.arff.split('.')[0])

if __name__ == '__main__':
	main()
