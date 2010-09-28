#!/usr/bin/python

from arff import *
import csv
import numpy
from optparse import OptionParser
from util import *

class CompassGraph:
	East = None
	West = None
	data = []
	DataCoordinates = numpy.array([])

	def __init__(self, InputData):
		self.data = InputData
		(self.East, self.West) = FindPoles(InputData)
		self.DataCoordinates = ComputeCoordinates(data, East, West)

	def ComputeCoordinates(data, East, West):
		d = distance(East,West)
		MaxX = 0
		MaxY = 0
		# Compute (x,y) coordinates for each instance
		for instance in data:
			a = distance(East,instance)
			b = distance(West,instance)
			x = (b**2 - d**2 - a**2) / (-2 * d)
			if x > MaxX:
				MaxX = x
			y = math.sqrt(a**2 - x**2)
			if y > MaxY:
				MaxY = y
			self.DataCoordinates = numpy.vstack((self.DataCoordinates,numpy.array([x,y])))
		# Normalize coordinates
		for datum in self.DataCoordinates:
			datum[0] = datum[0] / MaxX
			datum[1] = datum[1] / MaxY
		return self.DataCoordinates

	def FindPoles(data):
		this = randomelement(data)
		data.remove(this)
		East = farthestfrom(this,data)
		data.remove(East)
		data.append(this)
		West = farthestfrom(East,data)
		return East, West
	
	
def main():
	parser = OptionParser()
	parser.add_option("--arff", dest="arff", default=None, metavar="FILE", 					help="An arff file to graph.")
	parser.add_option("--gnuplot", dest="gnuplot", default=False, 					metavar="NONE", action="store_true", 
				help="Use gnuplot to write graph to file")
	(options, args) = parser.parse_args()
	
	if options.arff == None:
		print "Didn't supply an arff.  *grumble grumble*"
		sys.exit(0)

	arff = Arff(options.arff)
	compassgraph = CompassGraph(arff.data)


if __name__ == "__main__":
	main()
