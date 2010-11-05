from util import *
import numpy

class Quadrant:
	xmin = None
	xmax = None
	ymin = None
	ymax = None
	data = []
	Stats = None
	children = []

	def __init__(self, XMin, XMax, YMin, YMax, Data):
		self.xmin = XMin
		self.xmax = XMax
		self.ymin = YMin
		self.ymax = YMax
		self.Data = Data
		self.Stats = DefectStats()

	def NumericMedian(self):
		classes = []
		for instance in self.Data:
			for datum in instance.datum:
				classes.append(datum[-1])
		return numpy.median(classes)

	def Center(self):
                return [ ((self.xmax - self.xmin) / 2) + self.xmin, ((self.ymax - self.ymin) / 2) + self.ymin, "None" ]

	def NumericVariance(self):
		classes = []
		for instance in self.Data:
			for datum in instance.datum:
				classes.append(datum[-1])
		return numpy.std(classes)

        def Density(self):
                N = len(self.Data)
                Volume = (self.xmax - self.xmin) * (self.ymax - self.ymin)
                return N/Volume
	
	def DataCoordinates(self):
		coords = []
		for instance in self.Data:
			coords.append(instance.coords)
		return coords

	def DataCoordsAndClasses(self):
                coords = []
                for instance in self.Data:
                        value = instance.coords.tolist()
                        value.extend(instance.Class)
                        coords.append(value)
                return coords

	def Datums(self):
		datums = []
		for instance in self.Data:
			datums.append(instance.datum)
		return datums
