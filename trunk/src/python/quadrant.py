class Quadrant:
	xmin = None
	xmax = None
	ymin = None
	ymax = None
	Data=[] # Collection of instances which have coordinates and data information attached.
	children = []

	def __init__(self, XMin, XMax, YMin, YMax, Data):
		self.xmin = XMin
		self.xmax = XMax
		self.ymin = YMin
		self.ymax = YMax
		self.Data = Data

	def NumericMedian(self):
		classes = []
		for instance in self.Data:
			for datum in instance.datum:
				classes.append(datum[-1])
		return numpy.median(classes)

	def NumericVariance(self):
		classes = []
		for instance in self.Data:
			for datum in instance.datum:
				classes.append(datum[-1])
		return numpy.std(classes)
	
	def DataCoordinates(self):
		coords = []
		for instance in self.Data:
			coords.append(instance.coords)
		return coords
