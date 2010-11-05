from util import *
import numpy

class Quadrant:
	xmin = None
	xmax = None
	ymin = None
	ymax = None
	instances = []
	children = []

	def __init__(self, xmin, xmax, ymin, ymax, instances):
		self.xmin = xmin
		self.xmax = xmax
		self.ymin = ymin
		self.ymax = ymax
		self.instances = instances

	def center(self):
                return [ ((self.xmax - self.xmin) / 2) + self.xmin,
			 ((self.ymax - self.ymin) / 2) + self.ymin,
			 "None" ]

        def density(self):
                n = len(self.instances)
                volume = (self.xmax - self.xmin) * (self.ymax - self.ymin)
                return n/volume
	
	def coords(self):
		return map(self.coord, instances)

	def datums(self):
		return map(self.datum, instances)
	
	def split(self):
		x_split = equal_frequency_ticks_x(self.coords(), 1)[0]
		y_split = equal_frequency_ticks_y(self.coords(), 1)[0]
		return [ Quadrant(self.xmin, x_split, self.ymin, y_split, self.instances_in_bounds(self.xmin, x_split, self.ymin, y_split)),
			 Quadrant(x_split, self.xmax, self.ymin, y_split, self.instances_in_bounds(x_split, self.xmax, self.ymin, y_split)),
			 Quadrant(self.xmin, x_split, y_split, self.ymax, self.instances_in_bounds(self.xmin, x_split, y_split, self.ymax)),
			 Quadrant(x_split, self.xmax, y_split, self.ymax, self.instances_in_bounds(x_split, self.xmax, y_split, self.ymax)) ]
		
	def instances_in_bounds(self, xmin, xmax, ymin, ymax):
		instances = []
		for instance in self.instance_collection.instances:
			x = instance.coord.x
			y = instance.coord.y
			if ( x > xmin and x < xmax) and ( y > ymin and y < ymax ):
				instances.append(instance)
		return instances
