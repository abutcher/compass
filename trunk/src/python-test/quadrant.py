from util import *
import numpy
import math

class QuadrantTree:
	root = None
	minsize = None

	def __init__(self, instances):
		minx, miny = sys.maxint, sys.maxint
		maxx, maxy = -sys.maxint, -sys.maxint
		for instance in instances:
			if instance.coord.x < minx:
				minx = instance.coord.x
			if instance.coord.x > maxx:
				maxx = instance.coord.x
			if instance.coord.y < miny:
				miny = instance.coord.y
			if instance.coord.y > maxy:
				maxy = instance.coord.y
		self.root = Quadrant(minx, maxx, miny, maxy, instances)
		self.minsize = math.sqrt(len(instances))

		def grow(quadrant):
			if len(quadrant.instances) > self.minsize:
				quadrant.children = quadrant.split()
				for child in quadrant.children:
					grow(child)
		
		grow(self.root)

	def leaves(self):
		leaves = []
		def collect_leaves(quadrant):
			if quadrant.children == []:
				leaves.append(quadrant)
			else:
				for child in quadrant.children:
					collect_leaves(child)
		collect_leaves(self.root)
		return leaves


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
		return [ inst.coord for inst in self.instances ]

	def datums(self):
		return [ inst.datum for inst in self.instances ]
	
	def split(self):
		x_split = equal_frequency_ticks_x(self.coords(), 1)[0]
		y_split = equal_frequency_ticks_y(self.coords(), 1)[0]
		return [ Quadrant(self.xmin, x_split, self.ymin, y_split, self.instances_in_bounds(self.xmin, x_split, self.ymin, y_split)),
			 Quadrant(x_split, self.xmax, self.ymin, y_split, self.instances_in_bounds(x_split, self.xmax, self.ymin, y_split)),
			 Quadrant(self.xmin, x_split, y_split, self.ymax, self.instances_in_bounds(self.xmin, x_split, y_split, self.ymax)),
			 Quadrant(x_split, self.xmax, y_split, self.ymax, self.instances_in_bounds(x_split, self.xmax, y_split, self.ymax)) ]
		
	def instances_in_bounds(self, xmin, xmax, ymin, ymax):
		instances = []
		for instance in self.instances:
			x = instance.coord.x
			y = instance.coord.y
			if ( x > xmin and x < xmax) and ( y > ymin and y < ymax ):
				instances.append(instance)
		return instances
