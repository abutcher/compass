from util import *

class Instance:
	coord = None
	datum = None

	def __init__(self, coord, datum):
		self.coord = coord
		self.datum = datum

	def Coord(self):
                return [self.coord.x, self.coord.y, self.klass()]
	
	def klass(self):
		return self.datum[-1]
	
class InstanceCollection:
	instances = []
	max_x = 0
	max_y = 0
	
	def __init__(self, data_collection):
		east, west = data_collection.find_poles()
		base_d = distance(east, west)

		for datum in data_collection.datums:
			a = distance(west, datum)
			b = distance(east, datum)
			x = (b**2 - base_d**2 - a**2) / (-2 * base_d)
			if x > self.max_x:
				self.max_x = x
			try:
                                y = math.sqrt(a**2 - x**2)
                        except ValueError:
                                y = 0
			if y > self.max_y:
				self.max_y = y
			self.instances.append(Instance(DataCoordinate(x,y), datum))

	def normalize_coordinates(self):
		for instance in self.instances:
			instance.coord.x = instance.coord.x / self.max_x
			instance.coord.y = instance.coord.y / self.max_y
	
	def log_x_coordinates(self):
		for instance in self.instances:
			instance.coord.x = math.log(instance.coord.x + 0.0001)

	def log_y_coordinates(self):
		for instance in self.instances:
			instance.coord.y = math.log(instance.coord.y + 0.0001)

	def klasses(self):
		return [ inst.klass for inst in self.instances ]
	
	def coords(self):
		return [ inst.coord for inst in self.instances ]

	def datums(self):
		return [ inst.datum for inst in self.instances ]


	def k_fold_stratified_cross_val(self, k=10):
		bins = []
		bin_count = []
		random.shuffle(self.instances,random.random)
		if not isnumeric(self.instances[0].datum[-1]):
			data = sort_by_class(self.instances)
		for i in range(k):
			bins.append([])
			bin_count.append(0)
		for instance in self.instances:
			try:
				index = bin_count.index(0)
				bins[index].append(instance)
				bin_count[index] = 1
			except:
				for i in range(k):
					bin_count[i]=0
				index = bin_count.index(0)
				bins[index].append(instance)
				bin_count[index] = 1
		return bins

class DataCoordinate:
	def __init__(self, x, y):
		self.x = x
		self.y = y
	
class DataCollection:
	datums = []

	def __init__(self, datums):
		self.datums = datums
	
	def add_datum(self, datum):
		datums.append(datum)

	def find_poles(self):
		this = random_element(self.datums)
		self.datums.remove(this)
		east = farthest_from(this, self.datums)
		self.datums.remove(east)
		self.datums.append(this)
		west = farthest_from(east, self.datums)
		self.datums.append(east)
		self.datums.append(west)
		return east, west
