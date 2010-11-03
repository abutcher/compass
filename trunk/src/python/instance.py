class Instance:
	coords = None
	datum = None
	Class = None

	def __init__(self, coords, datum, Class=None):
		self.coords = coords
		self.datum = datum
                self.Class = Class
