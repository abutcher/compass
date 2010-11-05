class Instance:
	coords = None
	datum = None
	class = None

	def __init__(self, coords, datum, class=None):
		self.coords = coords
		self.datum = datum
                self.class = class
