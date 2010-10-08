from util import *

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

        # This Quadrant structure made this mad easy. Well played, Andrew! -Kel
        def Density(self):
                N = len(self.Data)
                Volume = (XMax - Xmin) * (YMax - YMin)
                return N/Volume

        # Classify given an instance.
        # Optional parameter lets you explictly choose.
        # "DEFECT" for defect sets, "EFFORT" for effort sets.
        # Peeks at the data by default.
        # Returns the median effort score or discrete classification.
        def Classify(self, instance,InputType=None):
                if InputType is None:
                        if isinstance(str,self.Data[0][len(self.Data)-1]):
                                InputType="DEFECT"
                        else:
                                InputType="EFFORT"
                if InputType == "DEFECT":
                        Neigbors = kNearestNeighbors(instance,self.Data)
                        return NaiveBayesClassify(instance,Neighbors)
                elif InputType == "EFFORT":
                        Neighbors = kNearestNeighbors(instance,self.Data)
                        return median(Neighbors)

        # Return a PD/PF combo for DEFECT sets or an MDMRE for EFFORT sets
        def PerformanceScore(self):
                if isinstance(str,self.Data[0][len(self.Data)-1]):
                        # This is a DEFECT set, so we calculate PD/PF for the true class
                        print("DO IT... DISCRETELY!")
                else:
                        # This is likely an EFFORT set, so we calculate Median Magnitude of Relative Error
                        print("DO IT... NUMERICALLY!")
