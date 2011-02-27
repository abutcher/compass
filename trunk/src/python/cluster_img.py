#!/opt/local/bin/python2.6

import matplotlib
matplotlib.use('WXagg')
from pylab import *
import numpy as np
from matplotlib.transforms import Bbox
from matplotlib.patches import Rectangle

rect1 = Rectangle((0.2, 0.1), 0.25, 0.4, facecolor="#aaaaaa")
rect2 = Rectangle((0.25, 0.45), 0.25, 0.4, facecolor="#aaaaaa")
gca().add_patch(rect1)
gca().add_patch(rect2)
bbox = Bbox.from_bounds(0.2,0.2,0.25,0.4)

show()
