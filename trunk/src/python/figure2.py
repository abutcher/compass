import matplotlib
import matplotlib.font_manager as fm
import matplotlib.lines as lines
matplotlib.use('WXAgg')
matplotlib.interactive(True)
import matplotlib.pyplot as plt
import matplotlib.collections as collections
from matplotlib import cm, colors
import numpy as np
from util import *

class Figure:
    fig = None
    title = None
    ax1 = None
    ax2 = None
    ax3 = None

    def __init__(self, title, instances, quadrants):
        self.fig = plt.figure()
        self.quadrants = quadrants
        self.instances = instances
        self.title = title
        # Set up separate axes
        self.draw_ax1(self.title, self.instances, self.quadrants)
        self.draw_ax3(None)
#        self.ax2 = self.fig.add_axes([0.75,0.7,0.2,0.2])
#        plt.title('Legend')
#        plt.setp(self.ax2, xticks=[], yticks=[])
        cid = self.fig.canvas.mpl_connect('button_press_event', self.onclick)

    def onclick(self, event):
        print 'button=%d, x=%d, y=%d, xdata=%f, ydata=%f'%(
            event.button, event.x, event.y, event.xdata, event.ydata)
        plt.cla()
        picked_quadrant = None
        for quadrant in self.quadrants:
            if event.xdata > quadrant.xmin and event.xdata < quadrant.xmax and event.ydata > quadrant.ymin and event.ydata < quadrant.ymax:
                picked_quadrant = quadrant
                print "xmin %.2f, xmax %.2f, ymin %.2f, ymax %.2f" % (quadrant.xmin, quadrant.xmax, quadrant.ymin, quadrant.ymax)
        if picked_quadrant == None:
            print "No quadrant in selected range."
            self.draw_ax3(None)
        else:
            self.draw_ax3(picked_quadrant)
        plt.draw()

    def draw_ax1(self, title, instances, quadrants):
        self.ax1 = self.fig.add_axes([0.1,0.1,0.6,0.8])
        plt.title(self.title)
        self.ax1.set_xlabel('x')
        self.ax1.set_ylabel('y')
        for i in range(len(instances)-1):
            self.ax1.plot(instances[i].coord.x, instances[i].coord.y, "o", markersize=2, alpha=0.5)
        you = random_element(instances)
        self.ax1.plot(you.coord.x, you.coord.y, "go", markersize=5)
        quadrants = sorted(quadrants, key=lambda quad: quad.qmedian(), reverse=True)
        colors = self.make_n_colors(cm.gray, len(quadrants)*5)
        for quadrant in quadrants:
            self.color_quadrants([quadrant], colors[quadrants.index(quadrant)*5])        

    def draw_ax3(self, quadrant):
        self.ax3 = self.fig.add_axes([0.75,0.7,0.2,0.2])
        plt.title('Information')
        plt.setp(self.ax3, xticks=[], yticks=[])
        if quadrant != None:
            variance = quadrant.qvariance()
            size = len(quadrant.instances)
            med_effort = median(transpose(quadrant.datums())[-1])
            print "variance: %.2f" % (variance)
            print "size: %d" % (size)
            print "median effort: %.2f" % (med_effort)
            plt.text(.06, .8, "Var: %.2f" % variance)
            plt.text(.06, .6, "MEf: %.2f" % med_effort)
            plt.text(.06, .4, "Size: %d" % size)

    def color_quadrants(self, quadrants, color):
        for i in range(len(quadrants)):
            xmin = quadrants[i].xmin
            xmax = quadrants[i].xmax
            ymin = quadrants[i].ymin
            ymax = quadrants[i].ymax
            self.ax1.bar(xmin, (ymax-ymin), width=(xmax-xmin), bottom=ymin, facecolor=color, visible=True, linewidth=0.5)

    def write_png(self):
        plt.savefig("%s.png" % self.title)

    def make_n_colors(self, cmap_name, n):
        cmap = cm.get_cmap(cmap_name, n)
        return cmap(np.arange(n))
        
    def show(self):
        plt.show()
        
        
