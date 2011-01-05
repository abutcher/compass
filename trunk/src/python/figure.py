import matplotlib.font_manager as fm
import matplotlib.lines as lines
import matplotlib.pyplot as plt
import matplotlib.collections as collections
from matplotlib import cm, colors
import numpy as np

class Figure:
    fig = None
    title = None
    ax1 = None
    ax2 = None
    ax3 = None

    def __init__(self, title, instances, quadrants, clusters, culled_clusters, cluster_stats, clusters_pre_stats, clusters_test_culled_stats, nb_stats):
        self.fig = plt.figure()
        self.title = title
        # Set up separate axes
        self.ax1 = self.fig.add_axes([0.1,0.1,0.6,0.8])
        plt.title(self.title)
        self.ax1.set_xlabel('x')
        self.ax1.set_ylabel('y')

        self.ax2 = self.fig.add_axes([0.75,0.7,0.2,0.2])
        plt.title('Legend')
        plt.setp(self.ax2, xticks=[], yticks=[])

        # Legend stuff

        good_label = "Good, HM %.2f" % (cluster_stats.HarmonicMean("TRUE"))
        nb_label = "NB, HM %.2f" % (nb_stats.HarmonicMean("TRUE"))
        idea_label = "No Cull, HM %.2f" % (clusters_pre_stats.HarmonicMean("TRUE"))
        bad_label = "Bad, HM %.2f" % (clusters_test_culled_stats.HarmonicMean("TRUE"))
        
        self.ax1.bar(0,0, width=0, bottom=0, facecolor='green', label=good_label)
        self.ax1.bar(0,0, width=0, bottom=0, facecolor='gray', label=bad_label)
        self.ax1.bar(0,0, width=0, bottom=0, facecolor='white', label=nb_label)
        self.ax1.bar(0,0, width=0, bottom=0, facecolor='white', label=idea_label)        


        handles, labels = self.ax1.get_legend_handles_labels()
        l = self.ax2.legend(handles, labels, loc='center', prop=dict(family='sans-serif',size=8))
        l.get_frame().set_linewidth(0)

        self.ax3 = self.fig.add_axes([0.75,0.3,0.2,0.3])
        plt.title('Distribution')
        plt.setp(self.ax3, xticks=[], yticks=[])
        
        # Pie chart stuff
        good = 0
        for cluster in clusters:
            good += len(cluster.datums())
        bad = 0
        for cluster in culled_clusters:
            bad += len(cluster.datums())

        patches, texts, autotexts = self.ax3.pie([good, bad], colors=['green', 'gray'], autopct='%1.1f%%')
        proptease = fm.FontProperties()
        proptease.set_size('xx-small')
        plt.setp(autotexts, fontproperties=proptease)
        plt.setp(texts, fontproperties=proptease)

        """
        if type(instances[0].klass()) is str:
            for instance in instances:
                if instance.klass().lower() == "true":
                    self.ax1.plot(instance.coord.x, instance.coord.y, "bo", markersize=2, alpha=0.5)
                else:
                    self.ax1.plot(instance.coord.x, instance.coord.y, "ro", markersize=2, alpha=0.5)
        else:
            for instance in instances:
                ax1.plot(instance.coord.x, instance.coord.y, 'o', markersize=2, alpha=0.5)
        """
        colors = self.make_n_colors(cm.cool, len(clusters)*30)

        #for i in range(len(clusters)):
        #    if clusters[i].mark:
        #        self.color_quadrants(clusters[i].quadrants, 'purple')
        #    else:
        #        self.color_quadrants(clusters[i].quadrants, colors[i*30])
        #    for quadrant in clusters[i].quadrants:
        #        for instance in quadrant.instances:
        #            if instance.klass().lower() == "true":
        #                self.ax1.plot(instance.coord.x, instance.coord.y, "bo", markersize=2, alpha=0.5)
        #            else:
        #                self.ax1.plot(instance.coord.x, instance.coord.y, "ro", markersize=2, alpha=0.5)

        for instance in instances:
            if instance.klass().lower() == "true":
                self.ax1.plot(instance.coord.x, instance.coord.y, "bo", markersize=2, alpha=0.5)
            else:
                self.ax1.plot(instance.coord.x, instance.coord.y, "ro", markersize=2, alpha=0.5)

        for cluster in clusters:
            self.color_quadrants(cluster.quadrants, 'green')
        for cluster in culled_clusters:
            self.color_quadrants(cluster.quadrants, 'gray')

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
        
        
        
        
