#!/opt/local/bin/python2.6

import matplotlib
matplotlib.use('WXagg')
from matplotlib import pyplot, mpl

# Make a figure and axes with dimensions as desired.
fig = pyplot.figure(figsize=(8,3))
ax1 = fig.add_axes([0.05, 0.65, 0.9, 0.15])
pyplot.setp(ax1, xticks=[1,2,3,4,5,6,7,8])

# Set the colormap and norm to correspond to the data for which
# the colorbar will be used.
cmap = mpl.cm.gray
norm = mpl.colors.Normalize(vmin=1, vmax=8)

# ColorbarBase derives from ScalarMappable and puts a colorbar
# in a specified axes, so it has everything needed for a
# standalone colorbar.  There are many more kwargs, but the
# following gives a basic continuous colorbar with ticks
# and labels.
cb1 = mpl.colorbar.ColorbarBase(ax1, cmap=cmap,
                                norm=norm,
                                orientation='horizontal')
cb1.set_label('Score')
cb1.set_ticks([1,2,3,4,5,6,7,8])
pyplot.show()
