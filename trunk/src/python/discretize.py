import arff
from util import *

def discretize(data, n=10):
    trans = transpose(data)
    for column in trans:
        if isnumeric(column[0]):
            scol = sorted(column)
            for i in range(len(column)):
                column[i] = (scol.index(column[i]) / (len(scol) / n))
    return transpose(trans)

