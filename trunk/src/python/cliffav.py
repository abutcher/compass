from arff import *
from cliffBORE import *
from discretize import *

def main():
    arff = Arff("arff/defect/kc1.arff")
    data = discretize(arff.data)
    bore = CliffBORE(data)
    print bore.prototypes
