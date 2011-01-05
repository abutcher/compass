#!/usr/bin/env python

from arff import *
from bore2 import *

arff = Arff("arff/defect/kc1.arff")

bore = Bore(arff.data, arff.headers, "trueyes")
print bore.top_rules(3)
