import re, sys

def arfftoarr(pathtoarff):
    arff = open(pathtoarff,'r')
    data = [][]

    for line in arff:
        if re.match(REGULAREXPRESSION, line):
            
