import re, sys

name = ""
attributes = []
data = []

def arfftoarr(pathtoarff):
    arff = open(pathtoarff,'r')

    for line in arff:
        line = line.replace("\n","")
        if re.match("^@relation", line):
            name = line.split(" ")[1]
        elif re.match("^@attribute", line):
            attributes.append(line.split(" ")[1])
        elif re.match("^@data", line):
            pass
        elif line != "":
            data.append(line.split(","))

    print name
    print attributes
    print data

arfftoarr("arff/telecom1.arff")            
