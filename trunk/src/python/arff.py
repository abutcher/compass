import csv
from util import *

class Arff:
    data=[]

    def __init__(self, filename):
        self.Extract(filename)

    def Extract(self, path):
        try:
            reader = csv.reader(open(path, "r"))
        except IOError:
            print "Cannot open: " + path

        for row in reader:
            if len(row) > 1 and '@' not in row[0]:
                for i in range(len(row)):
                    if isnumeric(row[i]):
                        row[i] = float(row[i])
                self.data.append(row)
