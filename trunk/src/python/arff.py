import csv
from util import *

class Arff:
    """ Collect the data from a .arff file. """

    def __init__(self, filename=None):
        self.data = []
        self.headers = []
        if type(filename) == list:
            if type(filename[0]) == str:
                for addthis in filename:
                    self.extract(addthis)
            # If filename is a list of lists, then it's a
            # pre-processed dataset that we can just assign
            # to self.data and move on.
            elif type(filename[0]) == list:
                self.data = filename
        else:
            self.extract(filename)
            self.extract_headers(filename)

    def extract(self, path):
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

    def extract_headers(self, path):
        infile = open(path, "r")
        for line in infile:
            if "@attribute" in line:
                self.headers.append(line.split(" ")[1])

    def output_lisp(self, table, path):
        outfile = open(path, "w")
        outfile.write("(defun " + table  + " ()\n")
        outfile.write("\t(data\n")
        outfile.write("\t\t:name '" + table + "\n")
        outfile.write("\t\t:columns '( COLUMNS HERE )\n")
        outfile.write("\t\t:egs '(\n")
        for row in self.data:
            string=""
            for item in row:
                string = string + " " + str(item)
            string = "\t\t\t(" + string + ")\n"
            outfile.write(string)
        outfile.write(")))\n")
