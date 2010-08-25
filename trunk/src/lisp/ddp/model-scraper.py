#!/usr/bin/env python

import sys
import re

infile = open(sys.argv[1], 'r')
mcost = []
roimpact = []
mreffect = []
rapl = []
oweight = []

for line in infile:
    line = line.replace("ddpData->","")
    line = line.replace("["," ")
    line = line.replace("]"," ")
    line = line.replace(";","")
    line = line.replace("\n","")
    line = line.replace("=", "")
    if re.match("^ *mCost", line):
		temp = line.split(" ")
		mcost.append(temp[2])
    if re.match("^ *roImpact", line):
		temp = line.split(" ")
		roimpact.append("(make-ro-impact :r " + str(int(temp[1])-1) + " :o " + str(int(temp[3])-1) + " :impact " + temp[4] + ")")
    if re.match("^ *mrEffect", line):
		temp = line.split(" ")
		mreffect.append("(make-mr-effect :m " + str(int(temp[1])-1) + " :r " + str(int(temp[3])-1) + " :effect " + temp[4] + ")")
    if re.match("^ *rAPL", line):
		temp = line.split(" ")
		rapl.append(temp[2])
    if re.match("^ *oWeight", line):
		temp = line.split(" ")
		oweight.append(temp[2])


print "(make-ddp-model"        

s = ":o-weight (list"
for weight in oweight:
    s = s + " " + weight
s = s + ")"
print s

s = ":m-cost (list"
for cost in mcost:
    s = s + " " + cost
s = s + ")"
print s

s = ":r-apl (list"
for risk in rapl:
    s = s + " " + risk
s = s + ")"
print s

print ":ro-impacts (list"
for var in roimpact:
    print var
print ")"

print ":mr-effects (list"
for var in mreffect:
    print var
print ")"

print ")"
