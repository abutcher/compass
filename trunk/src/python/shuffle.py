from util import *

def shuffle(data):
    shuffled=[]
    for i in range(len(data)):
        re = randomelement(data)
        shuffled.append(re)
        data.remove(re)
    return shuffled
