import math
import random
import time
#from Tkinter import *
from arff import *

def distance(instance1, instance2):
    if instance1 == None or instance2 == None:
        return float("inf")
    sumOfSquares = 0
    for i in range(1, len(instance1) - 1):
        sumOfSquares += (instance1[i] - instance2[i])**2
    return sumOfSquares

def loadCSV(fileName):
    fileHandler = open(fileName, "rt")
    lines = fileHandler.readlines()
    fileHandler.close()
    del lines[0] # removes the header
    dataset = []
    for line in lines:
        instance = lineToTuple(line)
        dataset.append(instance)
    return dataset

def lineToTuple(line):
    # remove leading/trailing witespace and newlines
    cleanLine = line.strip()
    # get rid of quotes
    cleanLine = cleanLine.replace('"', '')
    # separate the fields
    lineList = cleanLine.split(",")
    # convert strings into numbers
    stringsToNumbers(lineList)
    lineTuple = tuple(lineList)
    return lineTuple

# Destructively converts all the elements from the second one
# to floating point numbers.
# Parameters:
#   myList: a list of the format ["whatever", "number1", "number2", ...]
# Returns None
def stringsToNumbers(myList):
    if (len(myList) > 1):
        for i in range(1, len(myList)):
            myList[i] = float(myList[i])

def printTable(instances):
    for instance in instances:
        if instance != None:
            line = instance[0] + "\t"
            for i in range(1, len(instance)):
                line += "%.2f " % instance[i]
            print line

def meanInstance(name, instanceList):
    numInstances = len(instanceList)
    if (numInstances == 0):
        return
    numAttributes = len(instanceList[0])
    means = [name] + [0] * (numAttributes-1)
    for instance in instanceList:
        for i in range(1, numAttributes - 1):
            means[i] += instance[i]
    for i in range(1, numAttributes):
        means[i] /= float(numInstances)
    return tuple(means)

def assign(instance, centroids):
    minDistance = distance(instance, centroids[0])
    minDistanceIndex = 0
    for i in range(1, len(centroids)):
        d = distance(instance, centroids[i])
        if (d < minDistance):
            minDistance = d
            minDistanceIndex = i
    return minDistanceIndex

def createEmptyListOfLists(numSubLists):
    myList = []
    for i in range(numSubLists):
        myList.append([])
    return myList

def assignAll(instances, centroids):
    clusters = createEmptyListOfLists(len(centroids))
    for instance in instances:
        clusterIndex = assign(instance, centroids)
        clusters[clusterIndex].append(instance)
    return clusters

def computeCentroids(clusters):
    centroids = []
    for i in range(len(clusters)):
        name = "cluster" + str(i)
        centroid = meanInstance(name, clusters[i])
        centroids.append(centroid)
    return centroids

def kmeans(instances, k, animation=False):
    result = {}
    # randomly select k initial centroids
    random.seed(time.time())
    centroids = random.sample(instances, k)
    prevCentroids = []
    if animation:
        delay = 1.0 # seconds
        canvas = prepareWindow(instances)
        clusters = createEmptyListOfLists(k)
        clusters[0] = instances
        paintClusters2D(canvas, clusters, centroids, "Random centroids")
        #drawPoints(canvas, instances, "black", "circle")
        time.sleep(delay)
    iteration = 0
    while (centroids != prevCentroids):
        iteration += 1
        clusters = assignAll(instances, centroids)
        if animation:
            paintClusters2D(canvas, clusters, centroids, "Assign %d" % iteration)
            time.sleep(delay)
        prevCentroids = centroids
        centroids = computeCentroids(clusters)
        withinness = computeWithinness(clusters, centroids)
        if animation:
            paintClusters2D(canvas, clusters, centroids,
                            "Update %d, withinness %.1f" % (iteration, withinness))
            time.sleep(delay)
    result["clusters"] = clusters
    result["centroids"] = centroids
    result["withinness"] = withinness
    return result

def computeWithinness(clusters, centroids):
    result = 0
    for i in range(len(centroids)):
        centroid = centroids[i]
        cluster = clusters[i]
        for instance in cluster:
            result += distance(centroid, instance)
    return result

# Repeats k-means clustering n times, and returns the clustering
# with the smallest withinness
def repeatedKMeans(instances, k, n):
    bestClustering = {}
    bestClustering["withinness"] = float("inf")
    for i in range(1, n+1):
        print "k-means trial %d," % i ,
        trialClustering = kmeans(instances, k)
        print "withinness: %.1f" % trialClustering["withinness"]
        if trialClustering["withinness"] < bestClustering["withinness"]:
            bestClustering = trialClustering
            minWithinnessTrial = i
    print "Trial with minimum withinness:", minWithinnessTrial
    return bestClustering

def extractAttribute(instances, index):
    result = []
    for instance in instances:
        result.append(instance[index])
    return result

def paintCircle(canvas, xc, yc, r, color):
    canvas.create_oval(xc-r, yc-r, xc+r, yc+r, outline=color)

def paintSquare(canvas, xc, yc, r, color):
    canvas.create_rectangle(xc-r, yc-r, xc+r, yc+r, fill=color)

def drawPoints(canvas, instances, color, shape):
    random.seed(0)
    width = canvas.winfo_reqwidth()
    height = canvas.winfo_reqheight()
    margin = canvas.data["margin"]
    minX = canvas.data["minX"]
    minY = canvas.data["minY"]
    maxX = canvas.data["maxX"]
    maxY = canvas.data["maxY"]
    scaleX = float(width - 2*margin) / (maxX - minX)
    scaleY = float(height - 2*margin) / (maxY - minY)
    for instance in instances:
        x = 5*(random.random()-0.5)+margin+(instance[1]-minX)*scaleX
        y = 5*(random.random()-0.5)+height-margin-(instance[2]-minY)*scaleY
        if (shape == "square"):
            paintSquare(canvas, x, y, 5, color)
        else:
            paintCircle(canvas, x, y, 5, color)
    canvas.update()

def connectPoints(canvas, instances1, instances2, color):
    width = canvas.winfo_reqwidth()
    height = canvas.winfo_reqheight()
    margin = canvas.data["margin"]
    minX = canvas.data["minX"]
    minY = canvas.data["minY"]
    maxX = canvas.data["maxX"]
    maxY = canvas.data["maxY"]
    scaleX = float(width - 2*margin) / (maxX - minX)
    scaleY = float(height - 2*margin) / (maxY - minY)
    for p1 in instances1:
        for p2 in instances2:
            x1 = margin + (p1[1]-minX)*scaleX
            y1 = height - margin - (p1[2]-minY)*scaleY
            x2 = margin + (p2[1]-minX)*scaleX
            y2 = height - margin - (p2[2]-minY)*scaleY
            canvas.create_line(x1, y1, x2, y2, fill=color)
    canvas.update()

def mergeClusters(clusters):
    result = []
    for cluster in clusters:
        result.extend(cluster)
    return result

def prepareWindow(instances):
    width = 500
    height = 500
    margin = 50
    root = Tk()
    canvas = Canvas(root, width=width, height=height, background="white")
    canvas.pack()
    canvas.data = {}
    canvas.data["margin"] = margin
    setBounds2D(canvas, instances)
    paintAxes(canvas)
    canvas.update()
    return canvas

def setBounds2D(canvas, instances):
    attributeX = extractAttribute(instances, 1)
    attributeY = extractAttribute(instances, 2)
    canvas.data["minX"] = min(attributeX)
    canvas.data["minY"] = min(attributeY)
    canvas.data["maxX"] = max(attributeX)
    canvas.data["maxY"] = max(attributeY)

def paintAxes(canvas):
    width = canvas.winfo_reqwidth()
    height = canvas.winfo_reqheight()
    margin = canvas.data["margin"]
    minX = canvas.data["minX"]
    minY = canvas.data["minY"]
    maxX = canvas.data["maxX"]
    maxY = canvas.data["maxY"]
    canvas.create_line(margin/2, height-margin/2, width-5, height-margin/2,
                       width=2, arrow=LAST)
    canvas.create_text(margin, height-margin/4,
                       text=str(minX), font="Sans 11")
    canvas.create_text(width-margin, height-margin/4,
                       text=str(maxX), font="Sans 11")
    canvas.create_line(margin/2, height-margin/2, margin/2, 5,
                       width=2, arrow=LAST)
    canvas.create_text(margin/4, height-margin,
                       text=str(minY), font="Sans 11", anchor=W)
    canvas.create_text(margin/4, margin,
                       text=str(maxY), font="Sans 11", anchor=W)
    canvas.update()


def showDataset2D(instances):
    canvas = prepareWindow(instances)
    paintDataset2D(canvas, instances)

def paintDataset2D(canvas, instances):
    canvas.delete(ALL)
    paintAxes(canvas)
    drawPoints(canvas, instances, "blue", "circle")
    canvas.update()

def showClusters2D(clusteringDictionary):
    clusters = clusteringDictionary["clusters"]
    centroids = clusteringDictionary["centroids"]
    withinness = clusteringDictionary["withinness"]
    canvas = prepareWindow(mergeClusters(clusters))
    paintClusters2D(canvas, clusters, centroids,
                    "Withinness: %.1f" % withinness)
    
def paintClusters2D(canvas, clusters, centroids, title=""):
    canvas.delete(ALL)
    paintAxes(canvas)
    colors = ["blue", "red", "green", "brown", "purple", "orange"]
    for clusterIndex in range(len(clusters)):
        color = colors[clusterIndex%len(colors)]
        instances = clusters[clusterIndex]
        centroid = centroids[clusterIndex]
        drawPoints(canvas, instances, color, "circle")
        if (centroid != None):
            drawPoints(canvas, [centroid], color, "square")
        connectPoints(canvas, [centroid], instances, color)
    width = canvas.winfo_reqwidth()
    canvas.create_text(width/2, 20, text=title, font="Sans 14")
    canvas.update()


data1 = [
('Amy', 5, 3),
('Bob', 1, 2),
('Chris', 3, 1),
('Dave', 7, 2),
('Erin', 4, 10),
('Fran', 1, 5),
('Gayle', 7, 3),
('Hellen', 5, 5),
('Ian', 6, 5),
('John', 6, 5),
]

#data2 = loadCSV("survey2.csv")
#showDataset2D(data1)
#showDataset2D(data2)
#printTable(data1)
#printTable(data2)
#clustering = kmeans(data1, 2, True)
#clustering = kmeans(data2, 2, True)
#clustering = repeatedKMeans(data2, 2, 50)
#showClusters2D(clustering)
#printTable(clustering["centroids"])

#dataset = loadCSV("/home/davide/15-110/datasets/data-J.csv")
#dataset = loadCSV("foo.csv")
#showDataset2D(dataset)
#clustering = kmeans(dataset, 3, True)


if __name__ == "__main__":
    arff = Arff("arff/defect/kc1.arff")
    clustering = kmeans(arff.data, 10)
    print clustering["clusters"]
