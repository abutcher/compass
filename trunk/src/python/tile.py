""" 

Tile Clustering Algorithm

Accepts an already created grid (quadrant) structure and clusters them
using the neighborhood and grid density.


"""

def GRIDCLUS(quadrants):
    # Calculate block densities.
    # This list will have a 1 to 1 relationship with quadrants.
    tiles = []
    active = 0
    cluster = 0

    # Tile is a tuple consisting of (density, quadrant)
    for quadrant in quadrants:
        t = quadrant.Density(), quadrant
        tiles.append(t)

    # Sort by density
    tiles = sorted(tiles, key=lambda tile: tile[0], reverse=True)

    clusters = []

    for tile in tiles:
        active = tile
        neighbors = NeighborSearch(active, tiles)
        cluster  = []
        for neighbor in neighbors:
            if ShouldMark(active[0], neighbor[0]):
                cluster.append(neighbor)
                tiles.remove(neighbor)
        if cluster != []:
            tiles.remove(active)
            cluster.append(active)
            for neighbor in cluster:
                newneighbors = NeighborSearch(neighbor, tiles)
                for newneighbor in newneighbors:
                    if ShouldMark(neighbor[0], newneighbor[0]):
                        cluster.append(newneighbor)
                        tiles.remove(newneighbor)
            clusters.append(cluster)

    rest = []
    for tile in tiles:
        rest.append(tile)

    newclusters = []
    for cluster in clusters:
        newcluster = []
        for item in cluster:
            newcluster.append(item[1])
        newclusters.append(newcluster)

    return newclusters
    
def NeighborSearch(thisTile, tiles):
    neighbors = []
    for tile in tiles:
        if (thisTile != tile) and Neighbor(thisTile[1], tile[1]):
            neighbors.append(tile)
    return neighbors

def Neighbor(A, B):
    if ((A.xmin == B.xmax) or (A.xmax == B.xmin)) or ((A.ymin == B.ymax) or (A.ymax == B.ymin)):
        return True
    else:
        return False

def ShouldMark(DA, DB):
    # If a neighbor and within 10% density of eachother
    if ((0.1 > (abs(DA - DB)/DA)) or (0.1 > (abs(DA-DB)/DB))):
        return True
    else:
        return False
