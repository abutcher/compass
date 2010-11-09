from cluster import *

def GRIDCLUS (quadrants, acceptance=0.3):
    
    tiles = []

    for quadrant in quadrants:
        tiles.append((quadrant.density(), quadrant))
    
    tiles = sorted(tiles, key=lambda tile: tile[0], reverse=True)
    
    clusters = []

    def grab_neighbors(tile):
        neighbors = neighbor_search(tile, tiles, acceptance)
        if (neighbors != []):
            for neighbor in neighbors:
                cluster.append(neighbor)
                if neighbor in tiles:
                    tiles.remove(neighbor)
                grab_neighbors(neighbor)

    for tile in tiles:
        cluster = []
        
        active = tile
        grab_neighbors(tile)

        if (cluster != []):
            cluster.append(active)
            if active in tiles:
                tiles.remove(active)
            cluster = list(set(cluster))
            clusters.append(cluster)

    newclusters = []
    for cluster in clusters:
        newcluster = Cluster()
        for item in cluster:
            newcluster.add_quadrant(item[1])
        newclusters.append(newcluster)

    return newclusters

def adjacent(a, b):
    if ((a.xmin == b.xmax) or (a.xmax == b.xmin)) or ((a.ymin == b.ymax) or (a.ymax == b.ymin)):
        return True
    else:
        return False        

def should_mark(a, b, acceptance):
    if a[0] != 0 and b[0] != 0 and adjacent(a[1], b[1]) and ((acceptance > (abs(a[0] - b[0])/a[0])) or (acceptance > (abs(a[0]-b[0])/b[0]))):
        return True
    else:
        return False

def neighbor_search(tile, tiles, acceptance):
    neighbors = []
    for othertile in tiles:
        if (othertile != tile) and should_mark(tile, othertile, acceptance):
            neighbors.append(othertile)
    return neighbors
