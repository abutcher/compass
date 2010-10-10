def GRIDCLUS (quadrants):
    
    tiles = []

    for quadrant in quadrants:
        tiles.append((quadrant.Density(), quadrant))
    
    tiles = sorted(tiles, key=lambda tile: tile[0], reverse=True)
    
    clusters = []

    def grabneighbors(tile):
        neighbors = neighborsearch(tile, tiles)
        if (neighbors != []):
            for neighbor in neighbors:
                cluster.append(neighbor)
                if neighbor in tiles:
                    tiles.remove(neighbor)
                grabneighbors(neighbor)

    for tile in tiles:
        cluster = []
        
        active = tile
        grabneighbors(tile)

        if (cluster != []):
            cluster.append(active)
            if active in tiles:
                tiles.remove(active)
            cluster = list(set(cluster))
            clusters.append(cluster)

    newclusters = []
    for cluster in clusters:
        newcluster = []
        for item in cluster:
            newcluster.append(item[1])
        newclusters.append(newcluster)

    return newclusters

def adjacent(A, B):
    if ((A.xmin == B.xmax) or (A.xmax == B.xmin)) or ((A.ymin == B.ymax) or (A.ymax == B.ymin)):
        return True
    else:
        return False        

def shouldmark(A, B):
    if adjacent(A[1], B[1]) and ((0.1 > (abs(A[0] - B[0])/A[0])) or (0.1 > (abs(A[0]-B[0])/B[0]))):
        return True
    else:
        return False

def neighborsearch(tile, tiles):
    neighbors = []
    for othertile in tiles:
        if (othertile != tile) and shouldmark(tile, othertile):
            neighbors.append(othertile)
    return neighbors
