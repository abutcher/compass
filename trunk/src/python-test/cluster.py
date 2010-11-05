def class cluster:
    quadrants = []
    # scoring something something
    
    def addQuadrant(self, quadrant):
        quadrants.add(quadrant)
        
    def Datums(self):
        datums = []
        for q in quadrants:
            datums.extend(q.Datums())
        return datums
