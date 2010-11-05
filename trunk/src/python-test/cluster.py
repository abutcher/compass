class Cluster:
    quadrants = []
    # scoring something something

    def __init__(self, quadrants):
        self.quadrants = quadrants

    def __init__(self):
        self.quadrants = []
    
    def add_quadrant(self, quadrant):
        self.quadrants.append(quadrant)
        
    def datums(self):
        instances = []
        for quadrant in self.quadrants:
            instances.extend(quadrant.instances)
        return [ inst.datum for inst in instances ]
        
