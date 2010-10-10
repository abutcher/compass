""" Statistics for a list of quadrants """

def PerformanceScore(cluster):
    Data = []
    for quadrant in cluster:
        for instance in quadrant.Data:
            Data.append(instance.datum)
            
    if type(Data[0][-1]) is str:
        # This is a DEFECT set, so we calculate PD/PF for the true class
        Stats = DefectStats()
        for instance in Data:
            Data.remove(instance)
            Got = Classify(instance, Data, "DEFECT")
            Want = instance[len(instance)-1]
            if Got is Want:
                if Got is "true":
                    Stats.incf("TRUE","d")
                    Stats.incf("FALSE","a")
                elif Got is "false":
                    Stats.incf("FALSE","d")
                    Stats.incf("TRUE","a")
            elif Got is not Want:
                if Got is "true":
                    Stats.incf("TRUE","c")
                    Stats.incf("FALSE","b")
                elif Got is "false":
                    Stats.incf("FALSE","b")
                    Stats.incf("TRUE","c")
                    Data.append(instance)
            return [Stats.pd("TRUE"),Stats.pf("TRUE")]
        else:
            # This is likely an EFFORT set, so we calculate Median Magnitude of Relative Error
            MRE = []
            for instance in Data:
                Data.remove(instance)
                MRE.append(MRE(instance[-1],Classify(instance, Data, "EFFORT")))
                Data.append(instance)
            return median(MRE)

def Classify(instance, Data, InputType=None):
    if InputType is None:
        if type(Data[0][-1]) is str:
            InputType="DEFECT"
        else:
            InputType="EFFORT"
    if InputType == "DEFECT":
        Neigbors = kNearestNeighbors(instance,Data)
        return NaiveBayesClassify(instance,Neighbors)
    elif InputType == "EFFORT":
        Neighbors = kNearestNeighbors(instance,Data)
        return median(Neighbors)
    

        
