class Normal:
    def __init__(self,data):
        

def Naive-Bayes-Classify(instance, data, m=2, k=1):
    (ListOfClasses,ClassFrequency) = Classes(data)
    for ClassIndex in range(ListOfClasses):
        # Calculate the prior
        Prior = math.log( ClassFrequency[ClassIndex] + k ) / ( len(data) + ( k * len(ListOfClasses)))
        for FeatureIndex in range(len(instance)-1):
            # Gaussian PDF function.  May fix for Discrete values later.
            

        

# Returns list of discrete classes and frequency of classes
def Classes(data):
    Classes = []
    Frequency = []
    if isinstance(data, ndarray):
        for instance in data:
            try:
                # Try to increment a Classes frequency by one watching for the ValueError exception if the Class doesn't exist.
                Frequency[Classes.index(instance[len(instance)-1])] = Frequency[Classes.index(instance[len(instance)-1])] + 1
            except ValueError:
                # Add the class to the list and add a frequency count of one.
                Classes.append(instance[len(instance)-1])
                Frequency.append(1)
    return Classes, Frequency

