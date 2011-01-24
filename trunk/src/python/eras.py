import math

def eras(data, n):
    buckets = [[] for i in range(int(math.ceil(len(data)/n)))]
    for i in range(len(buckets)):
        while len(buckets[i]) < n:
            buckets[i].append(data[0])
            data.remove(data[0])
    if len(data) > 0:
        last_bucket = []
        for instance in data:
            last_bucket.append(instance)
    buckets.append(last_bucket)
    return buckets

#arff = Arff("arff/defect/kc1.arff")
#for bucket in eras(arff.data, 50):
#    print len(bucket)
        
