class node:
    right = None
    left = None
    data = None
    variance = None

    def __init__(self, data):
        self.data = data
        self.variance = variance(data)
