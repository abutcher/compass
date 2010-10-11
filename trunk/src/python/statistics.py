# This contains functions for defect prediction statistics.  If you're looking
# for related functions such as MRE() that aren't in this file, check util.py.

class DefectStats:
    # [a,b,c,d]
    TRUE = [0,0,0,0]
    FALSE = [0,0,0,0]

    def __init__(self):
        self.TRUE = [0,0,0,0]
        self.FALSE = [0,0,0,0]

    def incf(self,CLASS, pos):
        if CLASS is "TRUE":
            if pos is "a":
                self.TRUE[0] = self.TRUE[0] + 1
#                print(self.TRUE)
            elif pos is "b":
                self.TRUE[1] = self.TRUE[1] + 1
#                print(self.TRUE)
            elif pos is "c":
                self.TRUE[2] = self.TRUE[2] + 1
#                print (self.TRUE)
            elif pos is "d":
                self.TRUE[3] = self.TRUE[3] + 1
#                print (self.TRUE)
        elif CLASS is "FALSE":
            if pos is "a":
                self.FALSE[0] = self.FALSE[0] + 1
            elif pos is "b":
                self.FALSE[1] = self.FALSE[1] + 1
            elif pos is "c":
                self.FALSE[2] = self.FALSE[2] + 1
            elif pos is "d":
                self.FALSE[3] = self.FALSE[3] + 1

    def precision(self,CLASS):
        try:
            return float(self.__D__(CLASS)) / float((self.__C__(CLASS) + self.__D__(CLASS)))
        except:
            return 0.0

    def accuracy(self,CLASS):
        try:
            return float((self.__A__(CLASS) + self.__D__(CLASS))) / float((self.__A__(CLASS) + self.__B__(CLASS) + self.__C__(CLASS) + self.__D__(CLASS)))
        except:
            return 0.0

    def pd(self,CLASS):
        try:
            return float(self.__D__(CLASS)) / float((self.__B__(CLASS) + self.__D__(CLASS)))
        except:
            return 0.0

    def pf(self,CLASS):
        try:
            return float(self.__A__(CLASS)) / float((self.__A__(CLASS) + self.__C__(CLASS)))
        except:
            return 0.0

    def HarmonicMean(self,CLASS):
        try:
            return float((2 * self.pf(CLASS) * self.pd(CLASS))) / float((self.pf(CLASS) + self.pd(CLASS)))
        except:
            return 0.0

    # Private classes for grabbing A,B,C, and D
    def __A__(self,CLASS):
        if CLASS is "TRUE":
            return self.TRUE[0]
        elif CLASS is "FALSE":
            return self.FALSE[0]

    def __B__(self,CLASS):
        if CLASS is "TRUE":
            return self.TRUE[1]
        elif CLASS is "FALSE":
            return self.FALSE[1]

    def __C__(self,CLASS):
        if CLASS is "TRUE":
            return self.TRUE[2]
        elif CLASS is "FALSE":
            return self.FALSE[2]

    def __D__(self,CLASS):
        if CLASS is "TRUE":
            return self.TRUE[3]
        elif CLASS is "FALSE":
            return self.FALSE[3]

