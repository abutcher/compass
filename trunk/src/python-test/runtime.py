import time
import sys

def print_timing(func):
    def wrapper(*arg):
        t1 = time.time()
        res = func(*arg)
        t2 = time.time()
        sys.stdout.write('%s, %0.3f\n' % (func.func_name, (t2-t1)*1000.0))
        return res
    return wrapper
