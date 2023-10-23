

class Done:
    def __init__(self, val):
        self.val = val
        
        
class Call:
    def __init__(self, rest):
        self.rest = rest
        


def run(tc):
    while not isinstance(tc, Done):
        tc = tc.rest()
    return tc.val
    
    
def is_even(n):
    if n == 0:
        return Done(True)
    if n == 1:
        return Done(False)
    return Call(lambda: is_odd(n-1))
    
    

    
    
def is_odd(n):
    if n == 0:
        return Done(False)
    if n == 1:
        return Done(True)
    return Call(lambda: is_even(n-1))


print(run(is_even(10000000)))
