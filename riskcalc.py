#!/usr/bin/python

import optparse
#import Tkinter

P = {   111:15./36,
        110:21./36,
        121:55./216,
        120:161./216,
        211:125./216,
        210:91./216,
        222:295./1296,
        221:420./1296,
        220:581./1296,
        311:855./1296,
        310:441./1296,
        322:2890./7776,
        321:2611./7776,
        320:2275./7776}

ATTACK_MIN = 2 # Minimum armies to attack with

def multi_battle(a,t):
    """ multiple battles in  a row """
    b = [[a+1,0,1]]
    for d in t:
        for s in b:
            s[1] = d
            if s[0] > 1:
                s[0] = s[0] - 1
        while not _battle_over(b):
            b = roll(b)
            b = _combine_results(b)
    print b,pcheck(b)
    return b

def battle(a,d):
    b = [[a,d,1]]
    while not _battle_over(b):
        b = roll(b)
        b = _combine_results(b)
    print b,pcheck(b)
    return b
        
def _battle_over(battle):
    """ determine if battle is finished """
    for set in battle: # check each results set
        if not _set_over(set): # if set is not over
            return False       # then the battle continues
    return True #battle is over if all sets are over

def _set_over(set):
    """ determine whether this set is over """
    a = set[0]
    d = set[1]
    p = set[2]
    if a < ATTACK_MIN or d == 0:
        return True
    else:
        return False

def roll(input_set):
    """ returns result matrix from a battle with multiple possible beginnings
    input_set = [s1,s2,s3,...]
    where s = [a,d,p]
    where a = attacking armies
          d = defending armies
          p = probability of situation
    """
    n = []
    for s in input_set:
        if _set_over(s):
            n.append(s)
            continue
        p = s[2]
        result = _roll_dice(s[0],s[1])
        for r in result:
            n.append([r[0],r[1],p*r[2]])
    return n

def _roll_dice(a,d):
    """ returns possible results from a dice roll 
    a = number of attacking armies
    b = number of defending armies
    
    """
    if a > 3:
        if d > 1: # roll max dice (32)
            return [[a,d-2,P[322]],[a-1,d-1,P[321]],[a-2,d,P[320]]]
        elif d == 1: # 31
            return [[a,d-1,P[311]],[a-1,d,P[310]]]
    elif a == 3:
        if d > 1: #22
            return [[a,d-2,P[222]],[a-1,d-1,P[221]],[a-2,d,P[220]]]
        elif d == 1: #21
            return [[a,d-1,P[211]],[a-1,d,P[210]]]
    elif a == 2:
        if d > 1: #12
            return [[a,d-1,P[121]],[a-1,d,P[120]]]
        elif d == 1: #11
            return [[a,d-1,P[111]],[a-1,d,P[110]]]
    return [a,d,1]

def pcheck(result):
    ''' Makes sure that all p's add up to 1. '''
    p = 0
    for r in result:
        p += r[2]
    return p-result[-1][2]

def _combine_results_old(r):
    """ combine probabilites of sets where a=a and d=d
    """
    for n1,set1 in enumerate(r):
        for n2,set2 in enumerate(r):
            if n1 is not n2 and _is_dup(set1,set2):
                r[n1][2] = r[n1][2] + r[n2][2]
                r.pop(n2)
    return r

def set_id(set):
    if set[0] == 1:
        return '1'
    else:
        return str(set[0])+':'+str(set[1])

def _combine_results(seq, idfun=set_id): 
    # order preserving
    if idfun is None:
        def idfun(x): return x[0]
    seen = {}
    result = []
    for item in seq:
        marker = idfun(item)
        # in old Python versions:
        # if seen.has_key(marker)
        # but in new ones:
        if marker in seen: continue
        seen[marker] = 1
        p = 0
        for item2 in seq:
            if idfun(item2) == marker:
                p += item2[2]
        newitem = [item[0],item[1],p]
        result.append(newitem)
    return result

def _is_dup(a,b):
    """ determine if two sets match """
    if a[0] == b[0] and a[1] == b[1]:
        return True
    elif a[0] == 1 and b[0] == 1:
        return True
    else:
        return False

def _battle_results(b):
    x = []
    y = []
    for row in b:
        x.insert(0,row[0])
        y.insert(0,row[2]*100)
        if len(y) > 1 and len(y) < len(b):
            y[0] += y[1]
    return (x,y)

def make_chart(b):
    """make chart from battle results. """
    x,y = _battle_results(b)
    Lx = len(x) # max x value
    Lmax = 20 # max number of labels
    D = Lx/Lmax+1 # Divider
    x = [x[i]-1 for i in range((Lx+D-2)%D+1,Lx,D)]
    from GChartWrapper import Line
    G = Line(y).title('Risk Battle')
    #G.scale(0,1)
    G.axes('xy')
    G.axes.label(0,'fail',*x)
    #G.axes.label(0,1,*[int(x*len(y)) for x in [.2,.4,.6,.8,1]])
    #G.save('risk_battle.png')
    return G

def show_chart():
    pass

def test():
    #multi_battle(10,[4,1,1])
    #make_chart(multi_battle(43,[20,40,1]))
    # a = roll([[5,2,1]])
    # b = roll(a)
    # bb = _combine_results(b)
    # c = roll(b)
    # bb = _combine_results(c)
    # d = roll(c)
    # dd = _combine_results(d)
    # print 'a',a
    # print 'b',b
    # print 'c',c
    # print 'd',d
    # print 'dd',dd
    #make_chart(battle(11,10))
    #make_chart(multi_battle(52,[20,20,1,1,1,1]))
    #_battle_results(battle(50,60))
    make_chart(multi_battle(26,[10,2,2,2,2,2]))

def main():
    usage = "usage: %prog <attacking armies> <defending armies> [<d2> <d3> ...]"
    parser = optparse.OptionParser(usage)
    p, args = parser.parse_args()
    if len(args) < 2:
        print "Invalid Arguments"
        print usage
    elif len(args) == 2:
        a = eval(args[0])
        d = eval(args[1])
        b = battle(a,d)
    elif len(args) > 2:
        a = eval(args[0])
        d = [eval(x) for x in args[1:]]
        print a,d
        b = multi_battle(a,d)
#   chart = make_chart(b)
#   chart.save('tmp.png')
if __name__ == "__main__":
    main()
