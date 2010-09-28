#!/sw/bin/gawk -f 

##########################################################################
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Lesser General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
##########################################################################

# which2 : a stochastic anytime rule learner 
# (c) Tim Menzies (tim@menzies.us) 2010, LGLP 3.0 

# This program builds rules by ranking ideas, then repeatedly building new ideas
# by picking # and combining two old ideas (favoring those with higher ranks).  New
# ideas generated in this way are ranked and thrown back into the same pot as
# the old ideas so, if they are any good, they might be picked and extended
# in subsequent rounds. Alternatively, if the new idea stinks, it gets buried
# by the better ideas and is ignore.

# One important aspect of the following is that the scoring routines for
# ideas are completely seperate from the rest of the code (see the "score1"
# function). Hence, it is a simple # matter to try our different search biases.

# e.g. This call produces the following output.
# gawk -f which2.awk titanic.arff 

# In the following, the "candidates" are ideas that look promsing
# and "score" ranks the candidates. If the max score does not improve
# from the last round, then "lives" decreases.

# Each round tries random combinations of the stuff from prior rounds 
# (favoring those things with higher scores). Hence, at round 1,
# all the candidates are singletons. But. later on (see line 54)
# the candidates can grow to combinations of things.

# Each round prunes the candiates so that only the better candiadtes
# surive to round+1.

# The final output are the candidates of the last round (see lines
# 169 to 178). In this example, the best rule is feature3=female.

# To read the following, note that "1 1st" means "feature 1 = 1st".

#     1	
#     2	----------------
#     3	round 1 max 0 lives 5
#     4	
#     5	candidate[ 1,1st ] = 11st
#     6	candidate[ 1,1st,1,2nd ] = 11st,12nd
#     7	candidate[ 1,1st,2,child ] = 11st,2child
#     8	candidate[ 1,1st,3,female ] = 11st,3female
#     9	candidate[ 1,2nd ] = 12nd
#    10	candidate[ 1,2nd,3,female ] = 12nd,3female
#    11	candidate[ 2,child ] = 2child
#    12	candidate[ 3,female ] = 3female
#    13	
#    14	score[ 1,2nd ] = 0.65497458
#    15	score[ 2,child ] = 0.6797711
#    16	score[ 1,1st,2,child ] = 0.68008855
#    17	score[ 1,1st,1,2nd ] = 0.69219043
#    18	score[ 1,1st ] = 0.71384384
#    19	score[ 1,2nd,3,female ] = 0.71400064
#    20	score[ 1,1st,3,female ] = 0.73924712
#    21	score[ 3,female ] = 0.77617045
#    22	
#    23	----------------
#    24	round 2 max 0.77617045 lives 5
#    25	
#    26	candidate[ 1,1st ] = 11st
#    27	candidate[ 1,1st,1,2nd ] = 11st,12nd
#    28	candidate[ 1,1st,1,2nd,2,child ] = 11st,12nd,2child
#    29	candidate[ 1,1st,1,2nd,3,female ] = 11st,12nd,3female
#    30	candidate[ 1,1st,2,child ] = 11st,2child
#    31	candidate[ 1,1st,2,child,3,female ] = 11st,2child,3female
#    32	candidate[ 1,1st,3,female ] = 11st,3female
#    33	candidate[ 1,2nd ] = 12nd
#    34	candidate[ 1,2nd,3,female ] = 12nd,3female
#    35	candidate[ 2,child ] = 2child
#    36	candidate[ 3,female ] = 3female
#    37	
#    38	score[ 1,1st,2,child,3,female ] = 0.67778641
#    39	score[ 2,child ] = 0.6797711
#    40	score[ 1,1st,2,child ] = 0.68008855
#    41	score[ 1,1st,1,2nd,2,child ] = 0.69146934
#    42	score[ 1,1st,1,2nd ] = 0.69219043
#    43	score[ 1,1st ] = 0.71384384
#    44	score[ 1,2nd,3,female ] = 0.71400064
#    45	score[ 1,1st,3,female ] = 0.73924712
#    46	score[ 1,1st,1,2nd,3,female ] = 0.77596332
#    47	score[ 3,female ] = 0.77617045
#    48	
#    49	----------------
#    50	round 3 max 0.77617045 lives 4
#    51	
#    52	candidate[ 1,1st ] = 11st
#    53	candidate[ 1,1st,1,2nd ] = 11st,12nd
#    54	candidate[ 1,1st,1,2nd,2,child ] = 11st,12nd,2child
#    55	candidate[ 1,1st,1,2nd,2,child,3,female ] = 11st,12nd,2child,3female
#    56	candidate[ 1,1st,1,2nd,3,female ] = 11st,12nd,3female
#    57	candidate[ 1,1st,2,child ] = 11st,2child
#    58	candidate[ 1,1st,2,child,3,female ] = 11st,2child,3female
#    59	candidate[ 1,1st,3,female ] = 11st,3female
#    60	candidate[ 1,2nd,3,female ] = 12nd,3female
#    61	candidate[ 2,child ] = 2child
#    62	candidate[ 2,child,3,female ] = 2child,3female
#    63	candidate[ 3,female ] = 3female
#    64	
#    65	score[ 1,1st,2,child ] = 0.68008855
#    66	score[ 2,child,3,female ] = 0.6821201
#    67	score[ 1,1st,1,2nd,2,child,3,female ] = 0.68397115
#    68	score[ 1,1st,1,2nd,2,child ] = 0.69146934
#    69	score[ 1,1st,1,2nd ] = 0.69219043
#    70	score[ 1,1st ] = 0.71384384
#    71	score[ 1,2nd,3,female ] = 0.71400064
#    72	score[ 1,1st,3,female ] = 0.73924712
#    73	score[ 1,1st,1,2nd,3,female ] = 0.77596332
#    74	score[ 3,female ] = 0.77617045
#    75	
#    76	----------------
#    77	round 4 max 0.77617045 lives 3
#    78	
#    79	candidate[ 1,1st ] = 11st
#    80	candidate[ 1,1st,1,2nd ] = 11st,12nd
#    81	candidate[ 1,1st,1,2nd,2,child ] = 11st,12nd,2child
#    82	candidate[ 1,1st,1,2nd,2,child,3,female ] = 11st,12nd,2child,3female
#    83	candidate[ 1,1st,1,2nd,3,female ] = 11st,12nd,3female
#    84	candidate[ 1,1st,2,child ] = 11st,2child
#    85	candidate[ 1,1st,2,child,3,female ] = 11st,2child,3female
#    86	candidate[ 1,1st,3,female ] = 11st,3female
#    87	candidate[ 1,2nd,3,female ] = 12nd,3female
#    88	candidate[ 2,child,3,female ] = 2child,3female
#    89	candidate[ 3,female ] = 3female
#    90	
#    91	score[ 1,1st,2,child ] = 0.68008855
#    92	score[ 2,child,3,female ] = 0.6821201
#    93	score[ 1,1st,1,2nd,2,child,3,female ] = 0.68397115
#    94	score[ 1,1st,1,2nd,2,child ] = 0.69146934
#    95	score[ 1,1st,1,2nd ] = 0.69219043
#    96	score[ 1,1st ] = 0.71384384
#    97	score[ 1,2nd,3,female ] = 0.71400064
#    98	score[ 1,1st,3,female ] = 0.73924712
#    99	score[ 1,1st,1,2nd,3,female ] = 0.77596332
#   100	score[ 3,female ] = 0.77617045
#   101	
#   102	----------------
#   103	round 5 max 0.77617045 lives 2
#   104	
#   105	candidate[ 1,1st ] = 11st
#   106	candidate[ 1,1st,1,2nd ] = 11st,12nd
#   107	candidate[ 1,1st,1,2nd,2,child ] = 11st,12nd,2child
#   108	candidate[ 1,1st,1,2nd,2,child,3,female ] = 11st,12nd,2child,3female
#   109	candidate[ 1,1st,1,2nd,3,female ] = 11st,12nd,3female
#   110	candidate[ 1,1st,2,child ] = 11st,2child
#   111	candidate[ 1,1st,2,child,3,female ] = 11st,2child,3female
#   112	candidate[ 1,1st,3,female ] = 11st,3female
#   113	candidate[ 1,2nd,3,female ] = 12nd,3female
#   114	candidate[ 2,child,3,female ] = 2child,3female
#   115	candidate[ 3,female ] = 3female
#   116	
#   117	score[ 1,1st,2,child ] = 0.68008855
#   118	score[ 2,child,3,female ] = 0.6821201
#   119	score[ 1,1st,1,2nd,2,child,3,female ] = 0.68397115
#   120	score[ 1,1st,1,2nd,2,child ] = 0.69146934
#   121	score[ 1,1st,1,2nd ] = 0.69219043
#   122	score[ 1,1st ] = 0.71384384
#   123	score[ 1,2nd,3,female ] = 0.71400064
#   124	score[ 1,1st,3,female ] = 0.73924712
#   125	score[ 1,1st,1,2nd,3,female ] = 0.77596332
#   126	score[ 3,female ] = 0.77617045
#   127	
#   128	----------------
#   129	round 6 max 0.77617045 lives 1
#   130	
#   131	candidate[ 1,1st ] = 11st
#   132	candidate[ 1,1st,1,2nd ] = 11st,12nd
#   133	candidate[ 1,1st,1,2nd,2,child ] = 11st,12nd,2child
#   134	candidate[ 1,1st,1,2nd,2,child,3,female ] = 11st,12nd,2child,3female
#   135	candidate[ 1,1st,1,2nd,3,female ] = 11st,12nd,3female
#   136	candidate[ 1,1st,2,child ] = 11st,2child
#   137	candidate[ 1,1st,2,child,3,female ] = 11st,2child,3female
#   138	candidate[ 1,1st,3,female ] = 11st,3female
#   139	candidate[ 1,2nd,3,female ] = 12nd,3female
#   140	candidate[ 2,child,3,female ] = 2child,3female
#   141	candidate[ 3,female ] = 3female
#   142	
#   143	score[ 1,1st,2,child ] = 0.68008855
#   144	score[ 2,child,3,female ] = 0.6821201
#   145	score[ 1,1st,1,2nd,2,child,3,female ] = 0.68397115
#   146	score[ 1,1st,1,2nd,2,child ] = 0.69146934
#   147	score[ 1,1st,1,2nd ] = 0.69219043
#   148	score[ 1,1st ] = 0.71384384
#   149	score[ 1,2nd,3,female ] = 0.71400064
#   150	score[ 1,1st,3,female ] = 0.73924712
#   151	score[ 1,1st,1,2nd,3,female ] = 0.77596332
#   152	score[ 3,female ] = 0.77617045
#   153	
#   154	----------------
#   155	round 7 max 0.77617045 lives 0
#   156	
#   157	candidate[ 1,1st ] = 11st
#   158	candidate[ 1,1st,1,2nd ] = 11st,12nd
#   159	candidate[ 1,1st,1,2nd,2,child ] = 11st,12nd,2child
#   160	candidate[ 1,1st,1,2nd,2,child,3,female ] = 11st,12nd,2child,3female
#   161	candidate[ 1,1st,1,2nd,3,female ] = 11st,12nd,3female
#   162	candidate[ 1,1st,2,child ] = 11st,2child
#   163	candidate[ 1,1st,2,child,3,female ] = 11st,2child,3female
#   164	candidate[ 1,1st,3,female ] = 11st,3female
#   165	candidate[ 1,2nd,3,female ] = 12nd,3female
#   166	candidate[ 2,child,3,female ] = 2child,3female
#   167	candidate[ 3,female ] = 3female
#   168	
#   169	score[ 1,1st,2,child ] = 0.68008855
#   170	score[ 2,child,3,female ] = 0.6821201
#   171	score[ 1,1st,1,2nd,2,child,3,female ] = 0.68397115
#   172	score[ 1,1st,1,2nd,2,child ] = 0.69146934
#   173	score[ 1,1st,1,2nd ] = 0.69219043
#   174	score[ 1,1st ] = 0.71384384
#   175	score[ 1,2nd,3,female ] = 0.71400064
#   176	score[ 1,1st,3,female ] = 0.73924712
#   177	score[ 1,1st,1,2nd,3,female ] = 0.77596332
#   178	score[ 3,female ] = 0.77617045

BEGIN  {
    Goal="yes";     # Best = the Goal class. Rest = anything else
	Seed=1          # Random number see.
    More = 1.02;    # Improvement means at least a 2% growth
    Lives=5;        # If no improvement after five rounds, give up
    Dull=0.1;       # Ignore candidates with score < Dull*MaxScore
    Beam=10;        # Only the top (say) 10 candidates survive to the next round
    Samples=20;     # Pick this number of pairs of candidates from the last round
    Pinch = 1/1000; # Add a random number of up to "Pinch" to each score
	OverFitted=3;   # When do we prune a rule that matches on too few instances?
    CONVFMT="%.8g"; # Increase the string size for array contents so we can see the Pinch
    IGNORECASE=1; 
    _ = SUBSEP;
    C=","
}

## --------------------------------------------------
#Data entry. Pretty routine stuff. 
/@attribute/ {Name[++Name[0]]=$2}
             {gsub(/[ \t]*/,"")} # no blanks 
             {gsub(/#.*/,"")}    # no comments
/^$/         {next}              # no blank likes
/@data/      {In=1;FS=","; srand(Seed)}
/@/          {next}
In           {Rows++; 
              train(All,Best,H,Rows,Data,$NF==Goal)}

function train(all,best,h,rows,d,class,   i) {
    h[class]++
    for(i=1;i<=NF;i++)  {
        if ($i == "?") 
            continue;
        if (i == NF) {
            d[rows,i]=class
        } else {
            d[rows,i]=$i
            all[i,$i]++
            if (class)                
                best[i,$i]++ }}
}

# Now we can begin. In round0, offer a rough ranking of
# the ranges. In subequent rounds, randomly select and combine ranges
# from prior randoms.
END   {round0(All,Best,H,Hot0); # make some initial guess
       rounds(1,0,Lives,Hot0,Rows,Data,Out)
      }

# In round one, score by b^/(b+r)
function round0(all,best,h,hot,  i,b,r,memo,score) {
    for(i in best) {
        r = (all[i] - best[i])/h[0]
        b = best[i]/h[1]
        if (b > r) {
            s = b^2/(b+r) + rand()*Pinch
            memo[s] = i
            score[i]= s 
        }}
   chop(score,memo,hot) # prune the dull candidates
}

# Given some score[key]=number and memo[number]=key,
# sort the scores and return the top Beam
# number of keys, pruning all keys less than
# Dull times the max score.
function chop(score0,memo,out,   score,n,i) {
    n=asort(score0,score)
    for(i=n;i>=1;i--) {
        if (score[i] <= score[n]*Dull)
            break;
        if (i <= n - Beam)
            break
        out[memo[score[i]]] = score[i]
    }
}

# In subsequent rounds one, score all the candidates
# by running that combination over the data (see the "score"
# function. Note the "score" paramter that caches prior
# calcuations of the score. This speeds up the code by
# a factor of four (for large data sets).
function rounds(round, max0,lives,hot0,rows,data,out,score,   \
                max,i,sample,hot1,s,memo,hot2) {
    if (round == 1)
        max=0
    else { # terminate if we have stopped improving
        max = most(hot0)    
        lives =  (max > (max0*More)) ? Lives : lives - 1
        if(lives < 0) { # if termination, copy input to out
            for(i in hot0)
                out[i] = hot0[i]
            return max }
    }
    print "\n---| round: " round " seed: " Seed " |-----------------------------------"
    print "max   : " max "\nlives : " lives 
    normalize(hot0)           # make all the counts n= 1..100
    explode(hot0,sample)      # copy items n times
    twos(sample,Samples,hot1) # pick items at random from that sample
    for(i in hot0)            # add in the last rounds' ideas
        hot1[i] = i;
    values(hot1,"candidate")
    for(i in hot1) {          # score the new picks and the last rounds's ideas
        s = (i in score) ? score[i] : score(i,rows,data) + rand()*Pinch
        memo[s] = i
        score[i] = s
        }
    chop(score,memo,hot2)     # prune the dull candidates
    o(hot2,"score","-n -k 5")
    return rounds(round+1,max,lives,hot2,rows,data,out,score)
}

## -----------------------------------------
# Randomly pick pairs and combine them. Note that,
# in the following code, the picks come from "sample"
# where an item may be repeated many times (so things
# that are often repeated are more likely to be picked).

# "n" times, pick two things from "sample"
# and store them in "sampled"
function twos(sample,n,sampled,  pair) {
    while(n--) {
        pair= two(sample)
        sampled[pair]=pair
        }
}

# Pick two things at random. Try not
# to pick the same thing twice. Return
# the combination of the two things you pick.
function two(sample, tries, this, that) {
    this = one(sample)
    if(tries == 9) # nine lives
        return this
    that = one(sample)
    if (this == that)
        return two(sample,tries + 1)
     else
        return combine(this,that)
}

# combine two rules. don't repeat any ranges.
# sort them so that all the ranges of the same
# feature fall together.
function combine(this,that,   used,tmp,out) {
    split(this "," that,tmp,",")
    n=asort(tmp)
    out = tmp[1]
    used[tmp[1]]=1
    for(i=2;i<=n;i++)
        if (!used[tmp[i]]) {
            out = out "," tmp[i]
            used[tmp[i]] = 1
            }
    return out
}

## ---------------------------------
## score a rule by finding its matching rows in data.
function score(i,rows,data,   
			 cols,row, col,a,b,c,d,triggered,pd,pf,prec,acc,supports,s,fits) {
    a=b=c=d=Pinch # stop divide by zero errors
    cols=Name[0]
    for(row=1;row<=rows;row++) {
        triggered = matched(row,cols,data,i)
        if (data[row,cols]) {
            if (triggered) {d++} else {b++}
        } else {
            if (triggered) {c++} else {a++}
        }
    } 
	fits    = c + d
    pd      = d/(b+d)
    pf      = a/(a+c)
    prec    = d/(c+d)
    acc     = (a+d)/(a+b+c+d)
    support = (c+d)/(a+b+c+d)
    return score1(pd,pf,prec,acc,support,fits)
}
function score1(pd,pf,prec,acc,supportm,fits) {
	if (fits <= OverFitted)
		return 0
    if (Eval==1) return acc
    if (Eval==2) return 2 * pd * prec/(pd+prec)
    if (Eval==3) return 2 * pd * pf/(pd+pf)
    if (Eval==4) return support * 2 * pd * pf/(pd+pf)
    return support * 2 * pd * prec/(pd+prec)
}

# Given "this" of the form "f1_v1,f2_v2,...." see if "row" matches "this".
# Assumes that disjunctions are modeled as  configuous values from the
# same feature (this is gaurenteed by "combine"). Hence, whenever
# we move to a new feature, we need to check that at least of the values
# mentioned with the old feature was found.
function matched(row,cols,data,this,    col,n,goals,pair,f0,f,status) {
    n=split(this,goals,",")
    for(col=1;col<=n;col++) {
        split(goals[col],pair,_)
        f = pair[1]
        status[f] += data[row,f] == pair[2]
        if (f0 && (f != f0) && !status[f0]) 
            return 0
        f0 = f
    }
    return status[f]
}

## --------------------------------------
## interesting utils

# Given an array a[i]=n,
# fill up "out" with "n" number
# of "i". This creates a "sample" of
# things, from which we can pick randomly
# biased by the relative frequencies of "n".
# The total size of the sample is stored
# in "sample[0]"
function explode(a, out,i,j) {
    for(i in a)
        for(j=1;j<=a[i];j++)
            out[++out[0]] = i
}

# Pick any item at random from "sample".
# Assumes that the same size is in array
# element "sample[0]"
function one(sample, any) {
    any = int(rand()* sample[0])+1
    return sample[any]
}

## --------------------------------------
## boring utils

# Given an array a[i]=num, normalize
# all the numbers to integers 0..100
function normalize(a, i,sum) {
    for(i in a) sum += a[i]
    for(i in a) a[i] = int(100*a[i]/sum)
}

# combine an feature/ range
function fv(f,v) { return f _ v }

# find the max item in an array
function most(a,   i,max) {
    max = -1000000000
    for(i in a)
        if (a[i] > max)
            max = a[i];
    return max
}

# print array values
function values(a,s,what,  i,j,com) {
	print ""
    what = what ? what : ""
    com = "sort " what
    for(i in a) {
        j=a[i];
        gsub(_,",",j);
        print s": " j | com;
    }
    close(com)
 }	
# print an array, sorted by "what"
function o(a,s,what,   i,j,com) {
   print ""
    what = what ? what : ""
    com = "sort " what
    for(i in a) {
        j=i;
        gsub(_,",",j);
        print s"[ "j" ] = " a[i] | com;
    }
    close(com)
 } 
