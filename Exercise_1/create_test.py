from random import randint as ri

N = 200000
M = 1000000000
file = open('test_big', 'w')
file.truncate()
file.write(str(N) + " " + str(M) + "\n")

for idx in xrange(N):
    file.write(str(ri(1, M)) + " ")

file.close()
