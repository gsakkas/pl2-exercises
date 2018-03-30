import sys
from hashlib import sha256

inp = sys.argv[1]
n = 0
while True:
    initial = sha256(hex(n)).hexdigest()
    fst = sha256(initial.decode("hex")).hexdigest()
    snd = sha256(fst.decode("hex")).hexdigest()
    if snd[:4] == inp:
        print initial
        break
    n += 1
print n
