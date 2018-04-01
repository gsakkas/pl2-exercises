import sys
from hashlib import sha256

magic_code = sys.argv[1]

n = 0
while True:
    initial = sha256(hex(n)).hexdigest()
    fst = sha256(initial.decode("hex")).hexdigest()
    snd = sha256(fst.decode("hex")).hexdigest()
    if snd[:4] == magic_code:
        print initial
        break
    n += 1
