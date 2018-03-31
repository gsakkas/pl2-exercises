import sys
import requests
from hashlib import sha256
from HTMLParser import HTMLParser


class MyHTMLParser(HTMLParser):
    def __init__(self):
        HTMLParser.__init__(self)
        self.data = []
        self.get = False
        self.found = False
        self.correct = False
        self.finished = False

    def handle_starttag(self, tag, attrs):
        if tag == 'span' and ('class', 'question') in attrs:
            self.found = True
        elif tag == 'p' and ('class', 'right') in attrs:
            self.correct = True
        elif tag == 'input' and ('value', 'Play again!') in attrs:
            self.finished = True

    def handle_data(self, data):
        if self.found:
            self.data.append(str(data.split()[-1]))
            self.found = False
            self.get = True
        elif self.correct:
            self.data.append(data)
            self.found = False
            self.get = True

    def get_data(self):
        if self.get:
            self.get = False
            ret_data = self.data[0]
            self.data = []
            return ret_data

    def is_correct(self):
        if self.correct:
            self.correct = False
            return True

    def is_finished(self):
        if self.finished:
            self.finished = False
            return True


# Get website with bitcoin game
url = sys.argv[1]

# Create a Session
ses = requests.Session()
headers = {'User-Agent': 'Mozilla/5.0'}

# Get request
req = ses.get(url, headers=headers)
cookies = requests.utils.cookiejar_from_dict(
    requests.utils.dict_from_cookiejar(ses.cookies))

# Create a HTMLParser
parser = MyHTMLParser()

# Find solution
parser.feed(req.text)
magic_code = parser.get_data()

# Game info
game = 1
round = 1

for i in xrange(42):
    n = 0
    print ("- Game " + str(game) + ", Round " + str(round) +
           ", magic code: " + magic_code)
    while True:
        # Find a solution for given magic code
        initial = sha256(hex(n)).hexdigest()
        fst = sha256(initial.decode("hex")).hexdigest()
        snd = sha256(fst.decode("hex")).hexdigest()
        if snd[:4] == magic_code:
            print "Tested bitcoins: " + str(n)
            print initial
            data = {'answer': initial, 'submit': 'Submit!'}
            req = ses.post(url, headers=headers,
                           data=data, cookies=cookies)
            parser.feed(req.text)
            # Submit finished site
            if parser.is_finished():
                parser.is_correct()
                print parser.get_data()
                print "Finished game! Starting new..."
                data = {'again': 'Play again!'}
                req = ses.post(url, headers=headers)
                parser.feed(req.text)
                magic_code = parser.get_data()
                game += 1
                round = 0
            # Submit correct site but not finished
            elif parser.is_correct():
                print parser.get_data()
                data = {'again': 'Continue!'}
                req = ses.post(url, headers=headers,
                               data=data, cookies=cookies)
                parser.feed(req.text)
                magic_code = parser.get_data()
            else:
                print "Something went wrong. Exiting..."
                exit(-1)
            break
        n += 1
    round += 1
