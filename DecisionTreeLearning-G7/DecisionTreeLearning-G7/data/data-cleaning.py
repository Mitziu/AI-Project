import urllib2
link = "https://archive.ics.uci.edu/ml/machine-learning-databases/tic-tac-toe/tic-tac-toe.data"
reponse = urllib2.urlopen(link)
content = response.read()
print(content)
