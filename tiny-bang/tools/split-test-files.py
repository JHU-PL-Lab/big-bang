#!/usr/bin/python

import sys

f = open(sys.argv[1])
g = None
for line in f:
  if line[0:2] == '--':
    if g is not None: g.close()
    name = line[2:].strip()
    g = open(name + ".tb", 'w')
  else:
    if g is None:
      print "Error: line is not in a file: " + line
      sys.exit(1)
    else:
      g.write(line)
if g is not None:
  g.close()
