import config, os, pickle, random
from dataObjects import *

#Global functions:

def save():
  try:
    pickle(config.profile)
    output("Progress saved.")
  except:
    output("Error saving to profile.")
  return

def configure(x):
  if os.access(x, os.F_OK):                     # Check if x exists
    print(x + ' exists: all OK.')               
  else:                                         # Create x
    print('Warning: ' + x + ' does not exist.')
    if x[-1] == '/':                            # Check if x is directory
      print('...creating directory')            # x is directory - create
      os.mkdir(x)
    else:                                       # x is file - create
      print('...creating file')
      f = open(x, 'w')
      f.close()

def setup(home):
  print(home)
  setup = home + '.pycraft/'  # Set pycraft config dir
  configure(setup)        # Check config dir
  config = setup + 'config'   # Set pycraft options file
  configure(config)       # Check options file
  profileFile = setup + 'profiles'
  configure(profileFile)



def prepareGame(module):
  config.level = 0
  config.punisher = []
  config.lives = 5
  config.multiplier = 1
  config.score = 0
  print("shuffling deck")
  random.shuffle(module.levels[config.level].problems)

def score(token):
  try:
    multiplier = multiplier
  except:
    multiplier = 1

  if multiplier > 3:
    multiplier = 4
  else:
    multiplier += 1
  if token:
    config.score = config.score + (multiplier * 1) 
  else:
    multiplier = 0

def lives():
  config.lives -= 1
