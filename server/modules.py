import os, config, pickle

def request():
  modules = []
  x = 0
  for y in os.listdir(config.moduleDir):
    x += 1
    modules.append(y)
  return modules

def load(module):
  with open(config.moduleDir + module) as f:
    try:
      module = pickle.load(f)
      return module
    except:
      return False
