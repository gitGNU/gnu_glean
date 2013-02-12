#!/usr/bin/python

"""This script converts plain level text files (questions1 and solutions1, questions2 and solutions2, etc.) into pickled level files (level1, level2, level3, etc.)."""

#Imports
import pickle, os, sys, config, dataObjects, functions

#Variables
plainModules = config.home + "OldModules/"  #directory of to-be-converted files
moduleDir = config.home + "modules/"        #pycraft modules directory
moduleName = ""               #will contain the name of the module to be converted
modulePath = ""                   #will point to the whole path of the to-be-converted module
level = 0                      #postfix of questions, solutions and level files that will be processed. This increases over the duration of this script until the script has reached the final level
target = ""                   #will point to the whole path of the new converted module

#Functions
def convertModule():
  level = 0
  while True:
    questions = []
    solutions = []
    x = 0
    try:
      with open(modulePath + "questions" + str(level)) as f: 
        for line in f:
          line = line.strip(' \n')
          line = line.lower()
          questions.append(line)
          x += 1
    except:
      print("No more levels to process")
      break
    print("adding level" + str(level))
    config.module.addLevel()
    with open(modulePath + "solutions" + str(level)) as f:
      for line in f:
        line = line.strip('\n ')
        line = line.lower()
        solutions.append(line)
    y = 0
    while y < x:
      config.module.levels[level].addProblem(questions[y], solutions[y])
      y += 1
    print("...loaded entire level" + str(level) + ".")
    level += 1
      
#Logic
while moduleName == "" or moduleName == None:
  moduleName = functions.chooseModule("OldModules/")
modulePath = plainModules + moduleName + "/"
target = moduleDir + moduleName
functions.configure(target)
config.module = dataObjects.Module(moduleName) 
print("Now trying to convert " + modulePath + ", level " + str(level) + "...")
convertModule()
config.module.display()
print("opening " + target)
with open(target, "w") as f:
  print("saving")
  pickle.dump(config.module, f)
print("Conversion should be complete.")
