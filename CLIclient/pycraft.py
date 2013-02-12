#!/usr/bin/python

# Imports (various python libraries used throughout
import os, pickle, sys
sys.path.append('../server')
from commandMode import commandMode
import config, functions, modules, profiles
import time

# Variables:
target = "./config"

# Functions:
def output(string):
  print(string)

def saveMenu(settings):
  with open(target, "w") as f:
    output("saving")
    pickle.dump(settings, f)

def mainMenu(settings):
  while True:
    i = 0
    for each in settings:
      if settings[each] == None:
        settings[each] = "Change setting"

    for each in settings:
      i += 1
      output(str(i) + ".) " + each + ": " + settings[each])
    output("[P]lay")
    output("[Q]uit")
    playerOption = raw_input("Please enter the number in front of the line to change the setting, p to play or q to quit.") 
    if playerOption == "p":
      saveMenu(settings)
      output("Now loading 'game'")
      choice = 0
      while choice != "exit" and choice != "mainMenu":
        choice = game(settings)
      return choice
    elif playerOption == "1":
      config.profile = chooseProfile()
      settings["profile"] = config.profile
    elif playerOption == "2":
      config.module, settings["module"] = chooseModule() #retrieve both module for playing and module name for mainMenu listing?
    elif playerOption == "3":
      output("Option to change level.")
    elif playerOption == "q":
      saveMenu(settings)
      return "exit"
    else:
      output("invalid option please retry.")

def chooseProfile():
  print("""
        =======================
        # Profile  Selection: #
        =======================
        """)
  listProfiles = profiles.request()
  x = 0
  keys = {}
  for i in listProfiles:
    x += 1 
    print(str(x) + ".) " + i)
  x += 1
  print(str(x) + ".) new profile")
  print("0.) delete profile")
  playerAnswer = raw_input("Please choose the profile you would like use:")
  if playerAnswer == str(0):
    print("Please specify profile to be deleted")
    playerAnswer = raw_input()
    try:
      profile.pop(int(playerAnswer) - 1)
    except:
      print("""Please enter a valid profile number for deletion.""")
      exit()
  elif playerAnswer != str(x):
    try:
      settings["profile"] = listProfiles[int(playerAnswer) - 1]
    except:
      print("""Please enter the number before the line of your choice.""")
      exit()
  else:
    print("Please enter a new profile name:")
    playerAnswer = raw_input()
    settings["profile"] = profiles.create(playerAnswer, settings)

  settings["profile"] = profiles.load(settings["profile"])
  if settings["profile"] == False:
    exit()
  else:
    return settings["profile"]

def chooseModule():
  print("""
        =======================
        #  Module Selection:  #
        =======================
        """)
  listModules = modules.request()
  x = 0
  for i in listModules:
    x += 1
    print(str(x) + ".) " + i)
  playerAnswer = raw_input("Please choose the module you would like use:")
  try:
    module = listModules[int(playerAnswer) - 1]
  except:
    print("""Please enter the number before in front of the module of your choice.""")
  module = modules.load(module)
  if module == False:
    exit()
  else:
    return module, module.name

def game(profile, module, level):
  loadModule(module)
  prepareGame(config.module)
  # Event loop
  while True:
    if config.lives < 1:
      print("You died.")
      print("You're final score was: " + str(config.score))
      print("Now going back to menu")
      return "mainMenu"
    else:
      print(config.module.levels[config.level].chooseQuestion())

    playerOption = raw_input("Enter your answer (or command preceded by ':'):\n")
    try:
      y = playerOption[0]
    except:
      playerOption = " "

    #Check to see if playerOption is command.
    #If so, call commandMode.
    if playerOption[0] == ":":
      output("COMMAND MODE (h for help)")
      choice = commandMode(playerOption)
      if choice == "return":
        output("GAME RESUMED")
        pass
      else:
        return choice
    else:
      if config.module.levels[config.level].evaluateAnswer(playerOption):
        print("answer is true")
        score(True)
        print(config.score)
      else:
        print("answer is false")
        score(False)
        lives()
        print(config.lives)
        "necromancer()"

def main(settings):
  functions.setup(config.home) #Check necessary files exist

  #Main Event Loop
  choice = 0
  while choice != "exit":
    choice = mainMenu(settings)

  output("Thank you for playing. Until next time.")
  exit()

#Game Logic
print("Welcome to pycraft")
#Invocation below needs to allow for both parameter based launch and non-para launch.
#If non-para launch, then read values from file.

try:
  with open(target) as f:
    settings = pickle.load(f)
except:
  output("Settings not read - loading blank.")
  settings = {"profile":"Change profile", "module":"Change module", "level":"Change level"}

if __name__ == '__main__': main(settings)
