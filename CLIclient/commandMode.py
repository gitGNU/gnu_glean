#Source file containing commandMode
from functions import *
from dataObjects import *
import config

def commandMode(playerOption):
  while True:
    if playerOption == ":":
      playerOption = raw_input(":")
    else:
      playerOption = playerOption[1:]
      
    if playerOption == "quit" or playerOption == "q":
      return "exit"
    elif playerOption == "menu" or playerOption == "m":
      return "mainMenu"
    elif playerOption == "restart" or playerOption == "r":
      return "restart"
    elif playerOption == "help" or playerOption == "h":
      output("Enter q to quit, m to return to the main menu or r to restart.")
      playerOption = ":"
    elif playerOption == "save" or playerOption == "s":
      save()
      playerOption = ":"
    elif playerOption == "":
      return "return"
    else:
      output("Invalid command. Enter h to receive help.")
      playerOption = ":"

