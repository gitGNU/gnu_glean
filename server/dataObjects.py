#!/bin/python
#This file will contain the objects that will hold gaming data, such as profile information, 
# modules etc.

class Problem():
  def __init__(self, question, solution):
    self.question = question
    self.solution = solution

class Level():
  def __init__(self):
    self.problems = []
    self.problem = 0
  def addProblem(self, question, solution):
    self.problems.append(Problem(question, solution))
  def chooseQuestion(self):
    if self.problem >= len(self.problems):
      self.problem = 0
    question = self.problems[self.problem].question
    self.problem += 1
    return question
  def evaluateAnswer(self, answer):
    answer = answer.lower()
    if answer == self.problems[self.problem - 1].solution:
      return True
    else:
      return False

class Module():
  def __init__(self, moduleName):
    self.name = moduleName
    self.levels = []
  def addLevel(self):
    self.levels.append(Level())

  def display(self):
    x = 0
    y = 0
    z = len(self.levels) 
    while x < z:
      y += len(self.levels[x].problems)
      x += 1
    print("""displaying """ + self.name + """
==================""")
    print("""
Total number of questions in module: """ + str(y) + """
=========================================""")
    x = 0
    for i in self.levels:
      print("""
printing level""" + str(x) + """
===============""")
      for j in self.levels[x].problems:
        print("Question: " + str(j.question) + " Solution: " +str(j.solution))
      x +=1

class Profile():
  def __init__(self, profileName):
    self.name = profileName
    self.badges = []
    self.settings = {}

  def display(self):
    print(self.name)
    print(self.badges)
    print(self.settings)
