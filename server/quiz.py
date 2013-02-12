import random

def evaluateAnswer():
  if playerAnswer == solution:
    score.increase()
    question.dead()
    return
  else:
    score.decrease()
    question.undead()
    return

class Profile():
  multiplier = 1
  basescore = 1
  score = 0
  name = ""

  def decrease():
    multiplier = 1
  def increase():
    if multiplier < 5:
      multiplier += 1
    score += (basescore * multiplier)

player1 = Profile

