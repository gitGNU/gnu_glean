class problem():
  question = ""
  solution = ""

class level():
  problems = []

class module():
  levels = []

  def populate():
    x = 0
    for i in os.listdir(moduleDir): #This should only read Level files
      i = level()
      levels.append(i)
      try:
        levels[x] = pickle.load(i)
        x += 1
      except:
        print("Can't read level file")
        exit()

        

    """read questions from files into dictionary (question:solution) etc. """
    """Not dictionary because they aren't indexable"""
    """readline file module questions:
      append.problems([line x in questions, line x in answers])
      """
    length = len(problems) - 1 #length does not start counting with 0. Random and index does, hence - 1

  def summon():
    question = problems[random.randint(0, length)]
    print question
    """generate random question from within the dictionary limits."""
  def undead():
    """add question to necromancer"""
    length = len(problems) - 1 #length does not start counting with 0. Random and index does, hence - 1
  def dead():
    """remove question from dictionary"""
