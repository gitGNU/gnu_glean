class UI():
  gui = False
  if gui:
    """pygame interface bits"""
    # Initialise screen
    pygame.init()
    screen = pygame.display.set_mode((640, 480))
    pygame.display.set_caption('Pycraft')

    # Fill background
    background = pygame.Surface(screen.get_size())
    background = background.convert()
    background.fill((0, 0, 0))

    # Display some text
    font = pygame.font.Font(None, 36)
    text = font.render("This might be where questions appear", 1, (250, 250, 250))
    textpos = text.get_rect()
    textpos.centerx = background.get_rect().centerx
    background.blit(text, textpos)

    # Blit everything to the screen
    screen.blit(background, (0, 0))
    pygame.display.flip()
    #The below is pygame main loop legacy code:
    """
    for event in pygame.event.get():
      if event.type == pygame.QUIT or (event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE):
        return
      elif event.type == KEYDOWN:
        if event.unicode == ':':
          background = pygame.Surface(screen.get_size())
          background = background.convert()
          background.fill((0, 0, 0))
          text = font.render("Colon pressed", 1, (250, 250, 250))
          textpos = text.get_rect()
          textpos.centerx = background.get_rect().centerx
          background.blit(text, textpos)
          screen.blit(background, (0, 0))
          pygame.display.flip()
          print event
        else:
          pass
    """

  else:
    """If gui is false then we use a CLI interface."""
    def pyprint(output):
      print(output)
    def pyread(read):
      playerAnswer = raw_input(read)
      return playerAnswer
