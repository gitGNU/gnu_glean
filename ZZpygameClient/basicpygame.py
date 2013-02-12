#!/usr/bin/python

import pygame, time
from pygame.locals import *

def main():
  # Initialise screen
  pygame.init()
  screen = pygame.display.set_mode((640, 480))
  pygame.display.set_caption('pycraft')

  # Fill background
  background = pygame.Surface(screen.get_size())
  background = background.convert()
  background.fill((0, 0, 0))

  # Display some text
  font = pygame.font.Font(None, 21)
  outputField = font.render("This might be where questions appear", 1, (250, 250, 250))
  outputFieldPosition = outputField.get_rect()
  outputFieldPosition.top = 20
  outputFieldPosition.left = 20
  background.blit(outputField, outputFieldPosition)

  # Blit everything to the screen
  screen.blit(background, (0, 0))
  pygame.display.flip()

  # Event loop
  while 1:
    for event in pygame.event.get():
      if event.type == QUIT:
        return
    screen.blit(background, (0, 0))
    pygame.display.flip()
    time.sleep(2)
    background.fill((0, 0, 0))
    screen.blit(background, (0, 0))
    pygame.display.flip()
    time.sleep(3)
    outputField = font.render("New text", 1, (250, 250, 250))
    background.blit(outputField, outputFieldPosition)
    screen.blit(background, (0, 0))
    pygame.display.flip()


if __name__ == '__main__': main()
