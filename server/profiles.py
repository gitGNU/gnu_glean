import os, config, pickle, dataObjects

def request():
  profiles = []
  x = 0
  for y in os.listdir(config.profileDir):
    x += 1
    profiles.append(y)
  return profiles

def load(profile):
  with open(config.profileDir + profile) as f:
    try:
      profile = pickle.load(f)
      return profile.name
    except:
      return False
    
def create(profileName, settings):
  """Function that will allow for creation of new profiles"""
  profile = dataObjects.Profile(profileName)
  profile.settings = settings
  with open(config.profileDir + profile.name, "w") as f:
    pickle.dump(profile, f)

  return profile.name
