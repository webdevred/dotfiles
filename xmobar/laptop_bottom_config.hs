Config
  { font = "DejaVu Sans Mono 14"
  , borderColor = "black"
  , border = FullB
  , bgColor = "black"
  , fgColor = "#ffaaff"
  , alpha = 128
  , position = BottomH 17
  , textOffset = -1
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "."
  , allDesktops = True
  , overrideRedirect = True
  , textOutputFormat = Ansi
  , sepChar = "%"
  , alignSep = "}{"
  , commands = [Run XMonadLog]
  , template = "%XMonadLog%"
  }
