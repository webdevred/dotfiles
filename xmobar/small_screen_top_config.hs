Config { font = "DejaVu Sans Mono 10"
       , borderColor = "black"
       , border = FullB
       , bgColor = "black"
       , fgColor = "#ffaaff"
       , alpha = 128
       , position = Top
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
       , commands = [ Run Cpu ["-L","3","-H","50", "--normal","#ffaaff","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "}{ %memory% | %cpu% | %date%"
       }
