

Config {
    position = Top,
    font = "xft:Fixed-9",
    bgColor = "#000000",
    fgColor = "#ffffff",
    lowerOnStart = True,
    commands = [
        Run Cpu ["-t","<total>:CPU <user>:U <system>:S <iowait>:IO","-L","30","-H","60","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC","-w","3"] 10,
        Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Swap ["-t","Swap: <usedratio>%","-H","1024","-L","512","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run DynNetwork ["-t","Net: <rx> <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Date "%a %b %_d %l:%M" "date" 10,
        Run Battery ["-t","<acstatus>", "-f","ADP1/online",
                     "-L", "10", "-H", "80", "-p", "3",
                     "-l", "red", "-h", "green",
                     "--", "-o", "BAT:<left>%"
                    ] 60,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %cpu% | %memory% | %swap% | %dynnetwork% | %battery% | <fc=#FFFFCC>%date%</fc>"
}
