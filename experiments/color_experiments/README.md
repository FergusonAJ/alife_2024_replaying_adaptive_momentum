# Motivation

We want to make sure we get the colors right for our many plots in this paper. 

The goals for these plots are three-fold: 
1. Make them as color-blind friendly as possible
2. Make difference as clearly apparent as possible for all readers
3. Try and find colors that aren't too ugly. 

# What is this directory?

This directory contains a tool to generate a sample Muller plot (as in the paper) with an easy-to-change color scheme. 
While all of our analyses are in R, this tool is written in Python to make it accessible to a wider audience. 

# Running the tool

The main driver of this tool is the `main.py` file in the `analysis` directory. 
It expects you to run this python script **from within the `analysis` directory**. 

As such, here are the commands to run this tool (assuming a POSIX system): 
```
cd analysis
python3 main.py
```

You may have to change the second line to whatever command you use to run Python3. 

This will run the program, loading in all needed data and generating the Muller plot. 

# Changing the color scheme

The color scheme lives in `data/color_data.txt`. 
Most details are included in that file. A few quick notes here: 
- The order of the colors is what determines the color scheme. 
- Colors are denoted in dexadecimal (e.g., #ff0000)

The program will always pull the colors from this file (see next section). 

# Realtime vs headless execution

You have two options for running this tool: 

1. In a realtime interactive way, where the tool is active showing you the generated image
2. In a headless way, in which the program runs once and closes

You can swap these modes by toggling the `show_frontend` variable at the top of `main.py`. 

In realtime mode (`show_frontend = True`), you can press the spacebar to instantly reload the color palette and regenerate the image. 

In headless mode (`show_frontend = False`), there is no interface, the script merely generates the image and saves it as a file (`plots/muller.png`). 

Note that the file will always be generated, regardless of modality. 

# Dependencies

Beyond Python3 itself, this tool has one library dependency: pygame. 

If you need to install pygame, please see their installation guide here: https://www.pygame.org/wiki/GettingStarted

# Help! It's not working

Please check the terminal to see if it printed a warning / error message. 

The most likely cause is that the color file was incorrect, in which case a warning message _should_ be printed to the terminal. 

If that's not it, feel free to ping me! 
