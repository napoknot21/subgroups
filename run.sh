#!/bin/bash

# Run the project
make run

#Check if the directory 'dots' exists
mkdir dots

#Generate a pdf and a image file from the dot file
dot -Tpng ./output.dot -o ./dots/output.png
dot -Tpdf ./output.dot -o ./dots/output.pdf
