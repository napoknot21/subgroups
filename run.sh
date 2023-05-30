#!/bin/bash

# Define a usage function
usage() {
  echo "Usage: $0 -n <n_value> [-p] [-g] [-o output_file_name]"
  exit 1
}

# Initialize parameters
n_value=""
p_flag=""
g_flag=""
o_value=""

# Parse command-line arguments
while getopts ":n:pgo:" opt; do
  case ${opt} in
    n )
      n_value=$OPTARG
      ;;
    p )
      p_flag="-p"
      ;;
    g )
      g_flag="-g"
      ;;
    o )
      o_value="-o $OPTARG"
      ;;
    \? )
      usage
      ;;
    : )
      echo "Invalid option: $OPTARG requires an argument" 1>&2
      usage
      ;;
  esac
done
shift $((OPTIND -1))

# Check that n was provided
if [ -z "$n_value" ]; then
  echo "You must provide a value for n."
  usage
fi

# Run the OCaml program with the specified parameters
dune exec bin/main.exe -- -n "$n_value" $p_flag $g_flag $o_value

# Run the project
# dune exec subgroups

#Check if the directory 'dots' exists
# mkdir dots

#Generate a pdf and a image file from the dot file
#dot -Tpng ./output.dot -o ./dots/output.png
#dot -Tpdf ./output.dot -o ./dots/output.pdf
