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
o_value="output.dot"

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
      o_value=$OPTARG
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

# Check if the -o flag is provided
if [ -n "$o_value" ]; then
  # Verify if the output file name has .dot extension
  if [[ $o_value != *.dot ]]; then
    echo "The output file must have a .dot extension."
    usage
  fi
fi

# Run the OCaml program with the specified parameters
dune exec bin/main.exe -- -n "$n_value" $p_flag $g_flag $o_value

# Check if the 'dots' directory exists, create it if not
if [ ! -d "dots" ]; then
  mkdir dots
fi

# Generate a PNG and PDF file from the dot file
if [ -n "$o_value" ]; then
  dot -Tpng "$o_value" -o "dots/output.png"
  dot -Tpdf "$o_value" -o "dots/output.pdf"
fi
