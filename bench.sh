# This script accepts a benchmark name from the command line,
# runs that benchmark, and parses the output to a CSV file.
stack bench --benchmark-arguments "-m prefix $1" 2>&1 | \
    grep "time" | \
    sed 's/time[[:space:]]*//' | \
    sed 's/s.*/s/' | \
    sed 's/[[:space:]]/,/' > bench.csv
