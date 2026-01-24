#!/bin/bash
# Profile execution time and allocations, generate interactive HTML visualization

set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <path-to-cnf-file>"
    echo "Example: $0 tests/satlib/flat30-60/sat/flat30-1.cnf"
    exit 1
fi

CNF_FILE="$1"

if [ ! -f "$CNF_FILE" ]; then
    echo "Error: File '$CNF_FILE' not found"
    exit 1
fi

echo "Running lilsat with time profiling on: $CNF_FILE"
cabal run lilsat -O2 --enable-profiling -- "$CNF_FILE" +RTS -p -RTS

if [ ! -f "lilsat.prof" ]; then
    echo "Error: lilsat.prof was not generated"
    exit 1
fi

echo ""
echo "Profiling data generated: lilsat.prof"
echo ""

# Check if profiteur is installed
if ! command -v profiteur &> /dev/null; then
    echo "profiteur not found. Install it with:"
    echo "  cabal install profiteur"
    echo ""
    echo "You can view the raw profile at: lilsat.prof"
    exit 0
fi

echo "Generating interactive visualization with profiteur..."
profiteur lilsat.prof

if [ -f "lilsat.prof.html" ]; then
    echo ""
    echo "✓ Visualization generated: lilsat.prof.html"
    echo ""

    # Try to open in browser
    if command -v xdg-open &> /dev/null; then
        echo "Opening in browser..."
        xdg-open lilsat.prof.html
    elif command -v firefox &> /dev/null; then
        echo "Opening in Firefox..."
        firefox lilsat.prof.html &
    else
        echo "Open lilsat.prof.html in your browser to view the profile"
    fi
else
    echo "Error: Failed to generate HTML visualization"
    exit 1
fi
