#!/bin/bash
# Profile heap usage, generate visualization

set -e

if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
    echo "Usage: $0 <path-to-cnf-file> [profile-type]"
    echo ""
    echo "Profile types:"
    echo "  -hy  Profile by type (default)"
    echo "  -hc  Profile by cost centre"
    echo "  -hd  Profile by closure description"
    echo ""
    echo "Example: $0 tests/satlib/flat30-60/sat/flat30-1.cnf -hc"
    exit 1
fi

CNF_FILE="$1"
HEAP_TYPE="${2:--hy}"  # Default to -hy if not specified

if [ ! -f "$CNF_FILE" ]; then
    echo "Error: File '$CNF_FILE' not found"
    exit 1
fi

# Validate heap type
case "$HEAP_TYPE" in
    -hy|-hc|-hd) ;;
    *)
        echo "Error: Invalid profile type '$HEAP_TYPE'"
        echo "Valid options: -hy, -hc, -hd"
        exit 1
        ;;
esac

echo "Running lilsat with heap profiling ($HEAP_TYPE) on: $CNF_FILE"
cabal run lilsat -O2 --enable-profiling -- "$CNF_FILE" +RTS "$HEAP_TYPE" -RTS

if [ ! -f "lilsat.hp" ]; then
    echo "Error: lilsat.hp was not generated"
    exit 1
fi

echo ""
echo "Heap profile data generated: lilsat.hp"
echo ""

# Check if hp2pretty is installed
if command -v hp2pretty &> /dev/null; then
    echo "Generating SVG visualization with hp2pretty..."
    hp2pretty lilsat.hp

    if [ -f "lilsat.svg" ]; then
        echo ""
        echo "✓ Visualization generated: lilsat.svg"
        echo ""

        # Try to open in viewer
        if command -v xdg-open &> /dev/null; then
            echo "Opening in default viewer..."
            xdg-open lilsat.svg
        elif command -v firefox &> /dev/null; then
            echo "Opening in Firefox..."
            firefox lilsat.svg &
        else
            echo "Open lilsat.svg in your browser or SVG viewer"
        fi
    else
        echo "Error: Failed to generate SVG visualization"
        exit 1
    fi
else
    # Fallback to hp2ps
    echo "hp2pretty not found, falling back to hp2ps..."
    echo "For better output, install hp2pretty with:"
    echo "  cabal install hp2pretty"
    echo ""

    if command -v hp2ps &> /dev/null; then
        hp2ps -c lilsat.hp

        if [ -f "lilsat.ps" ]; then
            echo "✓ Visualization generated: lilsat.ps"
            echo ""

            # Try to convert to PDF if ps2pdf is available
            if command -v ps2pdf &> /dev/null; then
                echo "Converting to PDF..."
                ps2pdf lilsat.ps lilsat-heap.pdf
                echo "✓ PDF generated: lilsat-heap.pdf"

                if command -v xdg-open &> /dev/null; then
                    xdg-open lilsat-heap.pdf
                else
                    echo "Open lilsat-heap.pdf to view the profile"
                fi
            else
                echo "Open lilsat.ps with a PostScript viewer"
            fi
        fi
    else
        echo "hp2ps not found. You can view the raw profile at: lilsat.hp"
    fi
fi
