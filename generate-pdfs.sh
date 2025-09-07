#!/usr/bin/env bash

# PDF Generation Script for Binocular Documentation
# Generates PDFs from Markdown with Mermaid diagram support

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check if mmdc is available
if ! command -v mmdc &> /dev/null; then
    echo -e "${RED}❌ mermaid-cli (mmdc) not found. Please ensure you're in the nix develop environment.${NC}"
    exit 1
fi

# Check if a browser is available for mmdc (macOS users may need to install Chrome/Chromium)
check_browser() {
    if command -v chromium &> /dev/null; then
        echo -e "${BLUE}Using Chromium browser for diagram rendering${NC}"
        return 0
    elif command -v google-chrome &> /dev/null; then
        echo -e "${BLUE}Using Google Chrome for diagram rendering${NC}"
        return 0
    elif command -v chrome &> /dev/null; then
        echo -e "${BLUE}Using Chrome for diagram rendering${NC}"
        return 0
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        # On macOS, check for Chrome in typical locations
        if [[ -f "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" ]]; then
            echo -e "${BLUE}Using system Chrome for diagram rendering${NC}"
            return 0
        elif [[ -f "/Applications/Chromium.app/Contents/MacOS/Chromium" ]]; then
            echo -e "${BLUE}Using system Chromium for diagram rendering${NC}"
            return 0
        else
            echo -e "${YELLOW}⚠️  No browser found. On macOS, please install Chrome or Chromium:${NC}"
            echo -e "${YELLOW}   brew install --cask google-chrome${NC}"
            echo -e "${YELLOW}   or${NC}"
            echo -e "${YELLOW}   brew install --cask chromium${NC}"
            return 1
        fi
    else
        echo -e "${RED}❌ No compatible browser found for mermaid-cli${NC}"
        return 1
    fi
}

# Configuration
TEMP_DIR=$(mktemp -d)
OUTPUT_DIR="./pdfs"
DOCS=("Litepaper.md" "Whitepaper.md")

# Cleanup function
cleanup() {
    rm -rf "$TEMP_DIR"
}
trap cleanup EXIT

echo -e "${BLUE}🔧 Binocular PDF Generator${NC}"
echo "Temp directory: $TEMP_DIR"

# Check browser availability
if ! check_browser; then
    exit 1
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Function to process Mermaid diagrams in a markdown file
process_mermaid() {
    local input_file="$1"
    local output_file="$2"
    
    echo -e "${YELLOW}  Copying markdown as-is (Mermaid diagrams will appear as code blocks)...${NC}"
    
    # Just copy the file as-is without processing Mermaid diagrams
    cp "$input_file" "$output_file"
}

# Function to generate PDF from processed markdown
generate_pdf() {
    local processed_md="$1"
    local output_pdf="$2"
    local title="$3"
    
    echo -e "${YELLOW}  Generating PDF with pandoc...${NC}"
    
    # Different options based on document type
    local toc_options=""
    if [[ "$title" != "Litepaper" ]]; then
        toc_options="--table-of-contents --toc-depth=3"
    fi
    
    pandoc "$processed_md" \
        --pdf-engine=xelatex \
        --from=markdown+pipe_tables+grid_tables \
        --to=pdf \
        --output="$output_pdf" \
        --variable=geometry:margin=1in \
        --variable=fontsize:11pt \
        --variable=colorlinks:true \
        --variable=linkcolor:blue \
        --variable=urlcolor:blue \
        --variable=toccolor:black \
        $toc_options \
        --number-sections \
        --highlight-style=tango \
        --metadata title="$title" \
        --metadata author="Alexander Nemish @ Lantr" \
        --metadata date="$(date '+%B %d, %Y')" || {
            echo -e "${RED}  ❌ Failed to generate PDF with pandoc${NC}"
            return 1
        }
}

# Main processing loop
for doc in "${DOCS[@]}"; do
    if [[ ! -f "$doc" ]]; then
        echo -e "${RED}❌ File not found: $doc${NC}"
        continue
    fi
    
    echo -e "${BLUE}📄 Processing: $doc${NC}"
    
    # Extract title from filename
    title=$(basename "$doc" .md)
    
    # Process the markdown file (handle mermaid diagrams)
    processed_md="$TEMP_DIR/${title}_processed.md"
    process_mermaid "$doc" "$processed_md"
    
    # Generate PDF
    output_pdf="$OUTPUT_DIR/${title}.pdf"
    if generate_pdf "$processed_md" "$output_pdf" "$title"; then
        echo -e "${GREEN}  ✅ Generated: $output_pdf${NC}"
    else
        echo -e "${RED}  ❌ Failed to generate: $output_pdf${NC}"
        continue
    fi
done

echo -e "${GREEN}🎉 PDF generation complete!${NC}"
echo -e "${BLUE}📁 Output directory: $OUTPUT_DIR${NC}"
ls -la "$OUTPUT_DIR"