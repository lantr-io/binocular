

# Process markdown to PDF using pandoc with mermaid filter
pandoc Whitepaper.md \
  --pdf-engine=xelatex \
  --from=markdown+tex_math_single_backslash \
  --highlight-style=tango \
  --filter "mm.sh" \
  -V geometry:margin=1in \
  -V colorlinks=true \
  -V linkcolor=blue \
  -V urlcolor=blue \
  -V toccolor=blue \
  -V mainfont="DejaVu Serif" \
  -V monofont="DejaVu Sans Mono" \
  -V fontsize=11pt \
  -o Whitepaper.pdf

pandoc Litepaper.md \
  --pdf-engine=xelatex \
  --from=markdown+tex_math_single_backslash \
  --highlight-style=tango \
  --filter "mm.sh" \
  -V geometry:margin=1in \
  -V colorlinks=true \
  -V linkcolor=blue \
  -V urlcolor=blue \
  -V toccolor=blue \
  -V mainfont="DejaVu Serif" \
  -V monofont="DejaVu Sans Mono" \
  -V fontsize=11pt \
  -o Litepaper.pdf
