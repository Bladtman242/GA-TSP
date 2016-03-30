.DEFAULT_GOAL:= Main.pdf

Main.tex: Main.lhs
	./.cabal-sandbox/bin/lhs2TeX $< > $@

Main.pdf: Main.tex
	pdflatex Main.tex
