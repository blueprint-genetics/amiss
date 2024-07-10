
all: meta.yaml latex_template.tex introduction.md methods.md results.md discussion.md references.bib supplementary.md
	pandoc --template latex_template.tex meta.yaml -f markdown+multiline_tables+table_captions -t latex introduction.md methods.md results.md discussion.md misc.md -s -o paper.tex --bibliography references.bib --biblatex -N 
	pdflatex paper 
	biber paper
	pdflatex paper
	pdflatex paper
	pandoc --template latex_template.tex meta.yaml -f markdown+multiline_tables+table_captions -t latex supplementary.md -s -o supplementary.tex
	pdflatex supplementary
	pdflatex supplementary

clean:
	-rm paper.tex paper.blg paper.aux paper.log paper.bbl paper.bcf supplementary.tex supplementary.aux supplementary.log
