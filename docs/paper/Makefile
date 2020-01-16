
all: meta.yaml latex_template.tex introduction.md methods.md results.md discussion.md references.bib
	pandoc --template latex_template.tex meta.yaml -f markdown+simple_tables -t latex introduction.md methods.md results.md discussion.md -s -o paper.tex --bibliography references.bib --biblatex -N 
	pdflatex paper 
	biber paper
	pdflatex paper
	pdflatex paper

clean:
	-rm paper.tex paper.blg paper.aux paper.log paper.bbl paper.bcf
