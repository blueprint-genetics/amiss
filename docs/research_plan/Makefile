
all: meta.yaml latex_template.tex research_plan.md
	pandoc --template latex_template.tex meta.yaml -f markdown -t latex research_plan.md -s -o research_plan.tex --bibliography references.bib --biblatex -N
	pdflatex research_plan 
	biber research_plan
	pdflatex research_plan
	pdflatex research_plan

clean:
	-rm research_plan.tex research_plan.blg research_plan.aux research_plan.log research_plan.bbl research_plan.bcf
