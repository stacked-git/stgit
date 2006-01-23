DOCS = tutorial.txt
DOCS_HTML = $(DOCS:.txt=.html)
DOCS_PDF = $(DOCS:.txt=.pdf)

all: $(DOCS_HTML) $(DOCS_PDF)

%.html: %.txt
	asciidoc $<

%.xml: %.txt
	asciidoc --backend=docbook $<

%.pdf: %.xml
	xmlto pdf $<

clean:
	rm -f *.xml *.html *.pdf
