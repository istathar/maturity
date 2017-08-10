all: .target Placeholder.pdf

SOURCES= \
	TableByHand.latex \
	Placeholder.markdown

.target:
	if [ ! -d /tmp/pandoc_andrew ] ; then mkdir -p /tmp/pandoc_andrew ; fi
	mktemp -d -p /tmp/pandoc_andrew pdflatex-XXXXX > .target


Placeholder.pdf: Placeholder.tex
	latexmk \
		-pdf \
		-output-directory=`cat .target` \
		-interaction=nonstopmode \
		-halt-on-error \
		-file-line-error \
		-shell-escape \
		$<
	cp `cat .target`/$@ .


Placeholder.tex: ${SOURCES} engine/preamble.tex 
	-rm -f $@
	pandoc \
		--read=markdown \
		--write=latex \
		--verbose \
		--template engine/preamble.tex \
		--output $@ \
		${SOURCES}
	chmod -w $@


clean:
	-rm -f Placeholder.tex
	-if [ -f .target -a -d `cat .target` ] ; then rm -r `cat .target` ; fi
	-rm -f .target
