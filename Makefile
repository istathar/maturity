all: .target Placeholder.pdf

SOURCES= \
	Placeholder.markdown

.target:
	if [ ! -d /tmp/pandoc_andrew ] ; then mkdir -p /tmp/pandoc_andrew ; fi
	mktemp -d -p /tmp/pandoc_andrew pdflatex-XXXXX > .target


Placeholder.pdf: Placeholder.tex Example.svg Example.pgf
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


# stack path | grep "local-install-root" + /bin
BINDIR=.stack-work/install/x86_64-linux/lts-8.23/8.0.2/bin

${BINDIR}/render-svg: tests/RenderExampleSVG.hs tests/Example.hs
	stack build

${BINDIR}/render-pgf: tests/RenderExamplePGF.hs tests/Example.hs
	stack build

Example.svg: ${BINDIR}/render-svg
	${BINDIR}/render-svg -o $@ -w 400 

Example.pgf: ${BINDIR}/render-pgf
	${BINDIR}/render-pgf -o $@ -w 400 

clean:
	-rm -f Placeholder.tex
	-if [ -f .target -a -d `cat .target` ] ; then rm -r `cat .target` ; fi
	-rm -f .target
