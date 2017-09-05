all: .target EngineeringComponent.pdf

SOURCES= \
	EngineeringComponent.markdown

IMAGES= \
	ModelDescription.pdf
#
# stack path | grep "local-install-root" + /bin
BINDIR=.stack-work/install/x86_64-linux/lts-8.23/8.0.2/bin


.target:
	if [ ! -d /tmp/pandoc_andrew ] ; then mkdir -p /tmp/pandoc_andrew ; fi
	mktemp -d -p /tmp/pandoc_andrew pdflatex-XXXXX > .target


EngineeringComponent.pdf: EngineeringComponent.tex ${IMAGES}
	latexmk \
		-pdf \
		-output-directory=`cat .target` \
		-interaction=nonstopmode \
		-halt-on-error \
		-file-line-error \
		-shell-escape \
		$<
	cp `cat .target`/$@ .


EngineeringComponent.tex: engine/preamble.tex ${SOURCES}
	-rm -f $@
	pandoc \
		--read=markdown \
		--write=latex \
		--verbose \
		--template engine/preamble.tex \
		--output $@ \
		${SOURCES}
	chmod -w $@

ModelDescription.pdf: ${BINDIR}/render-model
	${BINDIR}/render-model -o $@ -w 400 

#
# Why does it seem so hard to get stack to just build the executable we desire?
#

${BINDIR}/render-model: src/RenderModelDescription.hs
	stack build

#
# Leftovers from testing and development


${BINDIR}/render-svg: tests/RenderExampleSVG.hs tests/Example.hs
	stack build

${BINDIR}/render-pgf: tests/RenderExamplePGF.hs tests/Example.hs
	stack build

Example.svg: ${BINDIR}/render-svg
	${BINDIR}/render-svg -o $@ -w 400 

Example.pgf: ${BINDIR}/render-pgf
	${BINDIR}/render-pgf -o $@ -w 400 

clean:
	-rm -f EngineeringComponent.tex
	-if [ -f .target -a -d `cat .target` ] ; then rm -r `cat .target` ; fi
	-rm -f .target
