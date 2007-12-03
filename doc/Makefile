SBCL = sbcl
MODULES = ':PERIODS'

all: html info

docstrings: make-tempfiles.sh
	@mkdir docstrings
	SBCLRUNTIME=$(SBCL) MODULES=$(MODULES) ./make-tempfiles.sh
	perl -i -pe 's/periods://g' docstrings/*.texinfo
	perl -i -pe 's/^  // if /@lisp/ .. /@end lisp/' docstrings/*.texinfo

info: periods.info

periods.info: periods.texi docstrings
	makeinfo periods.texi

html: periods.texi docstrings
	makeinfo --html periods.texi

clean:
	rm -f periods.info periods.pdf
	rm -fr periods docstrings
