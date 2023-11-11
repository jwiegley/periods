SBCL = sbcl
MODULES = ':PERIODS'

all: html info

docstrings: make-tempfiles.sh
	@mkdir docstrings
	SBCLRUNTIME=$(SBCL) MODULES=$(MODULES) ./make-tempfiles.sh
	perl -i -pe 's/periods://g' docstrings/*.texinfo
	perl -i -pe 's/^  // if /@lisp/ .. /@end lisp/' docstrings/*.texinfo
	perl -i -pe 's/=>/@result{}/ if /@lisp/ .. /@end lisp/' docstrings/*.texinfo
	perl -i -pe 's/.../@dots{}/g' docstrings/*.texinfo

info: periods.info

periods.info: periods.texi docstrings
	makeinfo periods.texi

# Nice sed: @ is a separator just like the usual / but helping us to not escape the real /
# Using https://github.com/orthecreedence/highlight-lisp/
html: periods.texi
	makeinfo --html --no-split periods.texi -o index.html
	sed -i "s@</body>@</body>  <script> HighlightLisp.highlight_auto(); HighlightLisp.paren_match(); </script>@g" index.html
	sed -i "s@</pre>@</code></pre>@g" index.html
	sed -i "s@<pre class=\"lisp\">@<pre><code class=\"lisp\">@g" index.html
	sed -i "s@<head>@<head> <script type=\"text/javascript\" src=\"https://lisp-maintainers.github.io/periods/static/highlight-lisp.js\"></script> <link rel=\"stylesheet\" href=\"https://lisp-maintainers.github.io/periods/static/github.css\">@g" index.html

# Works best on the gh-pages branch.
html-local:
	sed -i "s@https://lisp-maintainers.github.io/periods@@g" index.html

pdf: periods.texi docstrings
	texi2pdf periods.texi

clean:
	rm -f periods.info periods.pdf
	rm -fr periods docstrings
