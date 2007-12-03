#!/bin/sh

# Create Texinfo snippets from the documentation of exported symbols.

SBCL="$SBCLRUNTIME --noinform --noprint --disable-debugger"
SBCL="$SBCL --load /Users/johnw/Library/Lisp/init.lisp"
SBCL="$SBCL --load /Users/johnw/Library/Lisp/bootstrap.lisp"

# Output directory.  This has to end with a slash (it's interpreted by Lisp's
# `pathname' function) or you lose.  This is normally set from Makefile.
DOCSTRINGDIR="${DOCSTRINGDIR:-docstrings/}"

$SBCL <<EOF
(load "docstrings.lisp")
(dolist (module (quote ($MODULES)))
  (asdf:oos 'asdf:load-op module))
(sb-texinfo:generate-includes "$DOCSTRINGDIR" $MODULES)
(sb-ext:quit))
EOF
