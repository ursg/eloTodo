(require :asdf)
(require :uiop)

;; Download & install Quicklisp in the script rather than the GitHub Actions
;; because it's easier; the downloaded file is already in our working directory.
(uiop:run-program (list "curl" "-O" "https://beta.quicklisp.org/quicklisp.lisp"))
(load "quicklisp.lisp")
(quicklisp-quickstart:install)

(push (concatenate 'string (namestring (uiop:getcwd)) "/") asdf:*central-registry*)

(ql:quickload :elotodo)
(asdf:make :elotodo)
