(require :asdf)
(require :uiop)

(push (concatenate 'string (namestring (uiop:getcwd)) "/") asdf:*central-registry*)

(asdf:make :elotodo)
