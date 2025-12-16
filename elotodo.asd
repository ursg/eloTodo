(asdf:defsystem #:elotodo
  :description "A TUI-based todo list that ranks items using the elo system"
  :author "Urs Ganse <urs.ganse@helsinki.fi>"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("infix-math" "uiop" "com.inuoe.jzon" "serapeum" "cl-tui")
  :components ((:file "package")
               (:file "eloTodo"))
  :build-operation "program-op"
  :build-pathname "eloTodo"
  :entry-point "elotodo:main")
