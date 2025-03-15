
(asdf:defsystem "quotes"
  :depends-on ("clingon" "sqlite")
  :components ((:file "quotes"))
  :build-operation "program-op"
  :build-pathname "quotes"
  :entry-point "quotes:main")
