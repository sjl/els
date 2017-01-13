(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :rcurry

               )
  :package "ELS.QUICKUTILS")
