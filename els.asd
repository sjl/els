(asdf:defsystem :els
  :description "The Exciting Lisp Solver!"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:iterate
               :cl-losh
               :cl-arrows
               :cl-ggp
               :temperance)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "exciting-lisp-solver")))))
