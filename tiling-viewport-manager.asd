;;;; viewport-manager.asd

(asdf:defsystem #:tiling-viewport-manager
  :description "Little tiling window manager style thing for viewports"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :depends-on (:cepl)
  :serial t
  :components ((:file "package")
               (:file "tvm")))
