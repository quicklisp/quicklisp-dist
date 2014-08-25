;;;; quicklisp-dist.asd

(asdf:defsystem #:quicklisp-dist
  :serial t
  :depends-on (#:zs3
               #:quicklisp
               #:release-report
               #:commando
               #:yason)
  :components ((:file "dist-index")
               (:file "update-report")))
