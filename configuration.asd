
(asdf:defsystem :configuration
  :description "Handles configuration parameters for running systems"
  :author "VIP"
  :serial t
  :depends-on (#:vip-utils #:cl-ppcre #:vip-clim-core #:anaphors)
  :components ((:file "package")
               (:file "configuration")
               ))
