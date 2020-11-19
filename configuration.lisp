
(in-package :configuration)

;; It might be nice to make configuration parameters typed
;; This could be easily achieved with the clim accept-from-string

;; (although I think SMF might have a concept of typing anyway)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SMF STUFF

(defun smf-property (name)
  (let ((fmri (ccl:getenv "SMF_FMRI")))
    ;; we are running as an SMF service
    (when fmri
      (first (shell (format nil "svcprop -p '~A' '~A'" name fmri))))))


(defun smf-instance ()
  (when (ccl:getenv "SMF_FMRI")
    (regex-replace ".*\\:" (ccl:getenv "SMF_FMRI") "")))

;; (smf-instance)

(defvar *configuration-parameters* (make-hash-table))

;; only stored locally - does not attempt to persist
(defun (setf configuration-parameter) (value name)
  (setf (gethash name *configuration-parameters*) value))

;; (setf (configuration-parameter :foo) "asd")

(defun configuration-parameter (name &key (default
                                              (cond ((eq name :instance)
                                                     (or (smf-instance)
                                                         "TEST"))))
                                       (type 'string))
  (multiple-value-bind (cached present)
      (gethash name *configuration-parameters*)
    (let ((value (or cached
                     (and (not present)
                          (or (awhen (smf-property (string-downcase (symbol-name name)))
                                (accept-from-string type it))
                              #+ccl(awhen (ccl:getenv (regex-replace-all "/" (symbol-name name) "_"))
                                     (accept-from-string type it))))
                     default)))
      (unless present
        (setf (gethash name *configuration-parameters*)
              value))
      (unless (presentation-typep value type)
        (error "Expected configuration parameter ~A of type ~A but found ~A instead"
               name type value))
      value)))


;; (configuration-parameter :swank/port)
;; (configuration-parameter :swank/interface)
;; (configuration-parameter :http/port)
;; (configuration-parameter :mail/port)
;; (configuration-parameter :mail/host)


;; (configuration-parameter :swank/port :type 'integer :default 4005)
;; (configuration-parameter :swank/interface :type 'string)


(defmacro with-configuration ((&rest parameters) &body forms)
  (let ((outer (gensym "outer")))
    `(let ((,outer *configuration-parameters*)
           (*configuration-parameters* (make-hash-table)))
       ;; first copy the outer parameters
       (maphash (lambda (k v)
                  (setf (gethash k *configuration-parameters*) v))
                ,outer)
       ;; then apply new values
       (setf ,@ (loop for (a b) on parameters by #'cddr
                      collect `(gethash ,a *configuration-parameters*)
                      collect b))
       ,@forms)))

