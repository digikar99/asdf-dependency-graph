(defpackage #:asdf-dependency-graph
  (:use #:common-lisp)
  (:export #:generate))

(in-package #:asdf-dependency-graph)

(defstruct node parent child)

(defun dependency-node-list (asdf-system)
  (declare (type asdf:system asdf-system))
  (let ((child-name (asdf:primary-system-name asdf-system))
        (dependencies (asdf:system-depends-on asdf-system)))
    (loop :for system-description :in dependencies
          :nconcing (let ((system-name (optima:match system-description
                                         ((list* :feature _ name _)
                                          name)
                                         ((list* :version name _)
                                          name)
                                         (_ system-description))))
                      (cons (make-node :parent system-name :child child-name)
                            (dependency-node-list (asdf:find-system system-name)))))))

(defun generate (output-file target-system)
  (let ((target-system (etypecase target-system
                         (string (asdf:find-system target-system))
                         (symbol (asdf:find-system target-system))
                         (asdf:system target-system)))
        (img-format (let ((suffix-start (position #\. output-file :from-end t)))
                      (if suffix-start
                          (subseq output-file (1+ suffix-start))
                          "png"))))
    (uiop:with-temporary-file (:type "gv" :pathname path)
      (with-open-file (f path :direction :output
                              :if-exists :supersede)
        (write-string "strict digraph {" f)
        (write-string "rankdir=LR;" f)
        (dolist (node (dependency-node-list target-system))
          (with-slots (parent child) node
            (format f "  \"~A\" -> \"~A\";" parent child)))
        (write-string "}" f))
      (uiop:run-program (format nil "dot -T~A '~A' > '~A'"
                                img-format
                                (namestring path)
                                output-file)
                        :error-output *error-output*
                        :output *standard-output*))))
