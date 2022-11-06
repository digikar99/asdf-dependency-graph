(defpackage #:asdf-dependency-graph
  (:use #:common-lisp)
  (:export #:generate
           #:*interesting-systems*))

(in-package #:asdf-dependency-graph)

(defstruct node parent child)

(defun name-from-system-description (system-description)
  (optima:match system-description
    ((list* :feature _ name _)
     name)
    ((list* :version name _)
     name)
    (_ system-description)))

(defun dependency-node-list (asdf-system)
  (declare (type asdf:system asdf-system))
  (let ((child-name (asdf:primary-system-name asdf-system))
        (dependencies (asdf:system-depends-on asdf-system)))
    (loop :for system-description :in dependencies
          :nconcing (let ((system-name (name-from-system-description system-description)))
                      (cons (make-node :parent system-name :child child-name)
                            (dependency-node-list (asdf:find-system system-name)))))))

(defvar *interesting-systems*)
(setf (documentation '*interesting-systems* 'variable)
      "If bound, the generated graph contains nodes corresponding only to the systems
mentioned in *INTERESTING-SYSTEMS*")
(declaim (type list *interesting-systems*))

(defun may-be-filter-for-interesting-systems (node-list)
  (if (not (boundp '*interesting-systems*))
      node-list
      (flet ((all-nodes-are-interesting-p (node-list)
               (loop :for node :in node-list
                     :always
                     (and (member (node-parent node)
                                  *interesting-systems* :test #'string=)
                          (member (node-child  node)
                                  *interesting-systems* :test #'string=)))))
        (let ((*interesting-systems*
                (loop :for system :in *interesting-systems*
                      :collect (etypecase system
                                 (asdf:system (asdf:primary-system-name system))
                                 (string system)))))
          (loop :until (all-nodes-are-interesting-p node-list)
                :do (setq node-list
                          (loop :for node :in node-list
                                :nconcing
                                (cond ((and (member (node-parent node)
                                                    *interesting-systems* :test #'string=)
                                            (member (node-child  node)
                                                    *interesting-systems* :test #'string=))
                                       (list node))
                                      ((member (node-child node) *interesting-systems* :test #'string=)
                                       (loop :for system-description
                                               :in (asdf:system-depends-on
                                                    (asdf:find-system
                                                     (node-parent node)))
                                             :for name
                                               := (name-from-system-description
                                                   system-description)
                                             :collect
                                             (make-node :parent name
                                                        :child (node-child node))))))))
          node-list))))

(defun generate (output-file target-system)
  (let* ((target-system (etypecase target-system
                          (string (asdf:find-system target-system))
                          (symbol (asdf:find-system target-system))
                          (asdf:system target-system)))
         (img-format (let ((suffix-start (position #\. output-file :from-end t)))
                       (if suffix-start
                           (subseq output-file (1+ suffix-start))
                           "png")))
         (node-list (may-be-filter-for-interesting-systems
                     (dependency-node-list
                      target-system))))
    (uiop:with-temporary-file (:type "gv" :pathname path)
      (with-open-file (f path :direction :output
                              :if-exists :supersede)
        (write-string "strict digraph {" f)
        (write-string "rankdir=LR;" f)
        (dolist (node node-list)
          (with-slots (parent child) node
            (format f "  \"~A\" -> \"~A\";" parent child)))
        (write-string "}" f))
      (uiop:run-program (format nil "dot -T~A '~A' > '~A'"
                                img-format
                                (namestring path)
                                output-file)
                        :error-output *error-output*
                        :output *standard-output*))))
