#|
  This file is a part of nesper project.
  Copyright (c) 2015 Hiroyuki Tanaka (tanakahx@gmail.com)
|#

#|
  Author: Hiroyuki Tanaka (tanakahx@gmail.com)
|#

(in-package :cl-user)
(defpackage nesper-asd
  (:use :cl :asdf))
(in-package :nesper-asd)

(defsystem nesper
  :version "0.1"
  :author "Hiroyuki Tanaka"
  :license "MIT"
  :depends-on (:split-sequence)
  :components ((:module "src"
                :components
                ((:file "nesper"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op nesper-test))))
