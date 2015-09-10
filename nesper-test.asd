#|
  This file is a part of nesper project.
  Copyright (c) 2015 Hiroyuki Tanaka (tanakahx@gmail.com)
|#

(in-package :cl-user)
(defpackage nesper-test-asd
  (:use :cl :asdf))
(in-package :nesper-test-asd)

(defsystem nesper-test
  :author "Hiroyuki Tanaka"
  :license "MIT"
  :depends-on (:nesper
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "nesper"))))
  :description "Test system for nesper"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
