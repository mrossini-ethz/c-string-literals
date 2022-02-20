(defsystem "c-string-literals"
  :description "Enables string literals like in the c programming language."
  :version "0.1.0"
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on (:parseq)
  :serial t
  :components ((:file "package")
               (:file "c-string-literals"))
  :in-order-to ((test-op (test-op :c-string-literals/test))))

(defsystem "c-string-literals/test"
  :description "Unit testing for c-string-literals."
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on (:c-string-literals :fiveam)
  :serial t
  :components ((:file "test/package")
               (:file "test/test")))

(defmethod perform ((operation test-op) (system (eql (find-system :c-string-literals/test))))
  (funcall (intern "C-STRING-LITERALS-TEST" :c-string-literals/test)))
