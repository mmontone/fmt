(in-package :fmt-test)

(defun run-tests ()
  (run! 'fmt-tests))

(def-suite fmt-tests)

(in-suite fmt-tests)

(test basic-test
  (is (equalp (fmt nil "Hello" #\space "world")
	      "Hello world"))
  (is (equalp (fmt nil "Hello" #\newline
		   "world")
	      "Hello
world")))


(test aesthetic-test
  (is (equalp (fmt nil (:a "foo"))
	      "foo"))
  (is (equalp (fmt nil (:a 22))
	      "22"))
  (is (equalp (fmt nil (:aesthetic "foo"))
	      "foo"))
  (is (equalp (fmt nil (:a (list :a :b :c)))
	      "(A B C)")))

(test standard-test
  (is (equalp (fmt nil (:s "foo"))
	      "\"foo\""))
  (is (equalp (fmt nil (:s (list :a :b :c)))
	      "(:A :B :C)")))

(test when-test
  (let ((cond t))
    (is (equalp (fmt nil (:when cond "yes")) "yes")))
  (let ((cond nil))
    (is (equalp (fmt nil (:when cond "yes")) ""))))

(test join-test
  (is (equalp (fmt nil (:join ", " (list "foo" "bar" "baz")))
	      "foo, bar, baz"))
  (is (equalp (fmt nil (:join #\, (list "foo" "bar")))
	      "foo,bar"))
  (is (equalp (fmt nil (:join "," (list "foo")))
	      "foo"))
  (is (equalp (fmt nil (:join "," (list)))
	      ""))
  (is (equalp (fmt nil (:join #\space '(a b c)))
	      "A B C")))

(test do-test
  (is (equalp (fmt nil (:do (item (list 1 2 3))
			    (:s item)))
	      "123")))

(test filter-test
  (is (equalp (fmt nil (:a "lala" :upcase))
	      "LALA"))
  (is (equalp (fmt nil (:s "lala" :up))
	      "\"LALA\"")))

(test escape-test
  (is (equalp (fmt nil
		   "hello"
		   (:esc #\space "beautiful")
		   #\space "world")
	      "hello world"))
  (is (equalp (fmt* nil
		    "hello"
		    `(:esc #\space "beautiful")
		    #\space
		    "world")
	      "hello world")))		   

(test control-flow-test
  (is (equalp (fmt nil 
		   (:a "start")
		   #\newline
		   (:esc 
		    (loop for x in (list 1 2 3)
			do (emb (:s x))))
		   #\newline
		   (:a "end"))
	      "start
123
end")))

(test special-argument-test 
  (is (equalp (fmt nil (:join ", " (list "a" "b" "c") (:s _)))
	      "\"a\", \"b\", \"c\""))
  (is (equalp (fmt nil (:join ", " (list "a" "b" "c") (:a _ :up)))
	      "A, B, C")))
