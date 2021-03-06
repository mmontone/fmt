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
world"))
  (is (equalp (fmt* nil "Hello" #\space "world")
              "Hello world"))
  (is (equalp (fmt* nil "Hello" #\newline
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
              "(:A :B :C)"))
  (is (equalp (fmt* nil `(:s ,"foo"))
              "\"foo\""))
  (is (equalp (fmt* nil `(:s ,(list :a :b :c)))
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
              "A B C"))
  (is (equalp (fmt nil (:join (", " " and ")
                              (list "foo" "bar" "baz")))
              "foo, bar and baz"))
  (is (equalp (fmt* nil `(:join ", " ,(list "foo" "bar" "baz")))
              "foo, bar, baz"))
  (is (equalp (fmt* nil `(:join #\, ,(list "foo" "bar")))
              "foo,bar"))
  (is (equalp (fmt* nil `(:join "," ,(list "foo")))
              "foo"))
  (is (equalp (fmt* nil `(:join "," ,(list)))
              ""))
  (is (equalp (fmt* nil `(:join #\space ,'(a b c)))
              "A B C"))
  (is (equalp (fmt* nil `(:join (", " " and ")
                                ,(list "foo" "bar" "baz")))
              "foo, bar and baz")))

(test do-test
  (is (equalp (fmt nil (:do (item (list 1 2 3))
                            (:s item)))
              "123")))

(test filter-test
  (is (equalp (fmt nil (:a "lala" :upcase))
              "LALA"))
  (is (equalp (fmt nil (:s "lala" :up))
              "\"LALA\""))
  (is (equalp (fmt nil (:s "lala" #'string-upcase))
              "\"LALA\""))
  (is (equalp (fmt* nil `(:a "lala" :upcase))
              "LALA"))
  (is (equalp (fmt* nil `(:s "lala" :up))
              "\"LALA\""))
  (is (equalp (fmt* nil `(:a "  lala  " (:trim #\ )))
              "lala"))
  (is (equalp (fmt* nil `(:a "lala" ,#'string-upcase))
              "LALA"))
  (is (equalp (fmt* nil `(:a "lala" #'string-upcase))
              "LALA")))

(test default-filter-test
  (is (equalp (fmt nil (:a "foo" (:default "bar")))
              "foo"))
  (is (equalp (fmt nil (:a nil (:default "bar")))
              "bar")))

(test truncate-filter-test
  (is (equalp (fmt nil (:a "lalalala" (:truncate 10)))
              "lalalala"))
  (is (equalp (fmt nil (:a "lalalala" (:truncate 4)))
              "lala..."))
  (is (equalp (fmt nil (:a "lalalala" (:truncate 6 ", etc")))
              "lalala, etc")))

(test replace-filter-test
  (is (equalp (fmt nil (:a "foobar" (:replace "foo" "bar")))
              "barbar"))
  (is (equalp (fmt nil (:a "foobar" (:replace "bar" "foo")))
              "foofoo"))
  (is (equalp (fmt nil (:a "foobar" (:replace "baz" "foo")))
              "foobar")))

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
                       do (:fmt (:s x))))
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

(test trim-filter-test
  (is (equalp (fmt nil (:a "  hello  " :trim))
              "hello"))
  (is (equalp (fmt nil (:a "  hello  " (:trim #\ )))
              "hello"))
  (is (equalp (fmt nil (:a "//hello" (:trim #\/)))
              "hello")))

(test radix-operation-test
  (is (equalp (fmt nil (:r 4))
              "four"))
  (is (equalp (fmt nil (:r 4 :cardinal))
              "four"))
  (is (equalp (fmt nil (:r 4 :ordinal))
              "fourth"))
  (is (equalp (fmt nil (:r 4 2))
              "100"))
  (is (equalp (fmt nil (:r 4 :roman))
              "IV"))
  (is (equalp (fmt nil (:r 4 :old-roman))
              "IIII")))

(test decimal-operation-test
  (is (equalp (fmt nil (:d 23))
              "23"))
  (is (equalp (fmt* nil `(:d ,23))
              "23")))

(test binary-operation-test
  (is (equalp (fmt nil (:b 5))
              "101"))
  (is (equalp (fmt* nil `(:b 5))
              "101")))

(test octal-operation-test
  (is (equalp (fmt nil (:o 20))
              "24"))
  (is (equalp (fmt* nil `(:o 20))
              "24")))

(test hexadecimal-operation-test
  (is (equalp (fmt nil (:x 20))
              "14"))
  (is (equalp (fmt* nil `(:x 20))
              "14")))

(test float-operation-test
  (is (equalp (fmt nil (:f 23.1234))
              "23.1234"))
  (is (equalp (fmt nil (:f 23.1234 :width 5))
              "23.12"))
  (is (equalp (fmt nil (:f 23.1234 :width 4 :digits 1))
              "23.1")))

(test if-operation-test
  (is (equalp (fmt nil (:if t
                            (:a "yes")))
              "yes"))
  (is (equalp (fmt nil (:if t
                            (:a "yes")
                            :else
                            (:a "no")))
              "yes"))
  (is (equalp (fmt nil (:if nil
                            (:a "yes")
                            :else
                            (:a "no")))
              "no"))
  (let ((list (list 1 2 3)))
    (is (equalp (fmt nil (:if (not list)
                              "none"
                              :else
                              (:join "," list)))
                "1,2,3")))
  (let ((list (list)))
    (is (equalp (fmt nil (:if (not list)
                              "none"
                              :else
                              (:join "," list)))
                "none"))))

(test times-test
  (is (equalp (fmt nil (:times #\space 5))
              "     "))
  (is (equalp (fmt* nil `(:times #\a 6))
              "aaaaaa")))

(test cl-format-test
  (let ((list (list "foo" "bar" "baz")))
    (is (equalp (fmt nil (:format "~{~A~^, ~}" list))
                (fmt nil (:join ", " list)))))
  (let ((list (list :foo :bar :baz)))
    (is (equalp (fmt nil (:format "~{~S~^, ~}" list))
                (fmt nil (:join ", " list (:s _)))))))
