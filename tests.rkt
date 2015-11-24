#lang play
(require "start.rkt")

;basic tests 
  (test (run '{+ 1 1}) 2)
  
  (test (run '{{fun {x y z} {+ x y z}} 1 2 3}) 6)  
  
  (test (run '{with {{x 1} {y 2} {f {fun {x y z}
                                         {+ x y x y}}}}
                    {f x y x}})
        6)
  
  (test (run '{< 1 2}) #t)
  
  (test (run '{local {{define x 1}} x}) 1)
  
  (test (run '{with {{x 2} {y {local {{define x 1}} x}}} {+ x x}}) 4)
  
  ;; datatypes  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {List? {Empty}}}) #t)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Empty}}}) #t)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {List? {Cons 1 2}}}) #t)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Cons? {Cons 1 2}}}) #t)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Cons 1 2}}})
        #f)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Empty}}}) #t)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Cons? {Empty}}})
        #f)      
  
  ;; match
  (test (run '{match 1 {case 1 => 2}}) 2)
  
  (test (run '{match 2
                {case 1 => 2}
                {case 2 => 3}})             
        3)
  
  (test (run '{match #t {case #t => 2}}) 2)
  
  (test (run '{match #f
                {case #t => 2}
                {case #f => 3}})             
        3)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}
                      {define length
                        {fun {l}
                             {match l
                               {case {Empty}    => 0}
                               {case {Cons h t} => {+ 1 {length t}}}}}}}
                {length {Cons 1 {Cons 2 {Cons 3 {Empty}}}}}})
        3)

; MIS TESTS
;(1.0)
;Extienda la función run
(test (run '{List? {Empty}}) #t)
;Implemente la función (make-list l)
(test (make-list (list 1 2 3 4)) '(Cons 1 (Cons 2 (Cons 3 (Cons 4 (Empty))))))
;Extienda el lenguaje para soportar una sintaxis para crear listas
(test (run '{match {list 1 2}
          {case {Cons h r} => h}
          {case _ => 2}}) 1)
(test (run '{Cons? {list 1 2}}) #t)