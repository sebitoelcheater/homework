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
;;(1.0)
;Extienda la función run
(test (run '{{fun {x y z} {+ x y z}} 1 2 3}) 6)
(test (run '{with {{x 1} {y 2} {z 3}} {+ x y z}}) 6)
(test (run '{with {} {{fun {} 42}}}) 42)
(test (run '{local {{define x 1} 
                {define y 2}} 
           {+ x y}}) 3)
(test (run '{local {{datatype List 
                  {Empty} 
                  {Cons a b}}}
          {List? {Empty}}})
#t)
(test (run '{local {{datatype List 
                  {Empty} 
                  {Cons a b}}}
           {match {Cons 1 {Cons 2 {Empty}}}
             {case {Cons a b} => a}}})
1)
(test (run '{List? {Empty}}) #t)
(test (run '{List? {Cons 2 Empty}}) #t)
(test (run '{List? {Cons 3 4}}) #t)
;Implemente la función (make-list l)
(test (make-list (list 1 2 3 4)) '(Cons 1 (Cons 2 (Cons 3 (Cons 4 (Empty))))))
(test (make-list (list)) '(Empty))
(test (make-list (list 3)) '(Cons 3 (Empty)))
(test (make-list (list 1 2 3 4)) '(Cons 1 (Cons 2 (Cons 3 (Cons 4 (Empty))))))
;Extienda el lenguaje para soportar una sintaxis para crear listas
(test (run '{match {list 1 2}
          {case {Cons h r} => h}
          {case _ => 2}}) 1)
(test (run '{Cons? {list 1 2}}) #t)

;;(1.0) Agregue el keyword lazy
(test (run '{{fun {x  {lazy y}} x} 1 {/ 1 0}}) 1)
#;(test/exn (run '{{fun {x  y} x} 1 {/ 1 0}}) "/: division by zero")
(test (run '{local {{datatype Foo {Lazy {lazy a }}}
                {define x  {Lazy {/ 1 0}}}}
          {Foo? x}}) #t)
#;(test (run '{local {{datatype Foo {Lazy {lazy a}}}
                {define x  {Lazy {/ 1 0}}}}
          {match x
            {case {Lazy a} => a}}}) "/: division by zero")

;(0.6) Defina las funciones stream-hd y stream-tl
#;(run '{local {{datatype Stream
                            {Empty} 
                            {Cons hd {lazy tl}}}
                {define make-stream {fun {hd {lazy tl}}
                                         {Cons hd {lazy tl}}}}
                {define ones {make-stream 1 ones}}}
          {make-stream 1 {/ 1 0}}})

(test (run `{local {,stream-data ,make-stream ,stream-hd ,ones}
              {stream-hd ones}}) 1)
(test (run `{local {,stream-data ,make-stream
                             ,stream-hd ,stream-tl ,ones}
          {stream-hd {stream-tl ones}}}) 1)
(test (run `{local ,stream-lib
          {local {,ones ,stream-take}
            {stream-take 10 ones}}})'(1 1 1 1 1 1 1 1 1 1))
(test (run `{local ,stream-lib
          {local {,ones ,stream-take}
            {stream-take 3 {make-stream 1 {make-stream 2 {make-stream 3 {make-stream 1 4}}}}}}})'(1 2 3))
(test (run `{local ,stream-lib
          {local {,ones ,stream-take}
            {stream-take 0 {make-stream 1 {make-stream 2 2}}}}}) '())
(test (run `{local ,stream-lib
          {local {,ones ,stream-take}
            {stream-take 5 {make-stream 1 {make-stream 2 {StrEmpty}}}}}}) '(1 2))
(test (run `{local ,stream-lib
          {local {,ones ,stream-zipWith}
            {stream-take 10
                         {stream-zipWith
                          {fun {n m}
                               {+ n m}}
                          ones
                          ones}}}}) '(2 2 2 2 2 2 2 2 2 2))
(test (run `{local ,stream-lib
          {local {,ones ,stream-zipWith}
            {stream-take 3
                         {stream-zipWith
                          {fun {n m}
                               {+ n m}}
                          ones
                          {make-stream 3 ones}}}}}) '(4 2 2))