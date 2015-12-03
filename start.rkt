#lang play

#|
<expr> ::= <num>
         | <bool>
         | <id>
         | <string>
         | {if <expr> <expr> <expr>}
         | {with {{<id> <expr>}+ } <expr>}
         | {fun {<id>*}}  <expr>}
         | {<expr> <expr>*}
         | {local {<def>*} <expr>}         
         | {match <expr> <case>+}

<case> ::= {'case <pattern> '=> <expr>}
<pattern> ::= <num>
         | <bool>
         | <string>
         | <id>
         | (<constr-id> <attr-id>*)

<def>  ::= {define <id> <expr>}
         | {datatype <typename> <type-constructor>*}}


<type-constructor> ::= {<id> <member>*}
<constr-id> :: = <id>
<attr-id> :: = <id>
<typename> :: = <id>
<member>   :: = <id>

|#



(deftype Expr
  (num n)
  (bool b)
  (my-string s)
  (my-if c t f)
  (id s)
  (app fun-expr arg-expr-list)
  ;nodo para aplicación de primitivas
  (prim-app name args)  
  (fun id body)
  (lcal defs body)
  (datatype name variants)
  (my-match val cases)
  )

;AST for {define <id> <expr>}
(deftype Def
  (mydef name val-expr))

(deftype Variant
  (variant name params))

(deftype Val
  (structV name variant values))

(deftype Case
  (my-case pattern body))

(deftype Pattern
  (nilP)
  (idP id)
  (litP l)
  (constrP ctr patterns))

;; parse :: s-expr -> Expr
; realiza el parse se una SExpr a una Expr
(defun (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? boolean?) (bool s-expr)]
    [(? string?) (my-string s-expr)]
    [(? symbol?) (id s-expr)]    
    [(list 'if c t f) (my-if (parse c)
                          (parse t)
                          (parse f))]
    [(list 'fun (list x ...) b) (fun x (parse b))]    
    [(list 'with (list (list x e) ...) b)
     (app (fun x (parse b)) (map parse e))]
    [(list 'local (list defs ...) body)
     (lcal (map parse-def defs) (parse body))]    
    [(list 'list a ...) (parse (make-list a))]
    [(list 'match val-expr cases ...)
     (my-match (parse val-expr) (map parse-case cases))]
    [(list f a ...)
     (if (assq f *primitives*)     
         (prim-app f (map parse a))
         (app (parse f) (map parse a)))]))

;; parse-def:: s-expr -> mydef/datatype
(defun (parse-def s-expr)  
  (match s-expr
    [(list 'define id val-expr) (mydef id (parse val-expr))]
    [(list 'datatype name variants ...) (datatype name (map parse-variant variants))]))

;; parse-variante :: s-expr -> Variant
(defun (parse-variant v)
  (match v
    [(list name params ...) (variant name params)]))

;; parse-case :: s-expr -> Expr(my-case)
(defun (parse-case c)
  (match c
    [(list 'case pattern => body) (my-case (parse-pattern pattern) (parse body))]))

;; parse-pattern :: s-expr -> Pattern
(defun (parse-pattern p)  
  (match p
    [(? symbol?)  (idP p)]
    [(? number?)  (litP (num p))]
    [(? boolean?) (litP (bool p))]        
    [(list ctr patterns ...) (constrP (first p) (map parse-pattern patterns))]))

;MODIFICADA
;; interp :: Expr Env -> number/procedure
; es el intérprete
(defun (interp expr env)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(my-string s) s]
    [(my-if c t f)
     (if (interp c env)
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env) #;(match (env-lookup x env)
              [(? procedure?) ((env-lookup x env))]
              [else else])]
    
    [(fun ids body) (λ (arg-vals)       
                      (interp body
                              (multi-extend-env
                               (map (λ (id) (match id
                                                 [(? symbol?) id]
                                                 [(list 'lazy l-id) l-id])) ids)
                               (map (λ (id arg) (match id
                                                  [(? symbol?) (arg)]
                                                  [(list 'lazy l-id) arg])) ids arg-vals)
                               env)))]
    
    [(app fun-expr arg-expr-list)
     ((interp fun-expr env) (map (λ (a) (λ () (interp a env))) arg-expr-list))]
    
    [(prim-app prim arg-expr-list)
     (apply (cadr (assq prim *primitives*))
            (map (λ (a) (interp a env)) arg-expr-list))]
    
    [(lcal defs body)
     (def new-env (aEnv '() env))            
     (for-each (λ (d) (interp-def d new-env)) defs) 
     (interp body new-env)]
    
    [(my-match expr cases)
     (def value-matched (interp expr env))
     (def result (find-first-matching-case value-matched cases))
     (match result
       [#f (error "Error")]
       [(cons assocList body)        
        (define new-env (multi-extend-env (map car assocList)
                                          (map cdr assocList)
                                          env))
        (interp body new-env)])]
    ))

(defun (interp-def d env)
  (match d
    [(mydef id val-expr)
     (update-env! env id (interp val-expr env))]
    [(datatype name variants)
     ;; extend environment with new definitions corresponding to the datatype
     ;; it has no return value          
     (interp-datatype name env)
     (for-each (λ (v) (interp-variant name v env)) variants)]))

;MODIFICADA
; interp-datatype :: Symbol Env -> Procedure
(defun (interp-datatype name env)
  (update-env! env
               (string->symbol (string-append (symbol->string name) "?"))
               (λ (v)
                 (match (first v)
                   [(? procedure?) (match ((first v))
                                     [(structV n var _) (symbol=? n name)])]
                   [(structV n var _) (symbol=? n name)]))))

;MODIFICADA
; interp-variant :: Symbol Variant Env -> Procedure
(defun (interp-variant name var env)  
  ;; name of the variant or dataconstructor
  (def varname (variant-name var))
  
  ;; variant data constructor
  (update-env! env
               varname
               (λ (args)
                 (structV name varname (map (λ (id arg) (match id
                                                          [(? symbol?) (arg)]
                                                          [else arg])) (variant-params var) args))))
  
  ;; variant predicate
  (update-env! env
               (string->symbol (string-append (symbol->string varname) "?"))
               (λ (v)
                 (match (first v)
                   [(? procedure?) (match ((first v))
                                     [(structV _ var _) (symbol=? var varname)])]
                   [(structV _ var _) (symbol=? var varname)]))))

(defun (find-first-matching-case value cases)
  (match cases
    [(list) #f]
    [(cons (my-case pattern body) cs)
     (match (match-pattern-with-value pattern value)
       [#f (find-first-matching-case value cs)]
       [assocList (cons assocList body)])]))

;MODIFICADA
; match-pattern-with-value :: Pattern Value -> Expr
(defun (match-pattern-with-value pattern value)
  (match/values (values pattern value)
                [((idP i) v) (list (cons i v))]
                [((litP (bool v)) b)
                 (if (equal? v b) (list) #f)]
                [((litP (num v)) n)
                 (if (equal? v n) (list) #f)]
                [((constrP ctr patterns) (structV _ ctr-name str-values))
                 (if (symbol=? ctr ctr-name)
                     (apply append (map match-pattern-with-value
                                        patterns str-values))
                     #f)]
                [(x y) (match y
                         [(? procedure?) (match-pattern-with-value x (y))]
                         [else (error "Match failure")])]))

;; run :: s-expr -> number
; evalué la expresión en un ambiente donde se tiene la definición de Lista de los ejemplos anteriores.
#;(defun (old-run prog)
  (interp-lazy
   (interp (parse {list 'local {list {list 'datatype 'List {list 'Empty} {list 'Cons 'a 'b}}} prog}) empty-env)))

; interp-lazy :: Expr -> Expr
; realiza la interpretación para expresiones que vienen como procedures producto de lazyness.
(defun (interp-lazy lazy-expr)
  (match lazy-expr
    [(? procedure?) (interp-lazy (lazy-expr))]
    [else lazy-expr]))

; Para Sección 3
;; run :: s-expr -> number
; evalué la expresión en un ambiente donde se tiene la definición de Lista de los ejemplos anteriores.
(defun (run prog)  
  (print-list (interp-lazy
   (interp (parse {list 'local {list {list 'datatype 'List {list 'Empty} {list 'Cons 'a 'b}}} prog}) empty-env))))

; print-list Expr -> List
; printea la expresión
(defun (print-list e)
  (match e
    [(structV 'List Empty (list)) '()]
    [(structV 'List Cons (list h t)) (cons h (print-list t))]
    [else e]))



#|-----------------------------
Environment abstract data type
(updated with recursive case)
 
empty-env   :: Env
env-lookup  :: Sym Env -> Val
extend-env  :: Sym Val Env -> Env
update-env! :: 
|#
(deftype Env
  (mtEnv)
  (aEnv bindings rest))

(def empty-env  (mtEnv))

(defun (extend-env id val env)
  (match env
    [(mtEnv) (aEnv (list (cons id val)) empty-env)]
    [(aEnv bindings rest) (aEnv (cons (cons id val) bindings) rest)]))

(defun (env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "no binding for identifier: ~a" id)]
    [(aEnv bindings rest)
     (def binding (assoc id bindings))
     (if binding
         (cdr binding)
         (env-lookup id rest))]))

(defun (multi-extend-env ids vals env)  
  (match/values (values ids vals)
                [((list) _) env]
                [(_ (list)) env]
                [((cons id ids) (cons val vals))
                 (multi-extend-env ids vals (extend-env id val env))]))

;; update-env! :: Env Sym Val -> Void
;; imperative update of env, adding/overring the binding for id.
(defun (update-env! env id val)
  (set-aEnv-bindings! env (cons (cons id val) (aEnv-bindings env))))

;;;;;;;

;;; primitives
(define *primitives*
  `((+       ,(lambda args (apply + args)))
    (-       ,(lambda args (apply - args)))
    (*       ,(lambda args (apply * args)))
    (%       ,(lambda args (apply modulo args)))             
    (odd?    ,(lambda args (apply odd? args)))
    (even?   ,(lambda args (apply even? args)))
    (/       ,(lambda args (apply / args)))
    (=       ,(lambda args (apply = args)))
    (<       ,(lambda args (apply < args)))
    (<=      ,(lambda args (apply <= args)))
    (>       ,(lambda args (apply > args)))
    (>=      ,(lambda args (apply >= args)))
    (zero?   ,(lambda args (apply zero? args)))
    (not     ,(lambda args (apply not args)))
    (and     ,(lambda args (apply (lambda (x y) (and x y)) args)))
    (or      ,(lambda args (apply (lambda (x y) (or x y)) args)))))

; make-list :: List<Int> -> Expr
; dada una lista de enteros de Scheme retorna una expresión de MiniScheme+ que representa la lista dada, usando la definición del datatype List
(define (make-list lst)
  (if (empty? lst)
      (list 'Empty)
      (list 'Cons (car lst) (make-list (cdr lst)))))
; Estructura Stream que evite evaluar su cola a menos que sea estrictamente necesario.
#;(def stream-data '{datatype Stream
                            {Empty} 
                            {SCons h {lazy t}}})

#;(def make-stream '{define make-stream {fun {d {lazy l}}
                                         {SCons d {lazy l}}}})

; stream-data
(def stream-data '{datatype Stream
                            {StrCons hd {lazy tl}}
                            {StrEmpty}})
(def make-stream '{define make-stream  {fun {hd
                                            {lazy tl}}
                                            {StrCons hd tl}}})

; ones
(def ones '{define ones {make-stream 1 ones}})

; stream-hd
; para obtener la cabeza de un stream
(def stream-hd '{define stream-hd {fun {str}
                                       {match str
                                         [case {StrCons h t} => h]}}})

; stream-tl
; para obtener la cola de un stream
(def stream-tl '{define stream-tl {fun {str}
                                       {match str
                                         [case {StrCons h t} => t]}}})

; stream-take
; retorna una lista con los primeros n elementos de stream. 
(def stream-take '{define stream-take {fun {n str}
                                           {match str
                                             [case {StrCons h t}
                                               => {if {> n 0}
                                                      {Cons h {stream-take {- n 1} t}}
                                                      {Empty}}]
                                             [case {StrEmpty}
                                               => {Empty}]}}})
; stream-lib
; dada en el enunciado
(def stream-lib (list stream-data
                        make-stream
                        stream-hd
                        stream-tl
                        stream-take))

(def stream-zipWith '{define stream-zipWith {fun {fZip str1 str2}
                                                 {match str1
                                                   [case {StrCons h1 t1} => 
                                                     {match str2
                                                       [case {StrCons h2 t2} =>
                                                         {make-stream {fZip h1 h2} {stream-zipWith fZip t1 t2}}]}]}}})
(def fibs '{define fibs {make-stream 1 ones}})
(def fibs '{define fibs {fun {fZip str1 str2}
                             {match str1
                               [case {StrCons h1 t1} => 
                                 {match str2
                                   [case {StrCons h2 t2} =>
                                     {make-stream {fZip h1 h2} {stream-zipWith fZip t1 t2}}]}]}}})