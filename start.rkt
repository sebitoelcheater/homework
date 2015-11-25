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
  (lazy lazy-expr env)
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

;; interp :: Expr Env -> number/procedure
(defun (interp expr env)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(my-string s) s]
    [(my-if c t f)
     (if (interp c env)
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]
    
    [(fun ids body) (λ (arg-vals)       
                      (interp body
                              (multi-extend-env ids arg-vals env)))]
    
    [(app (fun ids fexpr) arg-expr-list) 
     ((interp (fun ids fexpr) env) (clean-sebito ids arg-expr-list env))]
    ;(map (λ (a) (if (contains? (get-lazys fun-expr) ))(begin (print fun-expr) (λ () (interp a env)))) arg-expr-list)
    
    [(prim-app prim arg-expr-list)
     (apply (cadr (assq prim *primitives*))
            (map (λ (a) (interp a env)) arg-expr-list))]
    
    [(lcal defs body)
     (def new-env (aEnv '() env))            
     (for-each (λ (d) (interp-def d new-env)) defs) 
     (interp body new-env)]
    
    [(lazy l-expr l-env) (interp l-expr l-env)]
    
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

(defun (interp-datatype name env)
  (update-env! env
               (string->symbol (string-append (symbol->string name) "?"))
               (λ (v)
                 (match (first v)
                   [(structV n var _) (symbol=? n name)]))))

(defun (interp-variant name var env)  
  ;; name of the variant or dataconstructor
  (def varname (variant-name var))
  
  ;; variant data constructor
  (update-env! env
               varname
               (λ (args)
                 (structV name varname args)))
  
  ;; variant predicate
  (update-env! env
               (string->symbol (string-append (symbol->string varname) "?"))
               (λ (v)
                 (match (first v)
                   [(structV _ var _) (symbol=? var varname)]))))

(defun (find-first-matching-case value cases)
  (match cases
    [(list) #f]
    [(cons (my-case pattern body) cs)
     (match (match-pattern-with-value pattern value)
       [#f (find-first-matching-case value cs)]
       [assocList (cons assocList body)])]))

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
                [(x y) (error "Match failure")]))

;; run :: s-expr -> number
(defun (run prog)
  (interp (parse {list 'local {list {list 'datatype 'List {list 'Empty} {list 'Cons 'a 'b}}} prog}) empty-env))

;; Para Sección 3
;(defun (run prog)  
;  (print-list (interp (parse prog) empty-env)))
;
;(defun (print-list e)
;  (match e
;    [(structV 'List Empty (list)) '()]
;    [(structV 'List Cons (list h t)) (cons h (print-list t))]
;    [else e]))



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
                 (match id
                   [(? symbol?) (multi-extend-env ids vals (extend-env id val env))]
                   [(list 'lazy id2) (multi-extend-env ids vals (extend-env id2 val env))])]))
;(begin (print val) (multi-extend-env ids vals (extend-env id val env)))

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
(define (make-list lst)
  (if (empty? lst)
      (list 'Empty)
      (list 'Cons (car lst) (make-list (cdr lst)))))

; get-lazys :: Expr<Fun> -> List<Expr>
(define (get-lazys expr-fun)
  (match expr-fun
    [(fun ids body) (foldl (λ (id lazys) (if (is-lazy? id)
                                            (cons (cadr id) lazys)
                                            lazys)) '() ids)]))

;is-lazy? :: Expr -> Boolean
(define (is-lazy? expr)
  (match expr
    [(? symbol?) #f]
    [(list 'lazy id) #t]
    [else (error "falsa alarma")]))

; contains? :: List, Val -> Boolean
; Verifica que en la lista entregada exista un elemento cuyo valor sea igual al entregado.
; De ser así retorna verdadero, de no ser así retorna falso.
(define (contains? lst val)
  (match lst
    [(list) #f]
    [(cons h t) (or (equal? val h) (contains? t val))]))

(define (clean-sebito ids arg-expr-list l-env)
  (if (or (empty? ids) (empty? arg-expr-list))
      '()
      (let
      ([id (car ids)]
       [arg (car arg-expr-list)])
    (if (is-lazy? id)
        (cons (lazy arg l-env) (clean-sebito (cdr ids) (cdr arg-expr-list) l-env))
        (cons arg (clean-sebito (cdr ids) (cdr arg-expr-list) l-env))))
      ))