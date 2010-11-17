#!r6rs

;; assumptions:
;; - in (flip p), p is a fixed number
;; - all functions are curried
;; - all variables are bound

(import (rnrs)
        (only (srfi :1) delete-duplicates)
        (only (ikarus) pretty-print gensym)
        (only (rnrs r5rs) delay force)
        (only (xitomatl curry) define/curry)
        (only (xitomatl control) compose)
        (only (xitomatl match) match-lambda))

(define-record-type clos (fields arg body env))

(define-record-type node (fields id name cpt))

(define s 0)

(define (sym)
  (set! s (+ s 1))
  (string->symbol (string-append "s" (number->string s))))

(define (make-id-node name cpt)
  (make-node (sym) name cpt))

(define (node-by-id id)
  (find (lambda (node) (eq? (node-id node) id))
        nodes))

(define (cpt-entry-values v)
    (cond [(clos? v) (list v)]
          [(boolean? v) (list v)]
          [(symbol? v) (node-values (node-by-id v))]
          [else (error v "cpt-values: unknown type")]))

(define (node-values node)
  (delete-duplicates
   (apply append
          (map (compose cpt-entry-values car)
               (node-cpt node)))))

(define (bind env s v)
  (cons (cons s v) env))

(define (lookup env e)
  (cdr (assoc e env)))

(define/curry (compile-apply arg-node opt-clos)
  (compile (clos-body opt-clos)
           (bind (clos-env opt-clos) (clos-arg opt-clos) (node-id arg-node))))

(define (compile e env)
  (define make-cpt
    (match-lambda
     [(:or #t #f)
      `([,e . 1.0])]
     [('lambda arg body)
      `([,(make-clos arg body env) . 1.0])]
     [('flip p)
      `([#t . ,p]
        [#f . ,(- 1.0 p)])]
     [('if test cons alt)
      (let ([test-node (compile test env)]
            [cons-node (compile cons env)]
            [alt-node (compile alt env)])
        `([,(node-id cons-node) . ,(node-id test-node)]
          [,(node-id alt-node) . (not ,(node-id test-node))]))]
     [(opt arg)
      (let* ([opt-node (compile opt env)]
             [arg-node (compile arg env)]
             [opt-closures (node-values opt-node)]
             [apply-nodes (map (compile-apply arg-node) opt-closures)])
        (map (lambda (opt-clos apply-node)
               `(,(node-id apply-node) . (equal? ,(node-id opt-node) ,opt-clos)))
             opt-closures
             apply-nodes))]
     [sym
      `([,(lookup env e) . 1.0])]))
  (let ([node (make-id-node e (make-cpt e))])
    (set! nodes (cons node nodes))
    node))

(define (print-cpt cpt)
  (map (lambda (row)
         (for-each display
                   `(,"    " ,(car row) ": " ,(cdr row) ,"\n")))
       cpt)
  (display "\n"))
  
(define (print-node node)
  (for-each display
            (list "node " (node-id node) "\n"
                  "  e: " (node-name node) "\n"
                  "  cpt:\n"))
  (print-cpt (node-cpt node)))

(define nodes '())

(define (bayesnet-compiler e)
  (set! nodes '())
  (compile e '())
  (map print-node nodes))

(bayesnet-compiler '(((lambda e (if (flip .2) (lambda x (if (flip .4) x #f)) (lambda y e))) (flip .7)) (flip .3)))