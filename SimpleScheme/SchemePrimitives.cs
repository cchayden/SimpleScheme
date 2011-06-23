// <copyright file="SchemePrimitives.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// Defines a set of primitives that are defined in scheme.
    /// </summary>
    public sealed class SchemePrimitives
    {
        /// <summary>
        /// Standard scheme macros
        /// </summary>
        public const string Code =
        @"(define quasiquote
          (macro (x)
            (define (constant? exp)
              (if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp))))
            (define (combine-skeletons left right exp)
              (cond
               ((and (constant? left) (constant? right))
            (if (and (eqv? (eval left) (car exp))
                 (eqv? (eval right) (cdr exp)))
                (list 'quote exp)
                (list 'quote (cons (eval left) (eval right)))))
               ((null? right) (list 'list left))
               ((and (pair? right) (eq? (car right) 'list))
            (cons 'list (cons left (cdr right))))
               (else (list 'cons left right))))
            (define (expand-quasiquote exp nesting)
              (cond
               ((vector? exp)
            (list 'apply 'vector (expand-quasiquote (vector->list exp) nesting)))
               ((not (pair? exp))
            (if (constant? exp) exp (list 'quote exp)))
               ((and (eq? (car exp) 'unquote) (= (length exp) 2))
            (if (= nesting 0)
                (second exp)
                (combine-skeletons ''unquote
                           (expand-quasiquote (cdr exp) (- nesting 1))
                           exp)))
               ((and (eq? (car exp) 'quasiquote) (= (length exp) 2))
            (combine-skeletons ''quasiquote
                       (expand-quasiquote (cdr exp) (+ nesting 1))
                       exp))
               ((and (pair? (car exp))
                 (eq? (caar exp) 'unquote-splicing)
                 (= (length (car exp)) 2))
            (if (= nesting 0)
                (list 'append (second (first exp))
                  (expand-quasiquote (cdr exp) nesting))
                (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
                           (expand-quasiquote (cdr exp) nesting)
                           exp)))
               (else (combine-skeletons (expand-quasiquote (car exp) nesting)
                        (expand-quasiquote (cdr exp) nesting)
                        exp))))
            (expand-quasiquote x 0)))

        (define delay
          (macro (exp)
            (define (make-promise proc)
              (let ((result-ready? #f)
                    (result #f))
            (lambda ()
              (if result-ready?
                  result
                  (let ((x (proc)))
                    (if result-ready?
                      result
                      (begin (set! result-ready? #t)
                             (set! result x)
                             result)))))))
            `(,make-promise (lambda () ,exp))))";
    }
}