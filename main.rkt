#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/stxparam)

(module+ test
  (require rackunit))

(define-syntax (delimit-app stx)
  (define app-stx
    (case (syntax-property stx 'paren-shape)
      [(#f #\() #'delimit-app/paren] [(#\[) #'delimit-app/bracket] [(#\{) #'delimit-app/brace]))
  (syntax-parse stx
    [(_ v ...)
     #`(#,app-stx v ...)]))

(define-syntax-parameter delimit-app-base-app (make-rename-transformer #'#%app))

(define-syntax-parameter delimit-app/paren
  (make-rename-transformer #'delimit-app-base-app))

(define-syntax-parameter delimit-app/bracket
  (syntax-parser [(_ v ...) #'(delimit-app-base-app list v ...)]))

(define-syntax-parameter delimit-app/brace
  (syntax-parser [(_ v ...) #'(delimit-app-base-app hash v ...)]))

(module+ test
  (check-equal? [delimit-app 1 2 3] (list 1 2 3))
  (check-equal? {delimit-app 'a 1 'b 2} (hash 'a 1 'b 2)))

(provide (rename-out [delimit-app #%app])
         delimit-app
         delimit-app/brace
         delimit-app/bracket
         delimit-app/paren
         delimit-app-base-app)
