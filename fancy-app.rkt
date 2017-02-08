#lang racket/base

(require (for-syntax racket/base)
         (rename-in fancy-app [#%app fancy-app])
         (only-in delimit-app delimit-app delimit-app-base-app)
         racket/stxparam
         syntax/parse/define)

(module+ test
  (require rackunit))

(define-for-syntax (paren-shape-set/stx stx base-stx)
  (syntax-property stx 'paren-shape (syntax-property base-stx 'paren-shape)))

(define-syntax (delimit-app/fancy-app stx)
  (syntax-parse stx
    [(_ v ...)
     #`(syntax-parameterize ([delimit-app-base-app
                              (make-rename-transformer #'fancy-app)])
         #,(paren-shape-set/stx #'(delimit-app v ...) stx))]))

(module+ test
  (check-equal? (delimit-app/fancy-app [delimit-app/fancy-app 1 _ 3] 2)
                (list 1 2 3)))

(provide (rename-out [delimit-app/fancy-app #%app])
         delimit-app/fancy-app)
