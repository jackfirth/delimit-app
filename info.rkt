#lang info
(define collection "delimit-app")
(define scribblings '(("main.scrbl" () (library) "delimit-app")))
(define deps '(("base" #:version "6.4")
               "fancy-app"))
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "rackunit-lib"))
