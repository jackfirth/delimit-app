#lang scribble/manual

@(require (for-label (except-in delimit-app #%app)
                     (except-in delimit-app/fancy-app #%app)
                     racket/base
                     racket/stxparam)
          scribble/example
          syntax/parse/define)

@(define (make-delimit-app-eval delimit-mod)
   (make-base-eval #:lang 'racket/base
                   `(require (for-syntax racket/base)
                             ,delimit-mod
                             racket/stxparam)))

@(define-simple-macro (delimit-app-examples form ...)
   (examples #:eval (make-delimit-app-eval 'delimit-app) form ...))

@(define-simple-macro (delimit-fancy-examples form ...)
   (examples #:eval (make-delimit-app-eval 'delimit-app/fancy-app) form ...))

@title{Delimiter-Sensitive Application}
@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

This library provides a definition of function application (an @racket[#%app]
macro) that changes meaning depending on whether parentheses, brackets, or
braces are used. By default parentheses mean normal function application,
brackets construct a @racket[list], and braces construct a @racket[hash]. Also
included is an optional @racketmodname[delimit-app/fancy-app] module that
provides out-of-the-box integration with @racketmodname[fancy-app].

@section{API Reference}
@defmodule[delimit-app]

@defform[(delimit-app v ...)]{
 Performs delimiter-sensitive application. If parentheses are used, expands to
 @racket[(delimit-app/paren v ...)]. If brackets are used, expands to
 @racket[(delimit-app/bracket v ...)]. If braces are used, expands to
 @racket[(delimit-app/brace v ...)]. Each of these forms are syntax parameters
 that can be customized with @racket[syntax-parameterize]. The default
 implementations of these parameters are defined in terms of the syntax
 parameter @racket[delimit-app-base-app], allowing a user to customize the
 underlying definition of function application irrespective of which delimiters
 are used. The type of delimiter is determined by analyzing the
 @racket['paren-shape] syntax property.
 @(delimit-app-examples
   (delimit-app + 1 2 3)
   ;; scribble/example doesn't seem to correctly pass along the 'paren-shape
   ;; property
   (eval:alts [delimit-app + 1 2 3] (delimit-app/bracket + 1 2 3))
   (eval:alts {delimit-app + 1 2 3} (delimit-app/brace + 1 2 3)))

 This identifier is also provided under the name  @racket[#%app], overriding
 traditional function application when @racketmodname[delimit-app] is
 imported.
 @(delimit-app-examples
   (+ 1 2 3)
   ;; see above comment
   (eval:alts [+ 1 2 3] (delimit-app/bracket + 1 2 3))
   (eval:alts {+ 1 2 3} (delimit-app/brace + 1 2 3)))}

@defform[(delimit-app-base-app v ...)]{
 A syntax parameter that defines what @racket[#%app] syntax @racket[delimit-app]
 is implemented in terms of. Override this to integrate @racket[delimit-app]
 with a form of function application other than the one provided by
 @racketmodname[racket/base]. See @racketmodname[delimit-app/fancy-app] for an
 example of what overriding this can achieve.
}

@deftogether[
 (@defform[(delimit-app/paren v ...)]
   @defform[(delimit-app/bracket v ...)]
   @defform[(delimit-app/brace v ...)])]{
 Syntax parameters that define what @racket[delimit-app] expands to when used
 with parentheses, brackets, or braces, respectively. The default definitions
 are:
 @itemlist[
 @item{Ordinary function application in terms of @racket[delimit-app-base-app]
   for parentheses.
   @(delimit-app-examples (delimit-app/paren + 1 2 3))}
 @item{List construction in terms of @racket[delimit-app-base-app] and
   @racket[list] for brackets.
   @(delimit-app-examples (delimit-app/bracket + 1 2 3))}
 @item{Hash construction in terms of @racket[delimit-app-base-app] and
   @racket[hash] for braces.
   @(delimit-app-examples (delimit-app/brace + 1 2 3))}]

 When overriding these parameters, be sure to use @racket[delimit-app-base-app]
 in function application positions to ensure modules that override that syntax
 parameter continue to work. Overriding is achieved with
 @racket[syntax-parameterize].
 @(delimit-app-examples
   (syntax-parameterize ([delimit-app/brace
                          (make-rename-transformer #'delimit-app/paren)])
     (delimit-app/brace + 1 2 3)))

 Note that these forms completely ignore the @racket['paren-shape] syntax
 property, which is only used by @racket[delimit-app] to dispatch to the correct
 syntax parameter.

 @(delimit-app-examples
   {delimit-app/paren + 1 2 3})}

@section{Integration with @racketmodname[fancy-app]}
@defmodule[delimit-app/fancy-app]

@defform[(delimit-app/fancy-app v ...)]{
 Like @racket[delimit-app], but where @racket[delimit-app-base-app] is
 parameterized to the @racket[#%app] macro from @racketmodname[fancy-app]. The
 @racketmodname[fancy-app] package defines application that constructs anonymous
 functions using underscores. Also provided by @racket[delimit-app/fancy-app] as
 @racket[#%app].
 @(delimit-fancy-examples
   ;; a hash-constructing lambda
   (define make-person {'name _ 'age (random 100)})
   (eval:alts (make-person 'Alice) (hash 'name 'Alice 'age 91))
   (eval:alts (make-person 'Bob) (hash 'name 'Bob 'age 56)))}
