# delimit-app [![Build Status](https://travis-ci.org/jackfirth/delimit-app.svg?branch=master)](https://travis-ci.org/jackfirth/delimit-app) [![codecov](https://codecov.io/gh/jackfirth/delimit-app/branch/master/graph/badge.svg)](https://codecov.io/gh/jackfirth/delimit-app)

A Racket library that makes function application delimiter-sensitive.

```racket
> (require delimit-app)
> (+ 1 2 3)
6
> [1 2 3]
'(1 2 3)
> {'a 1 'b 2}
(hash 'a 1 'b 2)
```
