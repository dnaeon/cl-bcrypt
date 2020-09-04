# cl-bcrypt

`cl-bcrypt` is a Common Lisp system for generating, parsing and
verification of [bcrypt][bcrypt] password hashes.

## Requirements

* [Quicklisp][Quicklisp]

## Installation

Clone the [cl-bcrypt][cl-bcrypt] repo in your [Quicklisp
local-projects directory][Quicklisp FAQ].

``` shell
git clone https://github.com/dnaeon/cl-bcrypt.git
```

Load the system.

``` common-lisp
CL-USER> (ql:quickload :cl-bcrypt)
```

## Supported Algorithm Identifiers

The supported hash algorithm identifiers are `2a` and `2b`.

## Usage

The following section provides some examples to get you started
with the [cl-bcrypt][cl-bcrypt] system.

The functions discussed here are availabe in the `CL-BCRYPT` (and its
nickname `BCRYPT`) package.

In order to create a new bcrypt password you need to use the
`BCRYPT:MAKE-PASSWORD` function, e.g.

``` common-lisp
CL-USER> (defparameter *password*
           (bcrypt:make-password "my-secret-password"))
*PASSWORD*
```

`BCRYPT:MAKE-PASSWORD` accepts keyword parameters, which allow you to
specify a different salt (e.g. obtained by `BCRYPT:GENERATE-SALT`),
different cost factor than the default, and a different algorithm
identifier than the default (e.g. `2a`).

If you don't specify explicitely a salt, a random one will be
generated for you by the `BCRYPT:GENERATE-SALT` function.

This example specifies a cost factor of `16` and a hash algorithm
identifier `2a`.

``` common-lisp
CL-USER> (defparameter *password*
           (bcrypt:make-password "my-secret-password" :cost 16 :identifier "2a"))
*PASSWORD*
```

You can use the `BCRYPT:ALGORITHM-IDENTIFIER`, `BCRYPT:COST-FACTOR`,
`BCRYPT:SALT` and `BCRYPT:PASSWORD-HASH` readers to inspect the
returned `BCRYPT:PASSWORD` instance from the `BCRYPT:MAKE-PASSWORD`
function, e.g.

``` common-lisp
CL-USER> (bcrypt:algorithm-identifier *password*)
"2a"
CL-USER> (bcrypt:cost-factor *password*)
16
CL-USER> (bcrypt:salt *password*)
#(18 117 245 59 29 97 63 72 199 11 254 164 52 87 213 169)
CL-USER> (bcrypt:password-hash *password*)
#(94 0 171 116 90 235 30 220 57 45 147 214 210 77 244 223 63 14 153 13 140 213 183)
```

The `BCRYPT:SALT` and `BCRYPT:PASSWORD-HASH` readers return the raw
bytes of the salt and the password hash respectively.

In order to encode a `BCRYPT:PASSWORD` instance into its text
representation you need to use the `BCRYPT:ENCODE` function.

``` common-lisp
CL-USER> (bcrypt:encode *password*)
"$2a$16$ClVzMvzfNyhFA94iLDdToOVeApbDppFru3JXNUyi1y1x6MkO0KzZa"
```

A bcrypt password hash can be decoded using the `BCRYPT:DECODE` function,
which will return a new instance of `BCRYPT:PASSWORD`, e.g.

``` common-lisp
CL-USER> (bcrypt:decode "$2a$16$ClVzMvzfNyhFA94iLDdToOVeApbDppFru3JXNUyi1y1x6MkO0KzZa")
#<CL-BCRYPT:PASSWORD {1002207AD3}>
```

If you encode back the returned instance you should get the same hash
string as the one that was decoded.

The `BCRYPT:PARSE-HASH` function returns a property list of the
parts that comprise the bcrypt hash string.

``` common-lisp
CL-USER> (bcrypt:parse-hash "$2a$16$ClVzMvzfNyhFA94iLDdToOVeApbDppFru3JXNUyi1y1x6MkO0KzZa")
(:ALGORITHM-IDENTIFIER "2a"
 :COST-FACTOR "16"
 :SALT "ClVzMvzfNyhFA94iLDdToO"
 :PASSWORD-HASH "VeApbDppFru3JXNUyi1y1x6MkO0KzZa")
```

When you need to test whether a given bcrypt hash matches a given
password you can use the `BCRYPT:PASSWORD=` predicate, e.g.

``` common-lisp
CL-USER> (bcrypt:password= "my-secret-password"
                           "$2a$16$ClVzMvzfNyhFA94iLDdToOVeApbDppFru3JXNUyi1y1x6MkO0KzZa")
T
```

## Tests

Tests are provided as part of the `cl-bcrypt.test` system.

In order to run the tests you can evaluate the following expressions.

``` common-lisp
CL-USER> (ql:quickload :cl-bcrypt.test)
CL-USER> (asdf:test-system :cl-bcrypt.test)
```

Or you can run the tests in a Docker container instead.

First, build the Docker image.

``` shell
docker build -t cl-bcrypt .
```

Run the tests.

``` shell
docker run --rm cl-bcrypt
```

## Contributing

`cl-bcrypt` is hosted on [Github][cl-bcrypt]. Please contribute by
reporting issues, suggesting features or by sending patches using pull
requests.

## Authors

* Marin Atanasov Nikolov (dnaeon@gmail.com)

## License

This project is Open Source and licensed under the [BSD
License][BSD License].

[bcrypt]: https://en.wikipedia.org/wiki/Bcrypt
[Quicklisp]: https://www.quicklisp.org/beta/
[Quicklisp FAQ]: https://www.quicklisp.org/beta/faq.html
[cl-bcrypt]: https://github.com/dnaeon/cl-bcrypt
[BSD License]: http://opensource.org/licenses/BSD-2-Clause
