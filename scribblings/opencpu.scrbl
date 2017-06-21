#lang scribble/manual

@title{OpenCPU}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com"
                      #:obfuscate? #t)]
@defmodule[opencpu]

@(require (for-label opencpu
                     racket
                     net/url
                     ))

This library provides an interface to the 
@link["https://www.opencpu.org/"]{OpenCPU} API, allowing
programmers to call R functions from Racket over HTTP.
Currently, the focus is on support for the stateless
@link["https://www.opencpu.org/api.html#api-json"]{JSON RPC}
usage mode.

A major goal of the library is to abstract away the construction
of API URLs, allowing different @tech{servers} to be used
interchangably — even @tech{servers} that do not perform any HTTP
requests at all — through the @racket[current-opencpu-server]
parameter. This is achieved by specifying @tech{R functions} in terms
of @tech{R packages}.

@deftogether[(@defstruct*[r-package ([name string?]
                                     [guard (-> any/c)])]
               (defproc #:link-target? #f
                        (r-package [name string?]
                                   [guard (-> any/c) (λ () #t)])
                        r-package?)
               (defform* #:kind "match expander"
                         #:link-target? #f
                         [(r-package name-pat guard-pat)
                          (r-package name-pat)
                          (r-package)]))]{
 Specifies an @deftech{R package} named @racket[name] which corresponds to
 the entities implementing the OpenCPU
 @link["https://www.opencpu.org/api.html#api-package"]{R package API}.
 Whether a package is global or user-specific is a property of the
 @tech{server}, not of the package specification: the same package may be
 global on one @tech{server} and user-specific on another. For this reason,
 @racket[r-package] values are only @racket[equal?] when they are @racket[eq?].

 If the optional @racket[guard] argument is provided to the constructor,
 @tech{R functions} can only be added to the package when @racket[guard] returns
 a non-false result. See also @racket[r-package-can-add-functions/c].
}

@deftogether[
 (@defform[(define-r-function id pkg maybe-name-str
             option ...)
           #:grammar ([maybe-name-str (code:line) name-str]
                      [option
                       (code:line #:arg/c arg/c-expr)
                       (code:line #:result/c result/c-expr)])
           #:contracts ([pkg r-package-can-add-functions/c]
                        [name-str string?]
                        [arg/c-expr flat-contract?]
                        [result/c-expr flat-contract?])]
   @defproc[(make-r-function [pkg r-package-can-add-functions/c]
                             [name string?]
                             [#:arg/c arg/c flat-contract? (or/c list/r hash/r)]
                             [#:result/c result/c flat-contract? any/r])
            (and/c r-function?
                   (->* {arg/c}
                        #:pre (send (current-opencpu-server) #,(method opencpu-server<%> accept-package?) pkg)
                        result/c))])]{
 Both the function @racket[make-r-function] and the special form
 @racket[define-r-function] specify a new @tech{R function} assosciated with
 the @tech{R package} @racket[pkg]: the difference is that @tech{R functions}
 created using @racket[define-r-function] report contract violations in terms of
 @racket[id] and the string naming the function is inferred from @racket[id]
 unless @racket[name-str] is provided.

 An @deftech{R function} is a procedure of a single argument, which must be either
 a list (for by-position arguments to a function) or a hash table (for functions with
 named arguments) and must satisfy the contract @racket[arg/c]. Calling the function
 sends a request to the @tech{server} determined by @racket[current-opencpu-server],
 which must support the function's @tech{R package}, and returns its result, which
 must satisfy the contract @racket[result/c]. (Specifically, it uses the server's
 @(method opencpu-server<%> call) method.)

 For low-level details on the representation of @tech{R functions}, see
 @racket[r-function].
}

@defparam[current-opencpu-server server (is-a?/c opencpu-server<%>)
          #:value default-opencpu-server]{
 Specifies the @tech{server} to be used for calls to @tech{R functions}.
 A @deftech{server} will usually perform an HTTP request to some OpenCPU
 server, but may implement its functionality in arbitrary ways: for example,
 @racket[no-opencpu-server] is a @tech{server} which doesn't allow any
 @tech{R functions} to be run.
}

@defproc[(make-opencpu-server
          [u (struct/dc url
                        [scheme string?]
                        [host string?])]
          [#:packages pkgs (or/c #f (and/c (listof r-package?)
                                           list-no-duplicates/c)) #f]
          [#:users users (and/c (listof r-user?) list-no-duplicates/c) '()])
         (is-a?/c remote-opencpu-server<%>)]{
 Creates a @tech{server} that implements @tech{R functions} by performing HTTP requests
 to the OpenCPU server with base url @racket[u].

 Any @tech{R package} included in an @racket[r-user] value in @racket[users]
 is treated as local to that user.

 If @racket[pkgs] is @racket[#f], all other @tech{R packages} are supported and are
 assumed to be installed globally. If @racket[pkgs] is a list of @tech{R packages},
 only the listed packages are supported as global packages.

 A given @tech{R package} may appear at most once, either in the @racket[pkgs]
 or the @racket[users] argument to @racket[make-opencpu-server].
}

@definterface[remote-opencpu-server<%> (opencpu-server<%>)]{
 Used to tag @tech{servers} creaded by @racket[make-opencpu-server].
}

@defstruct*[r-user ([name string?]
                    [packages (and/c (listof r-package?) list-no-duplicates/c)])]{
 Used with @racket[make-opencpu-server] to define @tech{R packages} installed
 locally to a particular user.
}

@defthing[default-opencpu-server (is-a?/c remote-opencpu-server<%>)]{
 A @tech{server} that uses the public OpenCPU server at
 @tt{https://cloud.opencpu.org/ocpu} to implement @tech{R functions}.
}

@defthing[no-opencpu-server (is-a?/c opencpu-server<%>)]{
 A @tech{server} which doesn't allow any @tech{R functions} to be run.
}

@defproc[(make-caching-opencpu-server [inner-server (is-a?/c opencpu-server<%>)])
         (is-a?/c opencpu-server<%>)]{
 Creates a @tech{server} that behaves like @racket[inner-server],
 but which caches the results of @tech{R functions} to avoid consulting 
 @racket[inner-server] on duplicate calls.
}



@section{Implementing New Types of Servers}

@definterface[opencpu-server<%> ()]{
 Any object that implements @racket[opencpu-server<%>] may be used as
 a @tech{server}, allowing @tech{R functions} to be implemented in arbitrary ways.

 @defmethod[(accept-package? [pkg r-package?])
            any/c]{
  Should return @racket[#f] to indicate that the @tech{R package} @racket[pkg]
  is not supported by @(this-obj), in which case the contract on
  @(method opencpu-server<%> call) will prevent any @tech{R functions} assosciated
  with @racket[pkg] from being run. For example,
  @racket[(send no-opencpu-server #,(method opencpu-server<%> accept-package?) some-package)]
  always returns @racket[#f].
 }

 @defmethod[(call [fn (and/c r-function?
                             (λ (fn)
                               (send #,(this-obj)
                                     #,(method opencpu-server<%> accept-package?)
                                     (r-function-package fn))))]
                  [arg (and/c (or/c list/r hash/r)
                              (r-function-arg/c fn))])
            (r-function-result/c fn)]{
  Tells @(this-obj) to execute its implementation of the @tech{R function} @racket[fn]
  using the argument @racket[arg]. The implementation may perform an HTTP request
  to an OpenCPU server (as @tech{servers} created by @racket[make-opencpu-server] do),
  but alternatively may produce the result in any arbitrary way (e.g. by consulting a
  cache).
 }
}

@defstruct*[r-function ([package r-package?]
                        [name string?]
                        [arg/c flat-contract?]
                        [result/c flat-contract?])
            #:omit-constructor]{
 The low-level representation of @tech{R functions}, which may be useful when implementing
 @(method opencpu-server<%> call). Note that the constructor is not provided: instead,
 use @racket[define-r-function] or @racket[make-r-function].
}



@section{Contracts}

@deftogether[(@defthing[any/r flat-contract?
                        #:value (or/c boolean? 
                                      string? 
                                      number/r
                                      list/r
                                      hash/r)]
               @defthing[number/r flat-contract?
                         #:value (or/c exact-integer? inexact-real?)]
               @defthing[list/r flat-contract?
                         #:value (listof any/r)]
               @defthing[hash/r flat-contract?
                         #:value (and/c (hash/c symbol? any/r
                                                #:immutable #t)
                                        hash-eq?)])]{
 Contracts recognizing the types of values that can be accepted and produced
 by @tech{R functions}
}

@defthing[list-no-duplicates/c flat-contract?]{
 Reconizes lists that do not contain any duplicate entries
 (according to @racket[equal?]).
}

@defthing[r-package-can-add-functions/c flat-contract?]{
 Recognizes @tech{R packages} for which the @racket[r-package-guard] would permit
 @tech{R functions} to be added when the contract is checked.
}

@defform[(dataframe-entry/c [key-id . contract-expr] ...)
         #:contracts ([contract-expr flat-contract?])]{
 Creates a flat contract accepting immutable, @racket[eq?]-based hash-tables
 consisting of @italic{exactly} the symbolic form of each @racket[key-id]
 as keys (i.e. hashes with missing or extra keys will be rejected),
 where each corresponding value must satisfy @racket[(and/c contract-expr any/r)].
 Thus, the resulting contract is more strict than @racket[hash/r].

 These contracts may be used in any context, but are particularly useful in
 writing contracts for R "data frame" values, which are represented as a list
 of hash tables conforming to the same @racket[dataframe-entry/c] contract,
 where each @racket[key-id] is translated as the heading of "column" of the dataframe.
}

@section{Exceptions}

This section documents exceptions that may be raised by this library.
Note that none of the constructors for structures documented in this
section are provided.

@defproc[(opencpu-error? [v any/c]) any/c]{
 A predicate recognizing the exceptions documented in this section.
}

@defstruct*[(exn:fail:opencpu exn:fail) ([status bytes?])
            #:omit-constructor]{
 The supertype for errors resulting from the OpenCPU HTTP server
 (except contract violations, for which @racket[exn:fail:contract]
 is used). Direct instances of this type are currently only created
 if the HTTP response has an unknown status code.
}

@defstruct*[(exn:fail:opencpu:server exn:fail:opencpu) ()
            #:omit-constructor]{
 Represents an error in the OpenCPU HTTP server, which is indicated by
 an HTTP status code of @litchar{502} or @litchar{503}.
}

@defstruct*[(exn:fail:opencpu:r exn:fail:opencpu) ([r-error-message string?])
            #:omit-constructor]{
 Indicates that R encountered an error, which is indicated by an HTTP status
 code of @litchar{400}. The @racket[exn:fail:opencpu:r-r-error-message]
 (which is also included in the @racket[exn-message]) is the error message from R.
}

@deftogether[(@defproc[(opencpu-network-error? [v any/c]) any/c]
               @defproc[(opencpu-network-error-orig-message [e opencpu-network-error?])
                        string?]
               @defproc[(opencpu-network-error-orig-marks [e opencpu-network-error?])
                        continuation-mark-set?]
               @defstruct*[(exn:fail:network:opencpu exn:fail:network)
                           ([orig-message string?]
                            [orig-marks continuation-mark-set?])
                           #:omit-constructor]
               @defstruct*[(exn:fail:network:errno:opencpu exn:fail:network:errno)
                           ([orig-message string?]
                            [orig-marks continuation-mark-set?])
                           #:omit-constructor])]{
 When this library encounters an @racket[exn:fail:network] or an
 @racket[exn:fail:network:errno], it re-raises the exception with an OpenCPU-specific
 subtype. The message and continuation marks from the underlying exception are preserved
 and can be accessed with the generic functions
 @racket[opencpu-network-error-orig-message] and @racket[opencpu-network-error-orig-marks],
 respectively.
}