#lang racket/base

(require net/url
         json
         adjutor
         racket/class
         racket/match
         racket/port
         racket/list
         racket/contract
         racket/generic
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide number/r
         any/r
         list/r
         hash/r
         list-no-duplicates/c
         (struct-out r-package) ;protected by structure
         r-package-can-add-functions/c
         opencpu-server<%>
         remote-opencpu-server<%>
         default-opencpu-server
         no-opencpu-server
         opencpu-error?
         define-r-function
         opencpu-network-error?
         (contract-out
          [current-opencpu-server
           (parameter/c (is-a?/c opencpu-server<%>))]
          [struct r-user ([name string?]
                          [packages (and/c (listof r-package?)
                                           list-no-duplicates/c)])]
          [struct r-function ([package r-package?]
                              [name string?]
                              [arg/c flat-contract?]
                              [result/c flat-contract?])
            #:omit-constructor]
          [make-r-function 
           (->i {[pkg r-package-can-add-functions/c]
                 [name string?]}
                {#:arg/c [arg/c flat-contract?]
                 #:result/c [result/c flat-contract?]}
                [_ {pkg arg/c result/c}
                   (->* {(if (unsupplied-arg? arg/c)
                             (or/c list/r hash/r)
                             arg/c)}
                        #:pre (send (current-opencpu-server) accept-package? pkg)
                        (if (unsupplied-arg? result/c)
                            any/r
                            result/c))])]
          [make-opencpu-server 
           (->i {[u (struct/dc url
                               [scheme string?]
                               [host string?])]}
                {#:packages [pkgs (or/c #f (and/c (listof r-package?)
                                                  list-no-duplicates/c))]
                 #:users [users (and/c (listof r-user?) list-no-duplicates/c)]}
                #:pre/desc {pkgs users}
                (check-no-duplicate-packages pkgs users)
                [_ (is-a?/c remote-opencpu-server<%>)])]
          [make-caching-opencpu-server
           (-> (is-a?/c opencpu-server<%>)
               (is-a?/c opencpu-server<%>))]
          [opencpu-network-error-orig-message
           (-> any/c string?)]
          [opencpu-network-error-orig-marks
           (-> any/c continuation-mark-set?)]
          [struct (exn:fail:network:opencpu exn:fail:network)
            ([message string?]
             [continuation-marks continuation-mark-set?]
             [orig-message string?]
             [orig-marks continuation-mark-set?])
            #:omit-constructor]
          [struct (exn:fail:network:errno:opencpu exn:fail:network:errno)
            ([message string?]
             [continuation-marks continuation-mark-set?]
             [errno (cons/c exact-integer? (or/c 'posix 'windows 'gai))]
             [orig-message string?]
             [orig-marks continuation-mark-set?])
            #:omit-constructor]
          [struct (exn:fail:opencpu exn:fail)
            ([message string?]
             [continuation-marks continuation-mark-set?]
             [status bytes?])
            #:omit-constructor]
          [struct (exn:fail:opencpu:server exn:fail:opencpu)
            ([message string?]
             [continuation-marks continuation-mark-set?]
             [status bytes?])
            #:omit-constructor]
          [struct (exn:fail:opencpu:r exn:fail:opencpu)
            ([message string?]
             [continuation-marks continuation-mark-set?]
             [status bytes?]
             [r-error-message string?])
            #:omit-constructor]
          ))

(define list-no-duplicates/c
  (flat-named-contract 'list-no-duplicates/c
                       (and/c list?
                              (λ (v) (equal? v (remove-duplicates v))))))

(define number/r
  (flat-named-contract 'number/r
                       (or/c exact-integer? inexact-real?)))

(define-values {any/r list/r hash/r}
  (flat-murec-contract ([any/r
                         (or/c boolean? 
                               string? 
                               number/r
                               list/r
                               hash/r)]
                        [list/r (listof any/r)]
                        [hash/r (and/c (hash/c symbol? any/r
                                               #:immutable #t)
                                       hash-eq?)])
                       (values any/r list/r hash/r)))

(structure r-package (name guard)
  #:constructor (λ (name [guard (λ () #t)])
                  (raw-constructor name guard))
  #:constructor-contract (->* {string?} {(-> any/c)} any)
  #:match-expander (syntax-parser
                     [(_ name guard)
                      (raw-match-transformation #'(_ name guard))]
                     [(_ pat)
                      #'(r-package pat _)]
                     [(_)
                      #'(r-package _ _)])
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (fprintf out "#<r-package: ~v>" (r-package-name this)))])

(define r-package-can-add-functions/c
  (make-flat-contract
   #:name 'r-package-can-add-functions/c
   #:first-order (match-lambda
                   [(r-package name guard)
                    (with-handlers ([exn:fail? (λ (e) #f)])
                      (guard))]
                   [_ #f])
   #:late-neg-projection
   (λ (blame)
     (λ (val neg-party)
       (match val
         [(r-package name guard)
          (unless (guard)
            (raise-blame-error
             blame #:missing-party neg-party
             val
             '("forbidden by r-package guard"
               given: "~e")
             val))
          val]
         [_ (raise-blame-error
             blame #:missing-party neg-party
             val
             '(expected: "r-package" given: "~e")
             val)])))))

(struct r-user (name packages)
  #:transparent)

(define (check-no-duplicate-packages raw-pkgs raw-users)
  (let/ec return
    (for*/fold ([hsh (if (list? raw-pkgs)
                         (for/hash ([pkg (in-list raw-pkgs)])
                           (values pkg 'global))
                         #hash())])
               ([usr (if (list? raw-users)
                         raw-users
                         '())]
                [pkg (in-list (r-user-packages usr))])
      (cond
        [(hash-ref hsh pkg #f)
         => (λ (other-loc)
              (return
               (list "duplicate r-package"
                     (format " package: ~e" pkg)
                     (format " location 1: ~e" other-loc)
                     (format " location 2: ~e" usr))))]
        [else
         (hash-set hsh pkg usr)]))
    #t))


(struct r-function (package name arg/c result/c)
  #:property prop:procedure
  (λ (this arg)
    (send (current-opencpu-server) call this arg))
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (match-define (r-function package name arg/c result/c)
       this)
     (write-string "#<r-function " out)
     ((case mode
        [(#t) write]
        [(#f) display]
        [else (lambda (p port) (print p port mode))])
      package out)
     (fprintf out " ~v>" name))])
      
(define opencpu-server<%>
  (interface ()
    [accept-package? (->m r-package? any/c)]
    [call (->i {[this any/c]
                [fn (this)
                    (and/c r-function?
                           (λ (fn)
                             (send this
                                   accept-package?
                                   (r-function-package fn))))]
                [arg (fn) (and/c (or/c list/r hash/r)
                                 (r-function-arg/c fn))]}
               [_ (fn) (r-function-result/c fn)])]
    ))

(define remote-opencpu-server<%>
  (interface {opencpu-server<%>}))

(define pre-printable<%>
  (interface ()
    [do-custom-write (->m output-port? (-> any/c output-port? any) any)]))

(define custom-write-mixin
  (mixin {pre-printable<%>} {printable<%>}
    (super-new)
    (inherit do-custom-write)
    (define/public (custom-write out)
      (do-custom-write out write))
    (define/public (custom-display out)
      (do-custom-write out display))
    (define/public (custom-print out mode)
      (do-custom-write out (λ (v out) (print v out mode))))))
      
(define restrictive-opencpu-server%
  (custom-write-mixin
   (class* object% {remote-opencpu-server<%> pre-printable<%>}
     (super-new)
     (init [(:url url)]
           packages
           users
           )
     (define base-url
       :url)
     (define hsh:pkg->function->url
       (make-hash))
     (define hsh:function->url
       (make-hash))
     (for ([pkg (in-list packages)])
       (hash-set! hsh:pkg->function->url
                  pkg
                  (pkg->function->url pkg)))
     (for* ([usr (in-list users)]
            [user-name (in-value (r-user-name usr))]
            [pkg (in-list (r-user-packages usr))])
           (hash-set! hsh:pkg->function->url
                      pkg
                      (pkg->function->url pkg #:user user-name)))
     (define/public (accept-package? pkg)
       (hash-has-key? hsh:pkg->function->url pkg))
     (define/public (call fn arg) 
       (match-define (r-function _ _ _ result/c)
         fn)
       (define rslt
         (opencpu-url-apply (function->url fn) arg))
       (cond
         [((flat-contract-predicate result/c) rslt)
          rslt]
         [else
          (raise-arguments-error
           'remote-opencpu-server<%>
           "result from server violates r-function contract"
           "promised" result/c
           "produced" rslt
           "r-function" fn
           "server url" (url->string (function->url fn)))]))
     (define/public (do-custom-write out recur)
       (fprintf out
                "#<object:remote-opencpu-server<%> ~v>"
                (url->string base-url)))
     (define/private (function->url fn)
       (match-define (r-function pkg _ _ _)
         fn)
       (hash-ref hsh:function->url
                 fn
                 (λ ()
                   (define function->url
                     (hash-ref hsh:pkg->function->url
                               pkg
                               (λ ()
                                 (let ([function->url (pkg->function->url pkg)])
                                   (hash-set! hsh:pkg->function->url pkg function->url)
                                   function->url))))
                   (let ([url (function->url fn)])
                     (hash-set! hsh:function->url fn url)
                     url))))
     (define/private (pkg->function->url pkg #:user [user-name #f])
       (match-define (r-package pkg-name)
         pkg)
       (define function-name->suffix 
         (let ([base (λ (fn-name)
                       (list (path/param "library" '())
                             (path/param pkg-name '())
                             (path/param "R" '())
                             (path/param fn-name '())
                             (path/param "json" '())))])
           (if user-name
               (λ (fn-name)
                 (list* (path/param "user" '())
                        (path/param user-name '())
                        (base fn-name)))
               base)))
       (λ (fn)
         (struct-copy url base-url
                      [path (append (url-path base-url)
                                    (function-name->suffix
                                     (r-function-name fn)))]))))))


(define standard-opencpu-server%
  (class restrictive-opencpu-server%
    (super-new)
    (define/override (accept-package? pkg)
      #t)))


#|
To specify an opencpu server:

global packages : (or/c #f (listof r-package?))
  #f means any package that is not a user package is accepted and assumed global
  '() means no global packages are accepted
  
|#
(define (make-opencpu-server base-url
                             #:packages [maybe-pkgs #f]
                             #:users [users '()])
  (new (if maybe-pkgs
           restrictive-opencpu-server%
           standard-opencpu-server%)
       [url base-url]
       [packages (or maybe-pkgs '())]
       [users users]))

(define default-opencpu-server
  (make-opencpu-server (string->url "https://cloud.opencpu.org/ocpu")))

(define current-opencpu-server
  (make-parameter default-opencpu-server))

(define no-opencpu-server
  (new (custom-write-mixin
        (class* object% {opencpu-server<%> pre-printable<%>}
         (super-new)
         (define/public (accept-package? pkg)
           #f)
         (define/public (call fn arg)
           (error 'call "no-opencpu-server accepts no r-function? values"))
          (define/public (do-custom-write out recur)
            (write-string "#<no-opencpu-server>" out))))))

(define (make-r-function pkg
                         name
                         #:arg/c [arg/c (or/c list/r hash/r)]
                         #:result/c [result/c any/r])
  (r-function pkg name arg/c result/c))


(define-syntax (define-r-function stx)
  (syntax-parse stx
    [(_ name:id raw-pkg (~optional (~var str-name (expr/c #'string?
                                                          #:name "function name string"))
                                   #:defaults ([str-name.c #'(symbol->string 'name)]))
        (~or (~optional (~seq #:arg/c (~var arg/c (expr/c #'flat-contract?
                                                          #:name "#:arg/c argument")))
                        #:defaults ([arg/c.c #'(or/c list/r hash/r)]))
             (~optional (~seq #:result/c (~var result/c (expr/c #'flat-contract?
                                                                #:name "#:result/c argument")))
                        #:defaults ([result/c.c #'any/r])))
        ...)
     #:declare raw-pkg (expr/c #'r-package-can-add-functions/c
                               #:name "package argument")
     #`(begin (define pkg
                raw-pkg.c)
              (define arg/c*
                arg/c.c)
              (define result/c*
                result/c.c)
              (define/contract name
                (->* {arg/c*}
                     #:pre (send (current-opencpu-server) accept-package? pkg)
                     result/c*)
                (r-function pkg str-name.c arg/c* result/c*)))]))

(define-values {prop:opencpu-error opencpu-error? opencpu-error-property-accessor}
  (make-struct-type-property 'prop:opencpu-error))

(define-generics opencpu-network-error
  (opencpu-network-error-orig-message opencpu-network-error)
  (opencpu-network-error-orig-marks opencpu-network-error)
  #:derive-property prop:opencpu-error #t)

(struct exn:fail:network:opencpu exn:fail:network (orig-message orig-marks)
  #:transparent
  #:methods gen:opencpu-network-error
  [(define (opencpu-network-error-orig-message e)
     (exn:fail:network:opencpu-orig-message e))
   (define (opencpu-network-error-orig-marks e)
     (exn:fail:network:opencpu-orig-marks e))])

(struct exn:fail:network:errno:opencpu exn:fail:network:errno (orig-message orig-marks)
  #:transparent
  #:methods gen:opencpu-network-error
  [(define (opencpu-network-error-orig-message e)
     (exn:fail:network:errno:opencpu-orig-message e))
   (define (opencpu-network-error-orig-marks e)
     (exn:fail:network:errno:opencpu-orig-marks e))])

(struct exn:fail:opencpu exn:fail (status)
  #:transparent
  #:property prop:opencpu-error #t)

(struct exn:fail:opencpu:r exn:fail:opencpu (r-error-message)
  #:transparent
  #:property prop:opencpu-error #t)

(struct exn:fail:opencpu:server exn:fail:opencpu ()
  #:transparent
  #:property prop:opencpu-error #t)

(define (opencpu-url-apply u r-args)
  (define-values {status headers in}
    (with-handlers ([exn:fail:network?
                     (λ (e)
                       (define-values {orig-message orig-marks errno}
                         (match e
                           [(exn:fail:network:errno orig-message orig-marks errno)
                            (values orig-message orig-marks errno)]
                           [(exn:fail:network orig-message orig-marks)
                            (values orig-message orig-marks #f)]))
                       (define msg
                         (string-append
                          "remote-opencpu-server<%>: network error\n"
                          (string-when errno
                            (format "  system error code: ~e\n" errno))
                          (format "  underlying error...: ~e" orig-message)))
                       (cond [errno
                              (raise (exn:fail:network:errno:opencpu
                                      msg
                                      (current-continuation-marks)
                                      errno
                                      orig-message
                                      orig-marks))]
                             [else
                              (raise (exn:fail:network:opencpu
                                      msg
                                      (current-continuation-marks)
                                      orig-message
                                      orig-marks))]))])
      (http-sendrecv/url u
                         #:method #"POST"
                         #:headers '("Content-Type: application/json")
                         #:data (jsexpr->string r-args))))
  (match status
    [(regexp #rx"200|201")
     (read-json in)]
    [(regexp #rx"400") ;error raised by R function
     (define r-error-message
       (port->string in))
     (raise (exn:fail:opencpu:r
             (string-append
              "remote-opencpu-server<%>: R reported an error\n"
              (format "  error message: ~e" r-error-message))
             (current-continuation-marks)
             status
             r-error-message))]
    [(regexp #rx"502|503") ;server error
     (raise (exn:fail:opencpu:server
             (string-append
              "remote-opencpu-server<%>: server error\n"
              (format "  status code: ~e" status))
             (current-continuation-marks)
             status))]
    [_ 
     (raise (exn:fail:opencpu:server
             (string-append
              "remote-opencpu-server<%>: server returned an unrecognized status code\n"
              (format "  status code: ~e" status))
             (current-continuation-marks)
             status))]))

(module+ test
  (define-r-function rnorm
    (r-package "stats"))

  (rnorm #hasheq([n . 3]
                 [mean . 10]
                 [sd . 10]))

  (rnorm #hasheq([n . 2])))

(define caching-opencpu-server%
  (custom-write-mixin
   (class* object% {opencpu-server<%> pre-printable<%>}
    (super-new)
    (init [(:inner-server inner-server)])
    (define inner-server
      :inner-server)
    (define cache
      (make-hash))
    (define/public (accept-package? pkg)
      (send inner-server accept-package? pkg))
    (define/public (call fn arg)
      (define fn-cache
        (hash-ref cache fn (λ ()
                             (let ([it (make-hash)])
                               (hash-set! cache fn it)
                               it))))
      (hash-ref fn-cache
                arg
                (λ ()
                  (let ([rslt (send inner-server call fn arg)])
                    (hash-set! fn-cache arg rslt)
                    rslt))))
     (define/public (do-custom-write out recur)
       (write-string "#<object:caching-opencpu-server% " out)
       (recur inner-server out)
       (write-string ">" out)))))

(define (make-caching-opencpu-server inner-server)
  (new caching-opencpu-server% [inner-server inner-server]))
