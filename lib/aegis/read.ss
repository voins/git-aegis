;; This file is part of git-aegis
;;
;; Copyright (C) 2009 Alexey Voinov <alxey.v.voinov@gmail.com>
;;
;; This is free  software:  you  can redistribute it  and/or  modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your opinion) any later version.
#lang scheme/base

(require scheme/match)
(provide ae-read
         ae-file->value)

(define (ae-comma ch port src line col pos)
  (datum->syntax #f
                 (string->symbol (string ch))
                 (list src line col pos 1)))

(define (ae-comment ch port src line col pos)
  (case (peek-char port)
    ((#\*) (let loop ((ch (read-char port)))
             (cond ((eof-object? ch)
                    (error "unterminated comment"))
                   ((and (char=? #\* ch) (char=? #\/ (peek-char port)))
                    (make-special-comment (read-char port)))
                   (else (loop (read-char port))))))
    (else (read-syntax/recursive src port ch))))


(define (ae-parse data)
  (match data
         ((list-rest name '= value '|;| rest)
          (cons (cons (ae-parse name) (ae-parse value))
                (ae-parse rest)))
         ((list-rest value '|,| rest)
          (cons (ae-parse value) (ae-parse rest)))
         ((quote false) #f)
         ((quote true) #t)
         ((quote ZERO) 0)
         ((? symbol? sym)
          (string->symbol
           (regexp-replace* #rx"_" (symbol->string sym) "-")))
         ((? string? str) str)
         ((? number? num) num)
         ((list) '())))


(define (ae-read (port (current-input-port)))
  (parameterize ((current-readtable
                  (make-readtable (current-readtable)
                                  #\, 'terminating-macro ae-comma
                                  #\; 'terminating-macro ae-comma
                                  #\/ 'terminating-macro ae-comment)))
    (ae-parse
     (let loop ((data (read port)))
       (if (eof-object? data) '()
           (cons data (loop (read port))))))))

(define (ae-file->value path)
  (with-input-from-file path ae-read))
