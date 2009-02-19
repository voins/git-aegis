;; This file is part of git-aegis
;;
;; Copyright (C) 2009 Alexey Voinov <alexey.v.voinov@gmail.com>
;;
;; This is free  software:  you  can redistribute it  and/or  modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your opinion) any later version.
#lang scheme/base
(require scheme/dict
         scheme/date
         aegis/read
         aegis/author
         aegis/actions)
(provide ae-read-commit)

(define (commit-message info)
  (let ((brief (dict-ref info 'brief-description ""))
        (full  (dict-ref info 'description "")))
    `((message . ,(cond ((string=? full "") (format "~a" brief))
                        ((string=? brief "") (format "~a" full))
                        (else (format "~a~n~n~a~n" brief full)))))))

(define (commit-history info)
  (let ((history (findf (lambda (x) (eq? (dict-ref x 'what) 'integrate-pass))
                        (dict-ref info 'history '()))))
    (if (not history) '()
        (let ((date (seconds->date (dict-ref history 'when 0)))
              (who  (ae-author (dict-ref history 'who #f))))
          `((date .   ,(date->string date #t))
            (author . ,(car who))
            (email .  ,(cdr who)))))))

(define (ae-read-commit path)
  (let ((info   (ae-file->value path)))
    (append (commit-message info)
            (commit-history info)
            (ae-read-actions path))))

