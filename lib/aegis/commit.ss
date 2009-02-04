;; This file is part of git-aegis
;;
;; Copyright (C) 2009 Alexey Voinov <alxey.v.voinov@gmail.com>
;;
;; This is free  software:  you  can redistribute it  and/or  modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your opinion) any later version.
#lang scheme/base
(require scheme/dict
         scheme/nest
         scheme/date
         aegis/read
         aegis/author)
(provide ae-read-commit)

(define (commit-message info commit)
  (let ((brief (dict-ref info 'brief-description ""))
        (full  (dict-ref info 'description "")))
    (dict-set commit 'message
              (cond ((string=? full "")  (format "~a" brief))
                    ((string=? brief "") (format "~a" full))
                    (else                (format "~a~n~n~a~n" brief full))))))

(define (commit-history info commit)
  (let ((history (findf (lambda (x) (eq? (dict-ref x 'what) 'integrate-pass))
                        (dict-ref info 'history '()))))
    (if (not history) commit
        (let ((date (seconds->date (dict-ref history 'when 0)))
              (who  (ae-author (dict-ref history 'who #f))))
          `((date .   ,(date->string date #t))
            (author . ,(car who))
            (email .  ,(cdr who))
            ,@commit)))))

(define (commit-actions fs commit)
  (define (revision src edit)
    (dict-ref (dict-ref src edit '()) 'revision #f))
  (dict-set commit 'actions
            (for/fold ((actions '())) ((src (dict-ref fs 'src '())))
              (let ((action   (dict-ref src 'action))
                    (origin   (revision src 'edit-origin))
                    (revision (revision src 'edit)))
                (if (and (eq? action 'modify) (equal? origin revision))
                    actions
                    (dict-set actions
                              (dict-ref src 'file-name)
                              (list action origin revision)))))))

(define (ae-read-commit path)
  (let ((info   (ae-file->value path))
        (fs     (ae-file->value (path-replace-suffix path ".fs"))))
    (nest ((commit-actions fs)
           (commit-history info)
           (commit-message info))
          '())))
