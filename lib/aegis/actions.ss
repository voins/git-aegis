;; This file is part of git-aegis
;;
;; Copyright (C) 2009 Alexey Voinov <alxey.v.voinov@gmail.com>
;;
;; This is free  software:  you  can redistribute it  and/or  modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your opinion) any later version.
#lang scheme/base
(require scheme/dict)
(provide merge-actions
         branch-actions
         filter-actions)

(define (merge-actions actions merged)
  (for/fold ((merged merged)) ((action (dict-ref actions 'actions actions)))
    (let-values (((name a.pre a.post a.touch) (apply values action)))
      (let ((old (dict-ref merged name #f)))
        (if (not old) (dict-set merged name `(,a.pre ,a.post ,a.touch))
            (let-values (((pre post touch) (apply values old)))
              (when (or (not (equal? post a.pre))
                        (member a.pre touch)
                        (member a.post touch))
                (error 'aegis "failed applying: ~a to: ~a" action old))
              (let ((touch (if (member post (list pre a.post #f))
                               (append touch a.touch)
                               (append touch a.touch (list post)))))
                (dict-set merged name `(,pre ,a.post ,touch)))))))))


(define (branch-actions branch)
  (for/fold ((actions '())) ((commit (dict-ref branch 'commit)))
    (merge-actions commit actions)))


(define (filter-actions branch filename)
  (for/fold ((result '())) ((commit (dict-ref branch 'commit)))
    (let ((action (assoc filename (dict-ref commit 'actions '()))))
      (if (not action) result
          (append result (list action))))))
