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
         aegis/read)
(provide merge-actions
         branch-actions
         diff-actions
         filter-actions
         ae-read-actions)

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
  (for/fold ((actions '())) ((commit (dict-ref branch 'commits)))
    (merge-actions commit actions)))


(define (diff-actions branch)
  (let* ((fs      (branch-actions branch))
         (actions (dict-ref branch 'actions '()))
         (result
          (for/fold ((result '())) ((action actions))
            (let-values (((name a.pre a.post a.touch) (apply values action)))
              (let ((old (dict-ref fs name #f)))
                (if (not old) (dict-set result name `(,a.pre ,a.post ,a.touch))
                    (let-values (((pre post touch) (apply values old)))
                      (if (equal? a.post post) result
                          (dict-set result name `(,post ,a.post ()))))))))))
    (for/fold ((result result)) ((action fs))
      (let-values (((name pre post touch) (apply values action)))
        (if (dict-ref actions name #f) result
            (dict-set result name `(,post #f ())))))))


(define (filter-actions branch filename)
  (for/fold ((result '())) ((commit (dict-ref branch 'commit)))
    (let ((action (assoc filename (dict-ref commit 'actions '()))))
      (if (not action) result
          (append result (list action))))))

(define (ae-read-actions path)
  `((actions
     ,@(let ((fs (ae-file->value (path-replace-suffix path ".fs"))))
         (for/fold ((actions '())) ((src (dict-ref fs 'src '())))
           (let* ((name    (dict-ref src 'file-name))
                  (action  (dict-ref src 'action))
                  (origin  (dict-ref src 'edit-origin '()))
                  (current (dict-ref src 'edit '()))
                  (pre     (and (memq action '(modify remove))
                                (dict-ref origin 'revision #f)))
                  (post    (and (memq action '(create modify))
                                (dict-ref current 'revision #f))))
             (dict-set actions name `(,pre ,post ()))))))))
