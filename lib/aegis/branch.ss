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
         srfi/13
         aegis/read
         aegis/commit)
(provide ae-read-branch)

(define (ae-number->string num)
  (let ((it (number->string num)))
    (string-pad it (max 3 (string-length it)) #\0)))

(define (branch-name parent number)
  (if (string=? parent "trunk")
      (format "branch.~a" number)
      (format "~a.~a" parent number)))

(define (commit-name parent number)
  (format "~a.D~a" parent (ae-number->string number)))

(define (commit-path parent-path number)
  (let-values (((base name dir) (split-path parent-path)))
    (let* ((filename (ae-number->string number))
           (dirname  (string-drop-right filename 2)))
      (if (string=? (path->string name) "trunk")
          (build-path base "change" dirname filename)
          (build-path (path-add-suffix parent-path ".branch")
                      dirname filename)))))

(define (read-sub-branch name path x)
  (ae-read-branch (branch-name name x) (commit-path path x)))

(define (read-sub-commit name path sub-branch x)
  (let ((delta  (dict-ref x 'delta-number))
        (change (dict-ref x 'change-number)))
    `((commit . ,(commit-name name delta))
      ,@(if (memq change sub-branch)
            `((branch . ,(branch-name name change)))
            (ae-read-commit (commit-path path change))))))

(define (ae-read-branch name path)
  (let* ((info (dict-ref (ae-file->value path) 'branch '()))
         (sub-branch (dict-ref info 'sub-branch '()))
         (commit     (sort (dict-ref info 'history '()) <
                           #:key (lambda (x) (dict-ref x 'delta-number)))))
    `((branch . ,name)
      ,@(if (null? sub-branch) '()
            `((sub-branch . ,(map (lambda (x)
                                    (read-sub-branch name path x))
                                  sub-branch))))
      ,@(if (null? commit) '()
            `((commit . ,(map (lambda (x)
                                (read-sub-commit name path sub-branch x))
                              commit)))))))
