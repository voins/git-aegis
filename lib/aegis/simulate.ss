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
         aegis/actions
         aegis/project)
(provide ae-simulate)

(define (find-start project branch-name branch commit-fs)
  (or (for/or ((start commit-fs))
        (with-handlers ((exn:fail? (lambda (x) #f)))
          (append (ae-simulate project branch (list start))
                  `((,branch-name . ,(car start))))))
      (error 'aegis "not applicable branch: ~s" branch-name)))

(define (ae-simulate project branch fs)
  (let-values
      (((commit-fs branches)
        (for/fold ((commit-fs (if (null? fs) '(("start")) fs)) (branches '()))
            ((commit (dict-ref branch 'commits '())))
          (let ((branch-name (dict-ref commit 'ref-branch #f))
                (commit-name (dict-ref commit 'commit)))
            (if (not branch-name)
                (let ((fs (merge-actions commit (cdar commit-fs))))
                  (values `((,commit-name . ,fs) . ,commit-fs) branches))
                (let* ((sub-branch (ae-find branch branch-name))
                       (fs (merge-actions sub-branch (cdar commit-fs))))
                  (values `((,commit-name . ,fs) . ,commit-fs)
                          (append branches
                                  (find-start project branch-name
                                              sub-branch commit-fs)))))))))
    (for/fold ((branches branches)) ((sub-branch (dict-ref branch 'sub-branch)))
      (let ((branch-name (dict-ref sub-branch 'branch)))
        (if (dict-ref branches branch-name #f) branches
            (append branches
                    (find-start project branch-name sub-branch commit-fs)))))))