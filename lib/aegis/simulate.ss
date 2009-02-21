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
         scheme/function
         aegis/actions
         aegis/project)
(provide ae-simulate)

(define (find-start commit-fs branch branches)
  (let ((branch-name (dict-ref branch 'branch)))
    (or (for/or ((start commit-fs))
          (with-handlers ((exn:fail? (lambda (x) #f)))
            (dict-update (ae-simulate branches branch (list start))
                         (car start) (curry cons branch-name) '())))
        (error 'aegis "not applicable branch: ~s" branch-name))))

(define (ae-simulate branches branch fs)
  (let loop ((commits   (dict-ref branch 'commits '()))
             (commit-fs (if (null? fs) '(("start")) fs))
             (branches  branches))
    (if (null? commits) (foldl (curry find-start commit-fs) branches
                               (dict-ref branch 'sub-branch))
        (let* ((commit (car commits))
               (fs     (merge-actions commit (cdar commit-fs))))
          (loop (cdr commits)
                (cons (cons (dict-ref commit 'commit) fs) commit-fs)
                (if (not (dict-ref commit 'branch #f)) branches
                    (find-start commit-fs commit branches)))))))

