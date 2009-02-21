;; This file is part of git-aegis
;;
;; Copyright (C) 2009 Alexey Voinov <alexey.v.voinov@gmail.com>
;;
;; This is free  software:  you  can redistribute it  and/or  modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your opinion) any later version.
#lang scheme/base
(require scheme/list
         scheme/dict
         aegis/project
         aegis/actions
         aegis/simulate
         git)
(provide project->git)

(define (actions->git project actions)
  (for ((action actions))
    (let ((name (first action))
          (rev (third action)))
      (cond (rev (ae-checkout project name rev)
                 (git-add name))
            (else (git-rm name))))))

(define (commit->git project commit)
  (actions->git project (dict-ref commit 'actions))
  (git-commit (dict-ref commit 'message)
              #:author (dict-ref commit 'author)
              #:email (dict-ref commit 'email)
              #:date (dict-ref commit 'date)))

(define (branch->git project branch branches)
  (let ((branch-name (dict-ref branch 'branch))
        (current-branch (git-current-branch)))
    (git-checkout branch-name)
    (for ((commit (dict-ref branch 'commits '())))
      (let ((branch-name (dict-ref commit 'branch #f)))
        (when branch-name
          (branch->git project commit branches)
          (git-merge branch-name #:no-ff #t #:no-commit #t)))
      (commit->git project commit)
      (for-each git-checkout (dict-ref branches (dict-ref commit 'commit) '()))
      (git-checkout branch-name))
    (for ((sub-branch (dict-ref branch 'sub-branch '())))
      (branch->git project sub-branch branches))
    (git-checkout current-branch)))


(define (project->git project)
  (let ((branch (ae-find (ae-read-project project) "trunk")))
    (git-init)
    (let* ((tree (git-mktree ""))
           (commit (git-commit-tree "Repository initialized" tree)))
      (git-update-head "master" commit)
      (let ((branches (ae-simulate '() branch '())))
        (for-each git-checkout (dict-ref branches "start" '()))
        (branch->git project branch branches)))))
