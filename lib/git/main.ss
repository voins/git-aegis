;; This file is part of git-aegis
;;
;; Copyright (C) 2009 Alexey Voinov <alxey.v.voinov@gmail.com>
;;
;; This is free  software:  you  can redistribute it  and/or  modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your opinion) any later version
#lang scheme/base
(require scheme/system
         scheme/list
         srfi/13)
(provide git-init
         git-add
         git-rm
         git-branch-exists?
         git-checkout
         git-commit
         git-tag
         git-mktree
         git-commit-tree
         git-update-head
         git-current-branch)

(define (git-init)
  (system "git init >/dev/null 2>/dev/null"))


(define (git-add path)
  (system (format "git add ~a >/dev/null 2>/dev/null" path)))


(define (git-rm path)
  (system (format "git rm ~a >/dev/null 2>/dev/null" path)))


(define (git-branch-exists? name)
  (system (format "git rev-parse --verify -q ~a >/dev/null 2>/dev/null" name)))


(define (git-checkout name)
  (if (git-branch-exists? name)
      (system (format "git checkout ~a >/dev/null 2>/dev/null" name))
      (system (format "git checkout -b ~a  >/dev/null 2>/dev/null" name))))

(define (run input command . args)
  (let* ((proc   (process (apply format command args)))
         (output (open-output-bytes))
         (thd    (thread (lambda ()
                           (display input (second proc))
                           (close-output-port (second proc))))))
    (for ((x (first proc))) (write-byte x output))
    (thread-wait thd)
    (close-input-port (first proc))
    (close-input-port (fourth proc))
    ((fifth proc) 'wait)
    (and (zero? ((fifth proc) 'exit-code))
         (get-output-string output))))


(define (git-commit message
                    #:author (author #f)
                    #:email (email #f)
                    #:date (date #f))
  (let ((author (if author (format "GIT_AUTHOR_NAME=~s " author) ""))
        (email (if email (format "GIT_AUTHOR_EMAIL=~s " email) ""))
        (date (if date (format "GIT_AUTHOR_DATE=~s " date) "")))
  (run message "env ~a~a~a git commit -F - >/dev/null 2>/dev/null"
       author email date)))


(define (git-tag name message)
  (run message "git tag -F - ~a" name))


(define (git-mktree content)
  (string-trim-right (run content "git mktree") #\newline))

(define (git-commit-tree message tree)
  (string-trim-right (run message "git commit-tree ~a" tree) #\newline))

(define (git-update-head branch id)
  (system (format "git update-ref refs/heads/~a ~a >/dev/null 2>/dev/null"
                  branch id)))

(define (git-current-branch)
  (regexp-replace #rx"^refs/heads/([^\n]*).*$"
                  (run "" "git symbolic-ref -q HEAD")
                  "\\1"))
