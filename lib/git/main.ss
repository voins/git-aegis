;; This file is part of git-aegis
;;
;; Copyright (C) 2009 Alexey Voinov <alxey.v.voinov@gmail.com>
;;
;; This is free  software:  you  can redistribute it  and/or  modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your opinion) any later version
#lang sheme/base
(require scheme/system
         scheme/list)
(provide git-init
         git-add
         git-rm
         git-branch-exists?
         git-checkout
         git-commit
         git-tag)

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


(define (run-with-input input command . args)
  (let* ((proc (process (apply format command args))))
    (display input (second proc))
    (close-input-port (first proc))
    (close-output-port (second proc))
    (close-input-port (fourth proc))
    ((fifth proc) 'wait)))
  

(define (git-commit message
                    #:author (author #f)
                    #:email (email #f)
                    #:date (date #f))
  (let ((author (if author (format "GIT_AUTHOR_NAME=~s " author) ""))
        (email (if email (format "GIT_AUTHOR_EMAIL=~s " email) ""))
        (date (if date (format "GIT_AUTHOR_DATE=~s " date) "")))
  (run-with-input message "env ~a~a~a git commit -F - >/dev/null 2>/dev/null"
                  author email date)))


(define (git-tag name message)
  (run-with-input message "git tag -F - ~a >/dev/null 2>/dev/null" name))
