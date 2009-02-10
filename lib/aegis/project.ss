;; This file is part of git-aegis
;;
;; Copyright (C) 2009 Alexey Voinov <alxey.v.voinov@gmail.com>
;;
;; This is free  software:  you  can redistribute it  and/or  modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your opinion) any later version.
#lang scheme/base
(require scheme/system
         scheme/file
         aegis/branch)
(provide ae-repository
         ae-read-project
         ae-checkout
         ae-find)

(define ae-repository
  (make-parameter (build-path (find-system-path 'temp-dir) "aegis/repository")
                  build-path))

(define (trunk-path project)
  (build-path (ae-repository) project "info/trunk"))

(define (history-path project filename)
  (build-path (ae-repository) project "history" filename))

(define (ae-read-project project)
  `((project . ,project)
    (trunk . ,(ae-read-branch "trunk" (trunk-path project)))))

(define (ae-checkout project filename revision)
  (let-values (((base file dir?) (split-path filename)))
    (make-directory* base)
    (system (format "co -r~a -p ~a,v > ~a"
                    revision (history-path project filename) filename))))
