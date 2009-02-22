;; This file is part of git-aegis
;;
;; Copyright (C) 2009 Alexey Voinov <alexey.v.voinov@gmail.com>
;;
;; This is free  software:  you  can redistribute it  and/or  modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your opinion) any later version.
#lang scheme/base
(require scheme/cmdline
         aegis/convert
         aegis/project)

(define display-list-and-exit (make-parameter #f))

(define projects
  (command-line #:program "git-aegis"
                #:once-each
                (("-r" "--repository") path
                 "Aegis repository path"
                 (ae-repository path))
                (("-l" "--list")
                 "List of projects in repository"
                 (display-list-and-exit #t))
                #:args projects
                projects))

(cond ((display-list-and-exit)
       (for ((name (directory-list (ae-repository))))
         (let ((path (build-path (ae-repository) name "info/trunk")))
           (when (file-exists? path)
             (printf "~a~n" name)))))
      (else
       (for ((name projects))
         (make-directory name)
         (parameterize ((current-directory name))
           (project->git name)))))
