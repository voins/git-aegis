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
         mzlib/os)
(provide ae-author)

(define authors
  '(("voins" "Alexey Voinov" . "alexey.v.voinov@gmail.com")))

(define (ae-author who)
  (or (dict-ref authors who #f)
      (cons who (format "~a@~a" who (gethostname)))))
