;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adapted to elisp from CL version from:
;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

;; Compute pi

;; (require 'cl-lib)

(defvar elb-acc)
(defvar elb-den)
(defvar elb-num)

(defalias 'elb-extract-digit
  #'(lambda (nth)
      (truncate (+ (* elb-num nth) elb-acc) elb-den)))

(defalias 'elb-eliminate-digit
  #'(lambda (d)
      (setq elb-acc (- elb-acc (* elb-den d)))
      (setq elb-acc (* elb-acc 10))
      (setq elb-num (* elb-num 10))))

(defalias 'elb-next-term
  #'(lambda (k)
      (let ((k2 (1+ (* k 2))))
        (setq elb-acc (+ elb-acc (* elb-num 2)))
        (setq elb-acc (* elb-acc k2))
        (setq elb-den (* elb-den k2))
        (setq elb-num (* elb-num k)))))

(defalias 'elb-pidigits
  #'(lambda (x)
      (let ((elb-acc 0)
            (elb-den 1)
            (elb-num 1)
            (res nil))
        (let ((d 0) (k 0) (i 0) (n 10000))
          (while (null (>= i n))
            (setq n x)
            (elb-next-term (setq k (1+ k)))
            (if (> elb-num elb-acc)
                nil
              (setq d (elb-extract-digit 3))
              (if (/= d (elb-extract-digit 4))
                  nil
                (setq res (cons d res))
                (setq i (1+ i))
                (elb-eliminate-digit d))))
          nil)
        (reverse res))))

(defalias 'elb-pidigits-entry
  #'(lambda ()
      (let* ((--cl-var-- 1000))
        (while (>= (setq --cl-var-- (1- --cl-var--)) 0)
          (elb-pidigits 500))
        nil)))

(provide 'elb-pidigits)
