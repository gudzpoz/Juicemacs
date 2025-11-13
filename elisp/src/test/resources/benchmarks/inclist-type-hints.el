;;; bench/inclist-type-hints.el --- Exercise type hints -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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

;; Iteratively increment the elements of a list. Same as inclist.el
;; but make use of compiler type hints.

;;; Code:

(require 'cl-lib)

(defvar elb-inclist-th-no-type-hints-len 50000)
(defvar elb-inclist-th-no-type-hints-list
  (mapcar #'random (make-list elb-inclist-th-no-type-hints-len 100)))
(defvar elb-inclist-th-no-type-hints-list-float
  (make-list elb-inclist-th-no-type-hints-len 100.0))
(defvar elb-inclist-th-no-type-hints-list-bignum
  (make-list elb-inclist-th-no-type-hints-len (1+ most-positive-fixnum)))

(defun elb-inclist-th (l)
  (declare (cl-optimize (speed 3) (safety 0)))
  (prog1 l
    (while l
      (let ((c (cl-the cons l)))
        (cl-incf (cl-the fixnum (car c)))
        (setq l (cdr c))))))

(defun elb-inclist-type-hints-entry ()
  (let ((l (copy-sequence elb-inclist-th-no-type-hints-list)))
    (cl-loop repeat 10000
             do (elb-inclist-th l))))

(defun elb-inclist-th-float (l)
  (declare (cl-optimize (speed 3) (safety 0)))
  (prog1 l
    (while l
      (let ((c (cl-the cons l)))
        (cl-incf (cl-the float (car c)))
        (setq l (cdr c))))))

(defun elb-inclist-type-hints-float-entry ()
  (let ((l (copy-sequence elb-inclist-th-no-type-hints-list-float)))
    (cl-loop repeat 10000
             do (elb-inclist-th-float l))))

(defun elb-inclist-th-bignum (l)
  (declare (cl-optimize (speed 3) (safety 0)))
  (prog1 l
    (while l
      (let ((c (cl-the cons l)))
        (cl-incf (cl-the bignum (car c)))
        (setq l (cdr c))))))

(defun elb-inclist-type-hints-bignum-entry ()
  (let ((l (copy-sequence elb-inclist-th-no-type-hints-list-bignum)))
    (cl-loop repeat 1000
             do (elb-inclist-th-bignum l))))

(defun elb-inclist-mapcar (l)
  (declare (cl-optimize (speed 3) (safety 0)))
  (mapcar #'1+ l))

(defun elb-inclist-mapcar-entry ()
  (let ((l elb-inclist-th-no-type-hints-list))
    (cl-loop repeat 1000
             do (setq l (elb-inclist-mapcar l)))))

(defun elb-inclist-mapcar-lambda (l)
  (declare (cl-optimize (speed 3) (safety 0)))
  (mapcar (lambda (x) (1+ (cl-the fixnum x))) l))

(defun elb-inclist-mapcar-lambda-entry ()
  (let ((l elb-inclist-th-no-type-hints-list))
    (cl-loop repeat 1000
             do (setq l (elb-inclist-mapcar-lambda l)))))

(defun elb-inclist-mapcar-lambda-float (l)
  (declare (cl-optimize (speed 3) (safety 0)))
  (mapcar (lambda (x) (1+ x)) l))

(defun elb-inclist-mapcar-lambda-float-entry ()
  (let ((l elb-inclist-th-no-type-hints-list-float))
    (cl-loop repeat 1000
             do (setq l (elb-inclist-mapcar-lambda-float l)))))

(provide 'elb-inclist-type-hints)
