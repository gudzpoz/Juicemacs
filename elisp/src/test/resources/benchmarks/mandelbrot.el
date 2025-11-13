;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; This file is NOT part of GNU Emacs.

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

;; Mandelbrot, rewritten from the Java version here
;; http://benchmarksgame.alioth.debian.org/u64q/program.php?test=mandelbrot&lang=yarv&id=3

;; Copyright © 2004-2013 Brent Fulgham
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;   * Redistributions of source code must retain the above copyright notice,
;;     this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above copyright notice,
;;     this list of conditions and the following disclaimer in the documentation
;;     and/or other materials provided with the distribution.
;;
;;   * Neither the name of "The Computer Language Benchmarks Game" nor the name
;;     of "The Computer Language Shootout Benchmarks" nor the names of its
;;     contributors may be used to endorse or promote products derived from this
;;     software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; The Computer Language Benchmarks Game
;; http://benchmarksgame.alioth.debian.org
;;
;;  contributed by Karl von Laudermann
;;  modified by Jeremy Echols
;;  modified by Detlef Reichl
;;  modified by Joseph LaFata
;;  modified by Peter Zotov

(defvar elb-mandelbrot-input 750)
(defvar elb-mandelbrot-expected-output 192)

(defun elb-mandelbrot (size)
  (let ((sum 0) (byte-acc 0) (bit-num 0) (y 0))
    (while (< y size)
      (let ((ci (- (/ (* 2.0 y) size) 1.0))
            (x 0))
        (while (< x size)
          (let ((zr 0.0) (zrzr 0.0)
                (zi 0.0) (zizi 0.0)
                (cr (- (/ (* 2.0 x) size) 1.5))
                (z 0)
                (escape 1)) ;; this was originally a let block
            (while (< z 50)
              (let ((tr (+ (- zrzr zizi) cr))
                    (ti (+ (* 2.0 zr zi) ci)))
                (setq zr tr
                      zi ti)
                (setq zrzr (* zr zr)
                      zizi (* zi zi))
                (if (> (+ zrzr zizi) 4.0)
                    (setq escape 0 z 50))
                (setq z (1+ z))))
            (setq byte-acc (logior (ash byte-acc 1) escape))
            (setq bit-num (1+ bit-num))
            (if (= 8 bit-num)
                (setq sum (logxor sum byte-acc)
                      byte-acc 0 bit-num 0)
              (if (= x (1- size))
                  (setq byte-acc (ash byte-acc (- 8 bit-num))
                        sum (logxor sum byte-acc)
                        byte-acc 0 bit-num 0)))
            (setq x (1+ x))))
        (setq y (1+ y))))
    sum))

(defun elb-mandelbrot-entry ()
  (= elb-mandelbrot-expected-output
     (elb-mandelbrot elb-mandelbrot-input)))

(provide 'elb-mandelbrot)
