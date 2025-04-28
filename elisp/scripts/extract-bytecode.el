(require 'eieio)

(defclass ebytecode ()
  ((byte :type number :initarg :byte)
   ;; Extracted by Section 1
   (sname :type symbol)
   (group :type number)
   (length :type number)
   (name :type string)
   (subrp :type boolean)
   (obsolete :type boolean)
   ;; Extracted by Section 2
   (stack-popped :type number)
   (stack-pushed :type number)
   (stack-misc :type boolean)
   ))

(setq extracted-bytecodes
      (cl-loop for i from 0 to 255 collect (ebytecode :byte i)))

(setq subr-list
      '(not add sub eqlsign gtr lss leq geq
            negate plus mult delete-region
            string= string< quo rem))

(dolist (bytecode extracted-bytecodes)
  ;; Dynamic-scope magic for `disassemble-offset'.
  (defvar bytedecomp-op)
  (defvar bytedecomp-ptr)
  (let ((bytes (string (oref bytecode byte) 0 0))
        (bytedecomp-ptr 0)
        (bytedecomp-op (oref bytecode byte))
        name sname)
    ;; `disassemble-offset' says "Don't call this!", so this might break anytime.
    (disassemble-offset bytes)
    (setq sname (aref byte-code-vector bytedecomp-op))
    (oset bytecode group bytedecomp-op)
    (oset bytecode sname sname)
    (oset bytecode length (1+ bytedecomp-ptr))

    (setq name (symbol-name sname))
    (oset bytecode obsolete (string-suffix-p "-OBSOLETE" name))
    (setq name (string-replace "-OBSOLETE" "*" name))
    (cl-assert (or (string= "nil" name) (string-prefix-p "byte-" name)) t)
    (setq name (if (string-prefix-p "byte-" name) (substring name 5) name))
    (oset bytecode name name)

    (setq sname (intern-soft
                 (if (let ((case-fold-search nil)) (string-match "[0-9N*]$" name))
                     (substring name 0 -1)
                   name)))
    (oset bytecode subrp
          (or (and (memq sname subr-list) t)
              (subrp (symbol-function sname))))))

(defun assert-effect (bytes sname consumed produced)
  "Test if the bytecode BYTES consumes exactly COMSUMED values on stack and
produce PRODUCED. Opcodes with dynamically determined stack effects
should not be using this function."
  (message "Testing %s %s..." sname bytes)
  (let* ((constants (make-vector 512 nil))
         (constants (and (aset constants 0 (- #xCAFEBABE 7)) constants))
         (push-consumed (let (code (i 1))
                          (dolist (v consumed (apply #'unibyte-string (nreverse code)))
                            (aset constants i v)
                            (cl-assert (< i 64))
                            (push (+ 192 i) code) ; constant[i]
                            (setq i (1+ i)))))
         (body
          (concat
           ;; fills the stack with distinctive values
           (unibyte-string 192) ; constant[0] #xCAFEBABE
           (cl-loop for i from 1 to 7
                    concat (unibyte-string 137 84)) ; dup, add1
           ;; -> stack_height=32

           ;; runs the bytecode
           push-consumed
           (apply #'unibyte-string bytes)
           ;; returns the result
           (unibyte-string 69 135) ; list3, return
           ))
         (test (make-byte-code () body constants 100))
         (_ (unless noninteractive
              (disassemble test)
              (signal 'error "do not run interactively")))
         (list (nreverse (funcall test)))
         (tos (car list))
         (tos2 (cadr list))
         (tos3 (caddr list))
         (stack-popped (length consumed))
         (stack-pushed (length produced))
         (bytecode (nth (car bytes) extracted-bytecodes)))
    (cl-assert (<= 0 stack-pushed 2))
    (pcase stack-pushed
      (0
       (cl-assert (= tos #xCAFEBABE) t))
      (1
       (cl-assert (= tos2 #xCAFEBABE) t)
       (cl-assert (equal tos (car produced)) t))
      (2
       (cl-assert (= tos3 #xCAFEBABE) t)
       (cl-assert (equal tos2 (car produced)) t)
       (cl-assert (equal tos (cadr produced)) t)))
    (oset bytecode stack-misc nil)
    (oset bytecode stack-popped stack-popped)
    (oset bytecode stack-pushed stack-pushed)))

(setq assert-effect-excludes
      '(#o000 ; crash
        #o010 #o011 #o012 #o013 #o014 #o015 #o016 #o017 ; varref
        #o020 #o021 #o022 #o023 #o024 #o025 #o026 #o027 ; varset
        #o030 #o031 #o032 #o033 #o034 #o035 #o036 #o037 ; varbind
        #o046 #o047 ; call[6], call[7]
        #o050 #o051 #o052 #o053 #o054 #o055 #o056 #o057 #o060 ; unbind

        ;; Buffer related
        #o140 #o141 #o142 #o143 #o144 #o145 #o146 #o147
        #o150 #o151 #o152 #o153 #o154 #o155 #o156 #o157
        #o160 #o161 #o162 #o163 #o164 #o165 #o166 #o167
        #o170 #o171 #o172 #o173 #o174 #o175 #o176 #o177

        #o202 #o203 #o204 #o205 #o206 ; goto*
        #o207 ; return

        ;; Buffer related
        #o212 #o213 #o214 #o215 #o216 #o217 #o220 #o221 #o222 #o223

        #o257 ; listN
        #o260 ; concatN
        #o261 ; insertN
        #o262 #o263 ; stackset, stackset2
        #o266 ; discardN
        #o267 ; switch
        #o300 ; constant
        ))

(dolist (byte assert-effect-excludes)
  (oset (nth byte extracted-bytecodes) stack-misc t))

(assert-effect '(1) 'byte-stack-ref
               '()
               '(#xCAFEBABD))
(assert-effect '(2) 'byte-stack-ref
               '()
               '(#xCAFEBABC))
(assert-effect '(3) 'byte-stack-ref
               '()
               '(#xCAFEBABB))
(assert-effect '(4) 'byte-stack-ref
               '()
               '(#xCAFEBABA))
(assert-effect '(5) 'byte-stack-ref
               '()
               '(#xCAFEBAB9))
(assert-effect '(6 6) 'byte-stack-ref
               '()
               '(#xCAFEBAB8))
(assert-effect '(7 7 0) 'byte-stack-ref
               '()
               '(#xCAFEBAB7))
(assert-effect '(32) 'byte-call
               '(+)
               '(0))
(assert-effect '(33) 'byte-call
               '(+ 1)
               '(1))
(assert-effect '(34) 'byte-call
               '(+ 1 1)
               '(2))
(assert-effect '(35) 'byte-call
               '(+ 1 1 1)
               '(3))
(assert-effect '(36) 'byte-call
               '(+ 1 1 1 1)
               '(4))
(assert-effect '(37) 'byte-call
               '(+ 1 1 1 1 1)
               '(5))
(assert-effect '(49 0 0) 'byte-pushconditioncase
               '(t)
               '())
(assert-effect '(50 0 0) 'byte-pushcatch
               '(t)
               '())
(assert-effect '(56) 'byte-nth
               '(0 (t))
               '(t))
(assert-effect '(57) 'byte-symbolp
               '(t)
               '(t))
(assert-effect '(58) 'byte-consp
               '((t . t))
               '(t))
(assert-effect '(59) 'byte-stringp
               '("")
               '(t))
(assert-effect '(60) 'byte-listp
               '(nil)
               '(t))
(assert-effect '(61) 'byte-eq
               '(t t)
               '(t))
(assert-effect '(62) 'byte-memq
               '(t (t))
               '((t)))
(assert-effect '(63) 'byte-not
               '(nil)
               '(t))
(assert-effect '(64) 'byte-car
               '((t . nil))
               '(t))
(assert-effect '(65) 'byte-cdr
               '((nil . t))
               '(t))
(assert-effect '(66) 'byte-cons
               '(t nil)
               '((t . nil)))
(assert-effect '(67) 'byte-list1
               '(t)
               '((t)))
(assert-effect '(68) 'byte-list2
               '(t t)
               '((t t)))
(assert-effect '(69) 'byte-list3
               '(t t t)
               '((t t t)))
(assert-effect '(70) 'byte-list4
               '(t t t t)
               '((t t t t)))
(assert-effect '(71) 'byte-length
               '("")
               '(0))
(assert-effect '(72) 'byte-aref
               '([42] 0)
               '(42))
(assert-effect '(73) 'byte-aset
               '([1] 0 42)
               '(42))
(assert-effect '(74) 'byte-symbol-value
               '(noninteractive)
               '(t))
(assert-effect '(75) 'byte-symbol-function
               '(t)
               '(nil))
(assert-effect '(76) 'byte-set
               '(symbol 42)
               '(42))
(assert-effect '(77) 'byte-fset
               '(symbol 42)
               '(42))
(assert-effect '(78) 'byte-get
               '(eval risky-local-variable)
               '(t))
(assert-effect '(79) 'byte-substring
               '("str" 1 nil)
               '("tr"))
(assert-effect '(80) 'byte-concat2
               '("a" "b")
               '("ab"))
(assert-effect '(81) 'byte-concat3
               '("s" "t" "r")
               '("str"))
(assert-effect '(82) 'byte-concat4
               '("a" "b" "c" "d")
               '("abcd"))
(assert-effect '(83) 'byte-sub1
               '(1)
               '(0))
(assert-effect '(84) 'byte-add1
               '(0)
               '(1))
(assert-effect '(85) 'byte-eqlsign
               '(1 1)
               '(t))
(assert-effect '(86) 'byte-gtr
               '(1 0)
               '(t))
(assert-effect '(87) 'byte-lss
               '(0 1)
               '(t))
(assert-effect '(88) 'byte-leq
               '(1 1)
               '(t))
(assert-effect '(89) 'byte-geq
               '(0 0)
               '(t))
(assert-effect '(90) 'byte-diff
               '(0 1)
               '(-1))
(assert-effect '(91) 'byte-negate
               '(1)
               '(-1))
(assert-effect '(92) 'byte-plus
               '(1 2)
               '(3))
(assert-effect '(93) 'byte-max
               '(1 2)
               '(2))
(assert-effect '(94) 'byte-min
               '(1 2)
               '(1))
(assert-effect '(95) 'byte-mult
               '(2 3)
               '(6))
(assert-effect '(129 0 0) 'byte-constant2
               '()
               '(#xCAFEBAB7))
(assert-effect '(136) 'byte-discard
               '(t)
               '())
(assert-effect '(137) 'byte-dup
               '(t)
               '(t t))
(assert-effect '(148) 'byte-match-beginning
               '(99)
               '(nil))
(assert-effect '(149) 'byte-match-end
               '(99)
               '(nil))
(assert-effect '(150) 'byte-upcase
               '("a")
               '("A"))
(assert-effect '(151) 'byte-downcase
               '("A")
               '("a"))
(assert-effect '(152) 'byte-string=
               '("abc" "abc")
               '(t))
(assert-effect '(153) 'byte-string<
               '("111" "222")
               '(t))
(assert-effect '(154) 'byte-equal
               '((t) (t))
               '(t))
(assert-effect '(155) 'byte-nthcdr
               '(2 (1 2 3 4))
               '((3 4)))
(assert-effect '(156) 'byte-elt
               '((1 2 3 4) 2)
               '(3))
(assert-effect '(157) 'byte-member
               '((2) ((1) (2)))
               '(((2))))
(assert-effect '(158) 'byte-assq
               '(y ((x . 1) (y . 2)))
               '((y . 2)))
(assert-effect '(159) 'byte-nreverse
               '((1 2 3))
               '((3 2 1)))
(assert-effect '(160) 'byte-setcar
               '((1) 2)
               '(2))
(assert-effect '(161) 'byte-setcdr
               '((1) 2)
               '(2))
(assert-effect '(162) 'byte-car-safe
               '(t)
               '(nil))
(assert-effect '(163) 'byte-cdr-safe
               '(t)
               '(nil))
(assert-effect '(164) 'byte-nconc
               '((1) (2 3))
               '((1 2 3)))
(assert-effect '(165) 'byte-quo
               '(127 8)
               '(15))
(assert-effect '(166) 'byte-rem
               '(127 8)
               '(7))
(assert-effect '(167) 'byte-numberp
               '(1.0)
               '(t))
(assert-effect '(168) 'byte-integerp
               '(1)
               '(t))

(dolist (bytecode extracted-bytecodes)
  (when (oref bytecode subrp)
    (let* ((byte (oref bytecode byte))
           (name (oref bytecode name))
           (case-fold-search nil)
           (name (if (string-match "[0-9N*]$" name) (substring name 0 -1) name))
           (sname (intern-soft name))
           (subr (indirect-function sname))
           (arity (and subr (func-arity subr))))
      (cl-assert (or (memq sname subr-list)
                     (subrp subr))
                 t "%s" byte)
      (when (and arity (integerp (cdr arity)))
        (let ((stack-popped (cdr arity))
              (stack-pushed 1)
              (is-misc (or (not (slot-boundp bytecode 'stack-misc)) (oref bytecode stack-misc))))
          (when is-misc
            (oset bytecode stack-popped stack-popped)
            (oset bytecode stack-pushed stack-pushed))
          (cl-assert (= (oref bytecode stack-popped) stack-popped))
          (cl-assert (= (oref bytecode stack-pushed) stack-pushed)))))))

(setq other-arities
      '((#o143 insert -1 +1)
        (#o152 indent-to -1 +1)
        (#o300 constant -0 +1)))
(dolist (arity other-arities)
  (let* ((byte (car arity))
         (name (cadr arity))
         (popped (caddr arity))
         (pushed (cadddr arity))
         (bytecode (nth byte extracted-bytecodes)))
    (cl-assert (string= (oref bytecode name) (symbol-name name)))
    (oset bytecode stack-popped (abs popped))
    (oset bytecode stack-pushed pushed)))

(setq special-arities
      '(("varref" -0 +1)
        ("varset" -1 +0)
        ("varbind" -1 +0)
        ("unbind" -0 +0)
        ("goto" -0 +0)
        ("goto-if-nil" -1 +0)
        ("goto-if-not-nil" -1 +0)
        ("unwind-protect" -1 +0)
        ("save-excursion" -0 +0)
        ("stack-set" -1 +0)
        ("stack-set2" -1 +0)
        ("switch" -2 +0)
        ("constant" -0 +1)))
(dolist (bytecode extracted-bytecodes)
  (let ((arity (assoc (oref bytecode name) special-arities)))
    (when arity
      (pcase-let ((`(,_ ,popped ,pushed) arity))
        (unless (slot-boundp bytecode 'stack-popped)
          (oset bytecode stack-popped (abs popped))
          (oset bytecode stack-pushed (abs pushed)))))))

;; (setq output 'for-check)
;; (setq output 'print-all)
(setq output 'for-java)

(when (eq output 'for-check)
  (dolist (bytecode extracted-bytecodes)
    (let ((popped (and (slot-boundp bytecode 'stack-popped) (oref bytecode stack-popped)))
          (pushed (and (slot-boundp bytecode 'stack-pushed) (oref bytecode stack-pushed))))
      (message "#o%03o\t%s\t-%s+%s"
               (oref bytecode byte)
               (oref bytecode name)
               popped pushed))))

(when (eq output 'print-all)
  (pp extracted-bytecodes))

(when (eq output 'for-java)
  (message "byte[] BYTECODE_STACK_EFFECTS = new byte[]{")
  (dolist (bytecode extracted-bytecodes)
    (let ((byte (oref bytecode byte))
          (name (oref bytecode name))
          (popped (and (slot-boundp bytecode 'stack-popped) (oref bytecode stack-popped)))
          (pushed (and (slot-boundp bytecode 'stack-pushed) (oref bytecode stack-pushed))))
      (if (and popped pushed)
          (message "%d, // #o%03o (%s)" (- pushed popped) byte name)
        (message "0x7F, // #o%03o (%s)" byte name))))
  (message "};"))
