; Copyright (c) 2010-2015, Mark Tarver

; All rights reserved.

; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. The name of Mark Tarver may not be used to endorse or promote products
;    derived from this software without specific prior written permission.

; THIS SOFTWARE IS PROVIDED BY Mark Tarver ''AS IS'' AND ANY
; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL Mark Tarver BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :shen)

;;; Kernel 41.1 stream primitives.
;;; Character streams (e.g. *standard-input*) need read-char; binary streams
;;; (e.g. file streams opened for bytes) use read-byte.
(defun |shen.char-stinput?| (stream)
  (if (subtypep (stream-element-type stream) 'character)
      '|true|
      '|false|))

(defun |shen.read-unit-string| (stream)
  (let ((c (read-char stream nil nil)))
    (if c (string c) "")))

(defvar |shen-cl.kernel-sysfunc?| (fdefinition '|shen.sysfunc?|))

(defun |shen.sysfunc?| (symbol)
  (if (not (symbolp symbol))
      '|false|
      (|or|
        (|shen-cl.lisp-prefixed?| symbol)
        (apply |shen-cl.kernel-sysfunc?| (list symbol)))))

(defun shen.pvar? (x)
  (if (and (arrayp x) (not (stringp x)) (eq (svref x 0) '|shen.pvar|))
      '|true|
      '|false|))

(defvar specials (coerce "=*/+-_?$!@~><&%{}:;`#'." 'list))

(defun symbol-characterp (c)
  (or (alphanumericp c)
      (not (null (member c specials)))))

(defun |shen.analyse-symbol?| (s)
  (if (and (> (length s) 0)
           (not (digit-char-p (char s 0)))
           (symbol-characterp (char s 0))
           (every #'symbol-characterp s))
      '|true|
      '|false|))

(defun |symbol?| (val)
  (if (and (symbolp val)
           (not (null val))
           (not (eq t val))
           (not (eq val '|true|))
           (not (eq val '|false|)))
      (|shen.analyse-symbol?| (|str| val))
      '|false|))

(defun |variable?| (val)
  (if (and (symbolp val)
           (not (null val))
           (not (eq t val))
           (not (eq val '|true|))
           (not (eq val '|false|))
           (upper-case-p (char (symbol-name val) 0))
           (every #'symbol-characterp (symbol-name val)))
      '|true|
      '|false|))

;;; Stub for pattern factorization extension (not compiled for 41.1).
;;; Returns defun unchanged (no optimization), preserving correctness.
(defun |shen.x.factorise-defun.factorise-defun| (defun-form)
  defun-form)

;;; Phase 1: HAMT-based Prolog binding environment.
;;; env[0] holds the current bindings HAMT (repurposed from the tag slot).
;;; env[1] still holds the ticket counter — unchanged.
;;; shen.gc is NOT overridden: decrementing env[1] on backtrack is still
;;; correct. Stale HAMT entries for freed tickets are harmless; they will
;;; never be looked up because those ticket numbers won't be active.

(defun |shen.hamt-bindings| (env)
  "Return the bindings HAMT stored at env[0], lazily initialising if absent."
  (let ((b (svref env 0)))
    (if (typep b 'cl-hamt:hash-dict)
        b
        (let ((empty (cl-hamt:empty-dict)))
          (setf (svref env 0) empty)
          empty))))

(defun |shen.lazyderef| (x env)
  (if (and (arrayp x)
           (not (stringp x))
           (eq (svref x 0) '|shen.pvar|))
      (multiple-value-bind (val found)
          (cl-hamt:dict-lookup (|shen.hamt-bindings| env) (svref x 1))
        (if found
            (|shen.lazyderef| val env)
            x))
      x))

;;; shen.deref does a deep (structural) dereference — must also use HAMT.
;;; The compiled version reads env[pvar.ticket] (flat array) which is never
;;; written by our HAMT-based shen.bindv.
(defun |shen.deref| (x env)
  (if (consp x)
      (cons (|shen.deref| (car x) env)
            (|shen.deref| (cdr x) env))
      (if (and (arrayp x)
               (not (stringp x))
               (eq (svref x 0) '|shen.pvar|))
          (multiple-value-bind (val found)
              (cl-hamt:dict-lookup (|shen.hamt-bindings| env) (svref x 1))
            (if found
                (|shen.deref| val env)
                x))
          x)))

(defun |shen.bindv| (pvar val env)
  (setf (svref env 0)
        (cl-hamt:dict-insert (|shen.hamt-bindings| env) (svref pvar 1) val))
  val)

;;; Override nextticket to evict the ticket from the HAMT.
;;; Without this, a ticket freed by shen.gc and then reallocated by shen.newpv
;;; would see the previous pvar's stale binding in the HAMT.
(defun |shen.nextticket| (env ticket)
  (setf (svref env 0)
        (cl-hamt:dict-remove (|shen.hamt-bindings| env) ticket))
  (setf (svref env 1) (+ ticket 1))
  env)

;;; Override shen.unwind to remove the binding from the HAMT on backtrack.
;;; The compiled version writes env[pvar.ticket] = shen.-null- (flat-array
;;; model), which does not affect the HAMT.  We must evict from the HAMT here.
(defun |shen.unwind| (pvar env result)
  (setf (svref env 0)
        (cl-hamt:dict-remove (|shen.hamt-bindings| env) (svref pvar 1)))
  result)

;;; Phase 2: OR-parallel clause trying for the type checker hot paths.

(defmacro safe-future (&body body)
  "Like lparallel:future but catches all errors in the worker thread,
   returning |false| instead of letting them escape to the SBCL debugger.
   Uses task-handler-bind which is lparallel's mechanism for handling errors
   in worker threads — it integrates with lparallel's condition-handler so
   errors are transferred cleanly rather than invoking the SBCL debugger."
  `(lparallel:future
     (lparallel:task-handler-bind
         ((error #'lparallel:invoke-transfer-error))
       ,@body)))

#+sbcl
(defun copy-env-for-fork (env)
  "Shallow copy the env absvector. Both branches share the HAMT at [0]
   (it is persistent/immutable) and diverge independently from there."
  (let* ((n (array-dimension env 0))
         (b (make-array n)))
    (dotimes (i n)
      (setf (svref b i) (svref env i)))
    b))

#+sbcl
(defun first-non-fail (futures)
  "Return the first future result that is not |false|, or |false| if all fail."
  (loop for f in futures
        for result = (handler-case (lparallel:force f)
                       (error () '|false|))
        when (not (eq result '|false|))
          return result
        finally (return '|false|)))

#+sbcl
(defvar |shen-cl.kernel-primitive| (fdefinition '|shen.primitive|))

#+sbcl
(defvar |shen-cl.kernel-system-S-h| (fdefinition '|shen.system-S-h|))

#+sbcl
(defun shen-cl.in-worker-thread-p ()
  "Return T if the current thread is an lparallel worker thread.
   Used to prevent recursive future creation which causes pool deadlocks."
  (let ((name (sb-thread:thread-name sb-thread:*current-thread*)))
    (and (stringp name)
         (string= name "lparallel"))))

#+sbcl
(defun |shen-cl.ensure-lparallel-kernel| ()
  ;; Return nil (= use sequential fallback) when called from a worker thread.
  ;; Recursive shen.system-S-h calls from within futures would deadlock the
  ;; 4-worker pool: each worker blocks on first-non-fail waiting for inner
  ;; futures that can never run because all workers are occupied.
  (when (not (shen-cl.in-worker-thread-p))
    (or (and *shen-lparallel-kernel*
             (setf lparallel:*kernel* *shen-lparallel-kernel*)
             *shen-lparallel-kernel*)
        (let ((stack-size-var (find-symbol "*DEFAULT-THREAD-STACK-SIZE*" :sb-thread)))
          ;; Must be set BEFORE make-kernel so workers inherit the nil binding.
          ;; With *debug-tasks-p* T, lparallel invokes the SBCL debugger inside
          ;; the worker thread on any error; the worker then tries to grab the
          ;; foreground while the main thread holds the lparallel mutex — deadlock.
          (setf lparallel:*debug-tasks-p* nil)
          (if stack-size-var
              (progv (list stack-size-var) (list (* 8 1024 1024))
                (setf *shen-lparallel-kernel* (lparallel:make-kernel 4)))
              (setf *shen-lparallel-kernel* (lparallel:make-kernel 4)))
          (setf lparallel:*kernel* *shen-lparallel-kernel*)
          *shen-lparallel-kernel*))))

#+sbcl
(defun |shen.primitive| (V5143 V5144 V5145 V5146 V5147 V5148)
  (if (|shen-cl.true?| (|shen.pvar?| (|shen.lazyderef| V5144 V5145)))
      (funcall |shen-cl.kernel-primitive| V5143 V5144 V5145 V5146 V5147 V5148)
      (if (|shen-cl.ensure-lparallel-kernel|)
          (first-non-fail
           (list
            ;; Clause 1: number
            (safe-future
              (let ((b (copy-env-for-fork V5145)))
                (if (|shen-cl.true?| (|shen.unlocked?| V5146))
                    (let* ((W5150 (|shen.lazyderef| V5144 b))
                           (W5151 (|freeze| (progn (|shen.incinfs|) (|when| (|number?| (|shen.lazyderef| V5143 b)) b V5146 V5147 V5148)))))
                      (if (eq W5150 '|number|) (|thaw| W5151)
                          (if (|shen-cl.true?| (|shen.pvar?| W5150)) (|shen.bind!| W5150 '|number| b W5151) '|false|)))
                    '|false|)))
            ;; Clause 2: boolean
            (safe-future
              (let ((b (copy-env-for-fork V5145)))
                (if (|shen-cl.true?| (|shen.unlocked?| V5146))
                    (let* ((W5153 (|shen.lazyderef| V5144 b))
                           (W5154 (|freeze| (progn (|shen.incinfs|) (|when| (|boolean?| (|shen.lazyderef| V5143 b)) b V5146 V5147 V5148)))))
                      (if (eq W5153 '|boolean|) (|thaw| W5154)
                          (if (|shen-cl.true?| (|shen.pvar?| W5153)) (|shen.bind!| W5153 '|boolean| b W5154) '|false|)))
                    '|false|)))
            ;; Clause 3: string
            (safe-future
              (let ((b (copy-env-for-fork V5145)))
                (if (|shen-cl.true?| (|shen.unlocked?| V5146))
                    (let* ((W5156 (|shen.lazyderef| V5144 b))
                           (W5157 (|freeze| (progn (|shen.incinfs|) (|when| (|string?| (|shen.lazyderef| V5143 b)) b V5146 V5147 V5148)))))
                      (if (eq W5156 '|string|) (|thaw| W5157)
                          (if (|shen-cl.true?| (|shen.pvar?| W5156)) (|shen.bind!| W5156 '|string| b W5157) '|false|)))
                    '|false|)))
            ;; Clause 4: symbol
            (safe-future
              (let ((b (copy-env-for-fork V5145)))
                (if (|shen-cl.true?| (|shen.unlocked?| V5146))
                    (let* ((W5159 (|shen.lazyderef| V5144 b))
                           (W5160 (|freeze| (progn (|shen.incinfs|) (|when| (|symbol?| (|shen.lazyderef| V5143 b)) b V5146 V5147 V5148)))))
                      (if (eq W5159 '|symbol|) (|thaw| W5160)
                          (if (|shen-cl.true?| (|shen.pvar?| W5159)) (|shen.bind!| W5159 '|symbol| b W5160) '|false|)))
                    '|false|)))
            ;; Clause 5: list
            (safe-future
              (let ((b (copy-env-for-fork V5145)))
                (if (|shen-cl.true?| (|shen.unlocked?| V5146))
                    (let ((W5161 (|shen.lazyderef| V5143 b)))
                      (if (null W5161)
                          (let* ((W5162 (|shen.lazyderef| V5144 b))
                                 (W5163 (|lambda| Z5164 (progn (|shen.incinfs|) (|thaw| V5148)))))
                            (if (consp W5162)
                                (let* ((W5165 (|shen.lazyderef| (car W5162) b))
                                       (W5166 (|freeze| (let* ((W5167 (|shen.lazyderef| (cdr W5162) b))
                                                                (W5168 (|lambda| Z5169 (funcall W5163 Z5169))))
                                                           (if (consp W5167)
                                                               (let* ((W5170 (car W5167))
                                                                      (W5171 (|shen.lazyderef| (cdr W5167) b))
                                                                      (W5172 (|freeze| (funcall W5168 W5170))))
                                                                 (if (null W5171) (|thaw| W5172)
                                                                     (if (|shen-cl.true?| (|shen.pvar?| W5171)) (|shen.bind!| W5171 '() b W5172) '|false|)))
                                                               (if (|shen-cl.true?| (|shen.pvar?| W5167))
                                                                   (let ((W5173 (|shen.newpv| b)))
                                                                     (|shen.gc| b (|shen.bind!| W5167 (list W5173) b (|freeze| (funcall W5168 W5173)))))
                                                                   '|false|))))))
                                  (if (eq W5165 '|list|) (|thaw| W5166)
                                      (if (|shen-cl.true?| (|shen.pvar?| W5165)) (|shen.bind!| W5165 '|list| b W5166) '|false|)))
                                (if (|shen-cl.true?| (|shen.pvar?| W5162))
                                    (let ((W5174 (|shen.newpv| b)))
                                      (|shen.gc| b (|shen.bind!| W5162 (list '|list| W5174) b (|freeze| (funcall W5163 W5174)))))
                                    '|false|)))
                          '|false|))
                    '|false|)))))
          (funcall |shen-cl.kernel-primitive| V5143 V5144 V5145 V5146 V5147 V5148))))

#+sbcl
(defun |shen-cl.system-S-h-W4952| (V4931 V4932 V4933 V4934 V4935 W4938 V4937)
  (declare (ignore V4933))
  (if (|shen-cl.true?| (|shen.unlocked?| V4935))
      (let ((W4953 (|shen.lazyderef| V4931 V4934)))
        (if (|shen-cl.true?| (|cons?| W4953))
            (let ((W4954 (|shen.lazyderef| (|hd| W4953) V4934)))
              (if (|shen-cl.true?| (|shen-cl.equal?| W4954 '|fn|))
                  (let ((W4955 (|shen.lazyderef| (|tl| W4953) V4934)))
                    (if (|shen-cl.true?| (|cons?| W4955))
                        (let ((W4956 (|hd| W4955)))
                          (let ((W4957 (|shen.lazyderef| (|tl| W4955) V4934)))
                            (if (null W4957)
                                (progn (|shen.incinfs|)
                                       (|shen.lookupsig| W4956 V4932 V4934 V4935 W4938 V4937))
                                '|false|)))
                        '|false|))
                  '|false|))
            '|false|))
      '|false|))

#+sbcl
(defun |shen-cl.system-S-h-W4958| (V4931 V4932 V4933 V4934 V4935 W4938 V4937)
  (if (|shen-cl.true?| (|shen.unlocked?| V4935))
      (let ((W4959 (|shen.lazyderef| V4931 V4934)))
        (if (|shen-cl.true?| (|cons?| W4959))
            (let ((W4960 (|hd| W4959)))
              (let ((W4961 (|shen.lazyderef| (|tl| W4959) V4934)))
                (if (|shen-cl.true?| (|cons?| W4961))
                    (let ((W4962 (|hd| W4961)))
                      (let ((W4963 (|shen.lazyderef| (|tl| W4961) V4934)))
                        (if (null W4963)
                            (let ((W4964 (|shen.newpv| V4934)))
                              (|shen.gc| V4934
                               (progn (|shen.incinfs|)
                                      (|when| (|not| (|cons?| (|shen.lazyderef| W4960 V4934)))
                                              V4934 V4935 W4938
                                              (|freeze| (|shen.lookupsig| W4960
                                                          (|cons| W4964 (|cons| '--> (|cons| V4932 '())))
                                                          V4934 V4935 W4938
                                                          (|freeze| (|shen.system-S-h| W4962 W4964 V4933 V4934 V4935 W4938 V4937))))))))
                            '|false|)))
                    '|false|)))
            '|false|))
      '|false|))

#+sbcl
(defun |shen-cl.system-S-h-W4965| (V4931 V4932 V4933 V4934 V4935 W4938 V4937)
  (if (|shen-cl.true?| (|shen.unlocked?| V4935))
      (let ((W4966 (|shen.lazyderef| V4931 V4934)))
        (if (|shen-cl.true?| (|cons?| W4966))
            (let ((W4967 (|hd| W4966)))
              (let ((W4968 (|shen.lazyderef| (|tl| W4966) V4934)))
                (if (|shen-cl.true?| (|cons?| W4968))
                    (let ((W4969 (|hd| W4968)))
                      (let ((W4970 (|shen.lazyderef| (|tl| W4968) V4934)))
                        (if (null W4970)
                            (let ((W4971 (|shen.newpv| V4934)))
                              (|shen.gc| V4934
                               (progn (|shen.incinfs|)
                                      (|shen.system-S-h| W4967 (|cons| W4971 (|cons| '--> (|cons| V4932 '())))
                                                        V4933 V4934 V4935 W4938
                                                        (|freeze| (|shen.system-S-h| W4969 W4971 V4933 V4934 V4935 W4938 V4937))))))
                            '|false|)))
                    '|false|)))
            '|false|))
      '|false|))

#+sbcl
(defun |shen-cl.system-S-h-W4972| (V4931 V4932 V4933 V4934 V4935 W4938 V4937)
  (if (|shen-cl.true?| (|shen.unlocked?| V4935))
      (let ((W4973 (|shen.lazyderef| V4931 V4934)))
        (if (|shen-cl.true?| (|cons?| W4973))
            (let ((W4974 (|shen.lazyderef| (|hd| W4973) V4934)))
              (if (|shen-cl.true?| (|shen-cl.equal?| W4974 '|cons|))
                  (let ((W4975 (|shen.lazyderef| (|tl| W4973) V4934)))
                    (if (|shen-cl.true?| (|cons?| W4975))
                        (let ((W4976 (|hd| W4975)))
                          (let ((W4977 (|shen.lazyderef| (|tl| W4975) V4934)))
                            (if (|shen-cl.true?| (|cons?| W4977))
                                (let ((W4978 (|hd| W4977)))
                                  (let ((W4979 (|shen.lazyderef| (|tl| W4977) V4934)))
                                    (if (null W4979)
                                        (let ((W4980 (|shen.lazyderef| V4932 V4934)))
                                          (let ((W4981 (|lambda| Z4982
                                                         (progn (|shen.incinfs|)
                                                                (|shen.system-S-h| W4976 Z4982 V4933 V4934 V4935 W4938
                                                                 (|freeze| (|shen.system-S-h| W4978 (|cons| '|list| (|cons| Z4982 '()))
                                                                               V4933 V4934 V4935 W4938 V4937)))))))
                                            (if (|shen-cl.true?| (|cons?| W4980))
                                                (let ((W4983 (|shen.lazyderef| (|hd| W4980) V4934)))
                                                  (let ((W4984 (|freeze| (let ((W4985 (|shen.lazyderef| (|tl| W4980) V4934)))
                                                                           (let ((W4986 (|lambda| Z4987 (funcall W4981 Z4987))))
                                                                             (if (|shen-cl.true?| (|cons?| W4985))
                                                                                 (let ((W4988 (|hd| W4985)))
                                                                                   (let ((W4989 (|shen.lazyderef| (|tl| W4985) V4934)))
                                                                                     (let ((W4990 (|freeze| (funcall W4986 W4988))))
                                                                                       (if (null W4989)
                                                                                           (|thaw| W4990)
                                                                                           (if (|shen-cl.true?| (|shen.pvar?| W4989))
                                                                                               (|shen.bind!| W4989 '() V4934 W4990)
                                                                                               '|false|)))))
                                                                                 (if (|shen-cl.true?| (|shen.pvar?| W4985))
                                                                                     (let ((W4991 (|shen.newpv| V4934)))
                                                                                       (|shen.gc| V4934 (|shen.bind!| W4985 (|cons| W4991 '()) V4934 (|freeze| (funcall W4986 W4991)))))
                                                                                     '|false|)))))))
                                                    (if (eq W4983 '|list|)
                                                        (|thaw| W4984)
                                                        (if (|shen-cl.true?| (|shen.pvar?| W4983))
                                                            (|shen.bind!| W4983 '|list| V4934 W4984)
                                                            '|false|))))
                                                (if (|shen-cl.true?| (|shen.pvar?| W4980))
                                                    (let ((W4992 (|shen.newpv| V4934)))
                                                      (|shen.gc| V4934 (|shen.bind!| W4980 (|cons| '|list| (|cons| W4992 '())) V4934 (|freeze| (funcall W4981 W4992)))))
                                                    '|false|)))
                                        '|false|)))
                                '|false|)))
                        '|false|)))
            '|false|)))
      '|false|)))

#+sbcl
(defun |shen-cl.system-S-h-W4993| (V4931 V4932 V4933 V4934 V4935 W4938 V4937)
  (if (|shen-cl.true?| (|shen.unlocked?| V4935))
      (let ((W4994 (|shen.lazyderef| V4931 V4934)))
        (if (|shen-cl.true?| (|cons?| W4994))
            (let ((W4995 (|shen.lazyderef| (|hd| W4994) V4934)))
              (if (|shen-cl.true?| (|shen-cl.equal?| W4995 '|@p|))
                  (let ((W4996 (|shen.lazyderef| (|tl| W4994) V4934)))
                    (if (|shen-cl.true?| (|cons?| W4996))
                        (let ((W4997 (|hd| W4996)))
                          (let ((W4998 (|shen.lazyderef| (|tl| W4996) V4934)))
                            (if (|shen-cl.true?| (|cons?| W4998))
                                (let ((W4999 (|hd| W4998)))
                                  (let ((W5000 (|shen.lazyderef| (|tl| W4998) V4934)))
                                    (if (null W5000)
                                        (let ((W5001 (|shen.lazyderef| V4932 V4934)))
                                          (let ((W5002 (|lambda| Z5003
                                                         (|lambda| Z5004
                                                           (progn (|shen.incinfs|)
                                                                  (|shen.system-S-h| W4997 Z5003 V4933 V4934 V4935 W4938
                                                                   (|freeze| (|shen.system-S-h| W4999 Z5004 V4933 V4934 V4935 W4938 V4937))))))))
                                            (if (|shen-cl.true?| (|cons?| W5001))
                                                (let ((W5005 (|hd| W5001)))
                                                  (let ((W5006 (|shen.lazyderef| (|tl| W5001) V4934)))
                                                    (let ((W5007 (|lambda| Z5008 (funcall (funcall W5002 W5005) Z5008))))
                                                      (if (|shen-cl.true?| (|cons?| W5006))
                                                          (let ((W5009 (|shen.lazyderef| (|hd| W5006) V4934)))
                                                            (let ((W5010 (|freeze| (let ((W5011 (|shen.lazyderef| (|tl| W5006) V4934)))
                                                                                     (let ((W5012 (|lambda| Z5013 (funcall W5007 Z5013))))
                                                                                       (if (|shen-cl.true?| (|cons?| W5011))
                                                                                           (let ((W5014 (|hd| W5011)))
                                                                                             (let ((W5015 (|shen.lazyderef| (|tl| W5011) V4934)))
                                                                                               (let ((W5016 (|freeze| (funcall W5012 W5014))))
                                                                                                 (if (null W5015)
                                                                                                     (|thaw| W5016)
                                                                                                     (if (|shen-cl.true?| (|shen.pvar?| W5015))
                                                                                                         (|shen.bind!| W5015 '() V4934 W5016)
                                                                                                         '|false|)))))
                                                                                           (if (|shen-cl.true?| (|shen.pvar?| W5011))
                                                                                               (let ((W5017 (|shen.newpv| V4934)))
                                                                                                 (|shen.gc| V4934 (|shen.bind!| W5011 (|cons| W5017 '()) V4934 (|freeze| (funcall W5012 W5017)))))
                                                                                               '|false|)))))))
                                                              (if (eq W5009 '*)
                                                                  (|thaw| W5010)
                                                                  (if (|shen-cl.true?| (|shen.pvar?| W5009))
                                                                      (|shen.bind!| W5009 '* V4934 W5010)
                                                                      '|false|))))
                                                          (if (|shen-cl.true?| (|shen.pvar?| W5006))
                                                              (let ((W5018 (|shen.newpv| V4934)))
                                                                (|shen.gc| V4934 (|shen.bind!| W5006 (|cons| '* (|cons| W5018 '())) V4934 (|freeze| (funcall W5007 W5018)))))
                                                              '|false|)))))
                                                (if (|shen-cl.true?| (|shen.pvar?| W5001))
                                                    (let ((W5019 (|shen.newpv| V4934)))
                                                      (|shen.gc| V4934
                                                                (let ((W5020 (|shen.newpv| V4934)))
                                                                  (|shen.gc| V4934
                                                                            (|shen.bind!| W5001 (|cons| W5019 (|cons| '* (|cons| W5020 '()))) V4934
                                                                                         (|freeze| (funcall (funcall W5002 W5019) W5020)))))))
                                                    '|false|)))
                                        '|false|)))
                                '|false|)))
                        '|false|)))
            '|false|)))
      '|false|)))

#+sbcl
(defun |shen-cl.system-S-h-W5021| (V4931 V4932 V4933 V4934 V4935 W4938 V4937)
  (if (|shen-cl.true?| (|shen.unlocked?| V4935))
      (let ((W5022 (|shen.lazyderef| V4931 V4934)))
        (if (|shen-cl.true?| (|cons?| W5022))
            (let ((W5023 (|shen.lazyderef| (|hd| W5022) V4934)))
              (if (|shen-cl.true?| (|shen-cl.equal?| W5023 '|@v|))
                  (let ((W5024 (|shen.lazyderef| (|tl| W5022) V4934)))
                    (if (|shen-cl.true?| (|cons?| W5024))
                        (let ((W5025 (|hd| W5024)))
                          (let ((W5026 (|shen.lazyderef| (|tl| W5024) V4934)))
                            (if (|shen-cl.true?| (|cons?| W5026))
                                (let ((W5027 (|hd| W5026)))
                                  (let ((W5028 (|shen.lazyderef| (|tl| W5026) V4934)))
                                    (if (null W5028)
                                        (let ((W5029 (|shen.lazyderef| V4932 V4934)))
                                          (let ((W5030 (|lambda| Z5031
                                                         (progn (|shen.incinfs|)
                                                                (|shen.system-S-h| W5025 Z5031 V4933 V4934 V4935 W4938
                                                                 (|freeze| (|shen.system-S-h| W5027 (|cons| '|vector| (|cons| Z5031 '()))
                                                                               V4933 V4934 V4935 W4938 V4937)))))))
                                            (if (|shen-cl.true?| (|cons?| W5029))
                                                (let ((W5032 (|shen.lazyderef| (|hd| W5029) V4934)))
                                                  (let ((W5033 (|freeze| (let ((W5034 (|shen.lazyderef| (|tl| W5029) V4934)))
                                                                           (let ((W5035 (|lambda| Z5036 (funcall W5030 Z5036))))
                                                                             (if (|shen-cl.true?| (|cons?| W5034))
                                                                                 (let ((W5037 (|hd| W5034)))
                                                                                   (let ((W5038 (|shen.lazyderef| (|tl| W5034) V4934)))
                                                                                     (let ((W5039 (|freeze| (funcall W5035 W5037))))
                                                                                       (if (null W5038)
                                                                                           (|thaw| W5039)
                                                                                           (if (|shen-cl.true?| (|shen.pvar?| W5038))
                                                                                               (|shen.bind!| W5038 '() V4934 W5039)
                                                                                               '|false|)))))
                                                                                 (if (|shen-cl.true?| (|shen.pvar?| W5034))
                                                                                     (let ((W5040 (|shen.newpv| V4934)))
                                                                                       (|shen.gc| V4934 (|shen.bind!| W5034 (|cons| W5040 '()) V4934 (|freeze| (funcall W5035 W5040)))))
                                                                                     '|false|)))))))
                                                    (if (eq W5032 '|vector|)
                                                        (|thaw| W5033)
                                                        (if (|shen-cl.true?| (|shen.pvar?| W5032))
                                                            (|shen.bind!| W5032 '|vector| V4934 W5033)
                                                            '|false|))))
                                                (if (|shen-cl.true?| (|shen.pvar?| W5029))
                                                    (let ((W5041 (|shen.newpv| V4934)))
                                                      (|shen.gc| V4934 (|shen.bind!| W5029 (|cons| '|vector| (|cons| W5041 '())) V4934 (|freeze| (funcall W5030 W5041)))))
                                                    '|false|)))
                                        '|false|)))
                                '|false|)))
                        '|false|)))
            '|false|)))
      '|false|)))

#+sbcl
(defun |shen-cl.system-S-h-W5042| (V4931 V4932 V4933 V4934 V4935 W4938 V4937)
  (if (|shen-cl.true?| (|shen.unlocked?| V4935))
      (let ((W5043 (|shen.lazyderef| V4931 V4934)))
        (if (|shen-cl.true?| (|cons?| W5043))
            (let ((W5044 (|shen.lazyderef| (|hd| W5043) V4934)))
              (if (|shen-cl.true?| (|shen-cl.equal?| W5044 '|@s|))
                  (let ((W5045 (|shen.lazyderef| (|tl| W5043) V4934)))
                    (if (|shen-cl.true?| (|cons?| W5045))
                        (let ((W5046 (|hd| W5045)))
                          (let ((W5047 (|shen.lazyderef| (|tl| W5045) V4934)))
                            (if (|shen-cl.true?| (|cons?| W5047))
                                (let ((W5048 (|hd| W5047)))
                                  (let ((W5049 (|shen.lazyderef| (|tl| W5047) V4934)))
                                    (if (null W5049)
                                        (let ((W5050 (|shen.lazyderef| V4932 V4934)))
                                          (let ((W5051 (|freeze| (progn (|shen.incinfs|)
                                                                        (|shen.system-S-h| W5046 '|string| V4933 V4934 V4935 W4938
                                                                         (|freeze| (|shen.system-S-h| W5048 '|string| V4933 V4934 V4935 W4938 V4937)))))))
                                            (if (eq W5050 '|string|)
                                                (|thaw| W5051)
                                                (if (|shen-cl.true?| (|shen.pvar?| W5050))
                                                    (|shen.bind!| W5050 '|string| V4934 W5051)
                                                    '|false|))))
                                        '|false|)))
                                '|false|)))
                        '|false|))
                  '|false|))
            '|false|))
      '|false|))

#+sbcl
(defun |shen-cl.system-S-h-W5052| (V4931 V4932 V4933 V4934 V4935 W4938 V4937)
  (if (|shen-cl.true?| (|shen.unlocked?| V4935))
      (let ((W5053 (|shen.lazyderef| V4931 V4934)))
        (if (|shen-cl.true?| (|cons?| W5053))
            (let ((W5054 (|shen.lazyderef| (|hd| W5053) V4934)))
              (if (|shen-cl.true?| (|shen-cl.equal?| W5054 '|lambda|))
                  (let ((W5055 (|shen.lazyderef| (|tl| W5053) V4934)))
                    (if (|shen-cl.true?| (|cons?| W5055))
                        (let ((W5056 (|hd| W5055)))
                          (let ((W5057 (|shen.lazyderef| (|tl| W5055) V4934)))
                            (if (|shen-cl.true?| (|cons?| W5057))
                                (let ((W5058 (|hd| W5057)))
                                  (let ((W5059 (|shen.lazyderef| (|tl| W5057) V4934)))
                                    (if (null W5059)
                                        (let ((W5060 (|shen.lazyderef| V4932 V4934)))
                                          (let ((W5061 (|lambda| Z5062
                                                         (|lambda| Z5063
                                                           (let ((W5064 (|shen.newpv| V4934)))
                                                             (|shen.gc| V4934
                                                              (let ((W5065 (|shen.newpv| V4934)))
                                                                (|shen.gc| V4934
                                                                 (progn (|shen.incinfs|)
                                                                        (|bind| W5065 (|shen.freshterm| (|shen.lazyderef| W5056 V4934)) V4934 V4935 W4938
                                                                         (|freeze| (|bind| W5064 (|shen.beta| (|shen.lazyderef| W5056 V4934)
                                                                                                              (|shen.deref| W5065 V4934)
                                                                                                              (|shen.deref| W5058 V4934))
                                                                                           V4934 V4935 W4938
                                                                                           (|freeze| (|shen.system-S-h| W5064 Z5063
                                                                                                         (|cons| (|cons| W5065 (|cons| '|:| (|cons| Z5062 '()))) V4933)
                                                                                                         V4934 V4935 W4938 V4937)))))))))))))))
                                            (if (|shen-cl.true?| (|cons?| W5060))
                                                (let ((W5066 (|hd| W5060)))
                                                  (let ((W5067 (|shen.lazyderef| (|tl| W5060) V4934)))
                                                    (let ((W5068 (|lambda| Z5069 (funcall (funcall W5061 W5066) Z5069))))
                                                      (if (|shen-cl.true?| (|cons?| W5067))
                                                          (let ((W5070 (|shen.lazyderef| (|hd| W5067) V4934)))
                                                            (let ((W5071 (|freeze| (let ((W5072 (|shen.lazyderef| (|tl| W5067) V4934)))
                                                                                     (let ((W5073 (|lambda| Z5074 (funcall W5068 Z5074))))
                                                                                       (if (|shen-cl.true?| (|cons?| W5072))
                                                                                           (let ((W5075 (|hd| W5072)))
                                                                                             (let ((W5076 (|shen.lazyderef| (|tl| W5072) V4934)))
                                                                                               (let ((W5077 (|freeze| (funcall W5073 W5075))))
                                                                                                 (if (null W5076)
                                                                                                     (|thaw| W5077)
                                                                                                     (if (|shen-cl.true?| (|shen.pvar?| W5076))
                                                                                                         (|shen.bind!| W5076 '() V4934 W5077)
                                                                                                         '|false|)))))
                                                                                           (if (|shen-cl.true?| (|shen.pvar?| W5072))
                                                                                               (let ((W5078 (|shen.newpv| V4934)))
                                                                                                 (|shen.gc| V4934 (|shen.bind!| W5072 (|cons| W5078 '()) V4934 (|freeze| (funcall W5073 W5078)))))
                                                                                               '|false|)))))))
                                                              (if (eq W5070 '-->)
                                                                  (|thaw| W5071)
                                                                  (if (|shen-cl.true?| (|shen.pvar?| W5070))
                                                                      (|shen.bind!| W5070 '--> V4934 W5071)
                                                                      '|false|))))
                                                      (if (|shen-cl.true?| (|shen.pvar?| W5067))
                                                          (let ((W5079 (|shen.newpv| V4934)))
                                                            (|shen.gc| V4934 (|shen.bind!| W5067 (|cons| '--> (|cons| W5079 '())) V4934 (|freeze| (funcall W5068 W5079)))))
                                                          '|false|))))
                                                (if (|shen-cl.true?| (|shen.pvar?| W5060))
                                                    (let ((W5080 (|shen.newpv| V4934)))
                                                      (|shen.gc| V4934
                                                                (let ((W5081 (|shen.newpv| V4934)))
                                                                  (|shen.gc| V4934
                                                                            (|shen.bind!| W5060 (|cons| W5080 (|cons| '--> (|cons| W5081 '()))) V4934
                                                                                         (|freeze| (funcall (funcall W5061 W5080) W5081)))))))
                                                    '|false|)))
                                        '|false|)))
                                '|false|)))
                        '|false|)))
            '|false|)))
      '|false|)))

#+sbcl
(defun |shen-cl.system-S-h-W5082| (V4931 V4932 V4933 V4934 V4935 W4938 V4937)
  (if (|shen-cl.true?| (|shen.unlocked?| V4935))
      (let ((W5083 (|shen.lazyderef| V4931 V4934)))
        (if (|shen-cl.true?| (|cons?| W5083))
            (let ((W5084 (|shen.lazyderef| (|hd| W5083) V4934)))
              (if (|shen-cl.true?| (|shen-cl.equal?| W5084 '|let|))
                  (let ((W5085 (|shen.lazyderef| (|tl| W5083) V4934)))
                    (if (|shen-cl.true?| (|cons?| W5085))
                        (let ((W5086 (|hd| W5085)))
                          (let ((W5087 (|shen.lazyderef| (|tl| W5085) V4934)))
                            (if (|shen-cl.true?| (|cons?| W5087))
                                (let ((W5088 (|hd| W5087)))
                                  (let ((W5089 (|shen.lazyderef| (|tl| W5087) V4934)))
                                    (if (|shen-cl.true?| (|cons?| W5089))
                                        (let ((W5090 (|hd| W5089)))
                                          (let ((W5091 (|shen.lazyderef| (|tl| W5089) V4934)))
                                            (if (null W5091)
                                                (let ((W5092 (|shen.newpv| V4934)))
                                                  (|shen.gc| V4934
                                                   (let ((W5093 (|shen.newpv| V4934)))
                                                     (|shen.gc| V4934
                                                      (let ((W5094 (|shen.newpv| V4934)))
                                                        (|shen.gc| V4934
                                                         (progn (|shen.incinfs|)
                                                                (|shen.system-S-h| W5088 W5094 V4933 V4934 V4935 W4938
                                                                 (|freeze| (|bind| W5093 (|shen.freshterm| (|shen.lazyderef| W5086 V4934)) V4934 V4935 W4938
                                                                           (|freeze| (|bind| W5092 (|shen.beta| (|shen.lazyderef| W5086 V4934)
                                                                                                                (|shen.lazyderef| W5093 V4934)
                                                                                                                (|shen.lazyderef| W5090 V4934))
                                                                                             V4934 V4935 W4938
                                                                                             (|freeze| (|shen.system-S-h| W5092 V4932
                                                                                                           (|cons| (|cons| W5093 (|cons| '|:| (|cons| W5094 '()))) V4933)
                                                                                                           V4934 V4935 W4938 V4937))))))))))))))
                                                '|false|)))
                                        '|false|))))
                                '|false|)))
                        '|false|))
                  '|false|))
            '|false|))
      '|false|)

#+sbcl
(defun |shen-cl.system-S-h-W5095| (V4931 V4932 V4933 V4934 V4935 W4938 V4937)
  (if (|shen-cl.true?| (|shen.unlocked?| V4935))
      (let ((W5096 (|shen.lazyderef| V4931 V4934)))
        (if (|shen-cl.true?| (|cons?| W5096))
            (let ((W5097 (|shen.lazyderef| (|hd| W5096) V4934)))
              (if (|shen-cl.true?| (|shen-cl.equal?| W5097 '|open|))
                  (let ((W5098 (|shen.lazyderef| (|tl| W5096) V4934)))
                    (if (|shen-cl.true?| (|cons?| W5098))
                        (let ((W5099 (|hd| W5098)))
                          (let ((W5100 (|shen.lazyderef| (|tl| W5098) V4934)))
                            (if (|shen-cl.true?| (|cons?| W5100))
                                (let ((W5101 (|hd| W5100)))
                                  (let ((W5102 (|shen.lazyderef| (|tl| W5100) V4934)))
                                    (if (null W5102)
                                        (let ((W5103 (|shen.lazyderef| V4932 V4934)))
                                          (let ((W5104 (|lambda| Z5105
                                                         (progn (|shen.incinfs|)
                                                                (|is!| W5101 Z5105 V4934 V4935 W4938
                                                                 (|freeze| (|when| (|element?| (|shen.lazyderef| Z5105 V4934)
                                                                                  (|cons| '|in| (|cons| '|out| '())))
                                                                                   V4934 V4935 W4938
                                                                                   (|freeze| (|shen.system-S-h| W5099 '|string| V4933 V4934 V4935 W4938 V4937)))))))))
                                            (if (|shen-cl.true?| (|cons?| W5103))
                                                (let ((W5106 (|shen.lazyderef| (|hd| W5103) V4934)))
                                                  (let ((W5107 (|freeze| (let ((W5108 (|shen.lazyderef| (|tl| W5103) V4934)))
                                                                           (let ((W5109 (|lambda| Z5110 (funcall W5104 Z5110))))
                                                                             (if (|shen-cl.true?| (|cons?| W5108))
                                                                                 (let ((W5111 (|hd| W5108)))
                                                                                   (let ((W5112 (|shen.lazyderef| (|tl| W5108) V4934)))
                                                                                     (let ((W5113 (|freeze| (funcall W5109 W5111))))
                                                                                       (if (null W5112)
                                                                                           (|thaw| W5113)
                                                                                           (if (|shen-cl.true?| (|shen.pvar?| W5112))
                                                                                               (|shen.bind!| W5112 '() V4934 W5113)
                                                                                               '|false|)))))
                                                                                 (if (|shen-cl.true?| (|shen.pvar?| W5108))
                                                                                     (let ((W5114 (|shen.newpv| V4934)))
                                                                                       (|shen.gc| V4934 (|shen.bind!| W5108 (|cons| W5114 '()) V4934 (|freeze| (funcall W5109 W5114)))))
                                                                                     '|false|)))))))
                                                    (if (eq W5106 '|stream|)
                                                        (|thaw| W5107)
                                                        (if (|shen-cl.true?| (|shen.pvar?| W5106))
                                                            (|shen.bind!| W5106 '|stream| V4934 W5107)
                                                            '|false|))))
                                                (if (|shen-cl.true?| (|shen.pvar?| W5103))
                                                    (let ((W5115 (|shen.newpv| V4934)))
                                                      (|shen.gc| V4934 (|shen.bind!| W5103 (|cons| '|stream| (|cons| W5115 '())) V4934 (|freeze| (funcall W5104 W5115)))))
                                                    '|false|))))
                                        '|false|))))
                                '|false|)))
                        '|false|))
                  '|false|))
            '|false|))
      '|false|)

#+sbcl
(defun |shen-cl.system-S-h-group-a| (V4931 V4932 V4933 env V4935 W4938 V4937)
  (first-non-fail
    (list
     (safe-future
       (let ((V4934 (copy-env-for-fork env)))
         (if (|shen-cl.true?| (|shen.unlocked?| V4935))
             (progn (|shen.incinfs|)
                    (|when| (|not| (|cons?| (|shen.lazyderef| V4931 V4934)))
                            V4934 V4935 W4938
                            (|freeze| (|shen.primitive| V4931 V4932 V4934 V4935 W4938 V4937))))
             '|false|)))
     (safe-future
       (let ((V4934 (copy-env-for-fork env)))
         (if (|shen-cl.true?| (|shen.unlocked?| V4935))
             (progn (|shen.incinfs|)
                    (|shen.by-hypothesis| V4931 V4932 V4933 V4934 V4935 W4938 V4937))
             '|false|)))
     (safe-future
       (let ((V4934 (copy-env-for-fork env)))
         (if (|shen-cl.true?| (|shen.unlocked?| V4935))
             (let ((W4943 (|shen.lazyderef| V4931 V4934)))
               (if (|shen-cl.true?| (|cons?| W4943))
                   (let ((W4944 (|hd| W4943)))
                     (let ((W4945 (|shen.lazyderef| (|tl| W4943) V4934)))
                       (if (null W4945)
                           (progn (|shen.incinfs|)
                                  (|shen.lookupsig| W4944 (|cons| '--> (|cons| V4932 '())) V4934 V4935 W4938 V4937))
                           '|false|)))
                   '|false|))
             '|false|))))))

#+sbcl
(defun |shen-cl.system-S-h-group-b| (V4931 V4932 V4933 env V4935 W4938 V4937)
  (first-non-fail
    (list
     (safe-future (let ((V4934 (copy-env-for-fork env))) (|shen-cl.system-S-h-W4952| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
     (safe-future (let ((V4934 (copy-env-for-fork env))) (|shen-cl.system-S-h-W4958| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
     (safe-future (let ((V4934 (copy-env-for-fork env))) (|shen-cl.system-S-h-W4965| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
     (safe-future (let ((V4934 (copy-env-for-fork env))) (|shen-cl.system-S-h-W4972| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
     (safe-future (let ((V4934 (copy-env-for-fork env))) (|shen-cl.system-S-h-W4993| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
     (safe-future (let ((V4934 (copy-env-for-fork env))) (|shen-cl.system-S-h-W5021| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
     (safe-future (let ((V4934 (copy-env-for-fork env))) (|shen-cl.system-S-h-W5042| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
     (safe-future (let ((V4934 (copy-env-for-fork env))) (|shen-cl.system-S-h-W5052| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
     (safe-future (let ((V4934 (copy-env-for-fork env))) (|shen-cl.system-S-h-W5082| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
     (safe-future (let ((V4934 (copy-env-for-fork env))) (|shen-cl.system-S-h-W5095| V4931 V4932 V4933 V4934 V4935 W4938 V4937))))))

#+sbcl
(defun |shen-cl.system-S-h-group-c| (V4931 V4932 V4933 env V4935 W4938 V4937)
  (first-non-fail
    (list
     (safe-future
       (let ((V4934 (copy-env-for-fork env)))
         (if (|shen-cl.true?| (|shen.unlocked?| V4935))
             (let ((W5125 (|shen.lazyderef| V4931 V4934)))
               (if (|shen-cl.true?| (|cons?| W5125))
                   (let ((W5126 (|shen.lazyderef| (|hd| W5125) V4934)))
                     (if (|shen-cl.true?| (|shen-cl.equal?| W5126 '|input+|))
                         (let ((W5127 (|shen.lazyderef| (|tl| W5125) V4934)))
                           (if (|shen-cl.true?| (|cons?| W5127))
                               (let ((W5128 (|hd| W5127)))
                                 (let ((W5129 (|shen.lazyderef| (|tl| W5127) V4934)))
                                   (if (|shen-cl.true?| (|cons?| W5129))
                                       (let ((W5130 (|hd| W5129)))
                                         (let ((W5131 (|shen.lazyderef| (|tl| W5129) V4934)))
                                           (if (null W5131)
                                               (progn (|shen.incinfs|)
                                                      (|is!| V4932 (|shen.rectify-type| (|shen.deref| W5128 V4934)) V4934 V4935 W4938
                                                       (|freeze| (|shen.system-S-h| W5130 (|cons| '|stream| (|cons| '|in| '())) V4933 V4934 V4935 W4938 V4937))))
                                               '|false|)))
                                       '|false|)))
                               '|false|))
                         '|false|))
                   '|false|))
             '|false|)))
     (safe-future
       (let ((V4934 (copy-env-for-fork env)))
         (if (|shen-cl.true?| (|shen.unlocked?| V4935))
             (let ((W5133 (|shen.lazyderef| V4931 V4934)))
               (if (|shen-cl.true?| (|cons?| W5133))
                   (let ((W5134 (|shen.lazyderef| (|hd| W5133) V4934)))
                     (if (|shen-cl.true?| (|shen-cl.equal?| W5134 '|set|))
                         (let ((W5135 (|shen.lazyderef| (|tl| W5133) V4934)))
                           (if (|shen-cl.true?| (|cons?| W5135))
                               (let ((W5136 (|hd| W5135)))
                                 (let ((W5137 (|shen.lazyderef| (|tl| W5135) V4934)))
                                   (if (|shen-cl.true?| (|cons?| W5137))
                                       (let ((W5138 (|hd| W5137)))
                                         (let ((W5139 (|shen.lazyderef| (|tl| W5137) V4934)))
                                           (if (null W5139)
                                               (progn (|shen.incinfs|)
                                                      (|shen.system-S-h| W5136 '|symbol| V4933 V4934 V4935 W4938
                                                       (|freeze| (|shen.system-S-h| (|cons| '|value| (|cons| W5136 '())) V4932 V4933 V4934 V4935 W4938
                                                                   (|freeze| (|shen.system-S-h| W5138 V4932 V4933 V4934 V4935 W4938 V4937))))))
                                               '|false|)))
                                       '|false|)))
                               '|false|))
                         '|false|))
                   '|false|))
             '|false|)))))) 

#+sbcl
(defun |shen.system-S-h| (V4931 V4932 V4933 V4934 V4935 V4936 V4937)
  (if (|shen-cl.ensure-lparallel-kernel|)
      (let ((W4938 (1+ V4936)))
        (let ((W4939 (if (|shen-cl.true?| (|shen.unlocked?| V4935))
                         (progn (|shen.incinfs|)
                                (|when| |shen.*spy*| V4934 V4935 W4938
                                  (|freeze| (|shen.show| (list V4931 '|:| V4932)
                                                         V4933 V4934 V4935 W4938 V4937))))
                         '|false|)))
          (if (eq W4939 '|false|)
              (let ((ga (|shen-cl.system-S-h-group-a| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
                (if (eq ga '|false|)
                    (let ((W4946 (if (|shen-cl.true?| (|shen.unlocked?| V4935))
                                     (let ((W4947 (|shen.lazyderef| V4931 V4934)))
                                       (if (|shen-cl.true?| (|cons?| W4947))
                                           (let ((W4948 (|shen.lazyderef| (|hd| W4947) V4934)))
                                             (if (|shen-cl.true?| (|shen-cl.equal?| W4948 '|fn|))
                                                 (let ((W4949 (|shen.lazyderef| (|tl| W4947) V4934)))
                                                   (if (|shen-cl.true?| (|cons?| W4949))
                                                       (let ((W4950 (|hd| W4949)))
                                                         (let ((W4951 (|shen.lazyderef| (|tl| W4949) V4934)))
                                                           (if (null W4951)
                                                               (progn (|shen.incinfs|)
                                                                      (|when| (|shen-cl.equal?| (|arity| (|shen.deref| W4950 V4934)) 0)
                                                                              V4934 V4935 W4938
                                                                              (|freeze| (|shen.cut| V4934 V4935 W4938
                                                                                         (|freeze| (|shen.system-S-h| (|cons| W4950 '()) V4932 V4933 V4934 V4935 W4938 V4937))))))
                                                               '|false|)))
                                                       '|false|))
                                                 '|false|))
                                           '|false|))
                                     '|false|)))
                      (if (eq W4946 '|false|)
                          (let ((gb (|shen-cl.system-S-h-group-b| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
                            (if (eq gb '|false|)
                                (let ((W5116 (if (|shen-cl.true?| (|shen.unlocked?| V4935))
                                                 (let ((W5117 (|shen.lazyderef| V4931 V4934)))
                                                   (if (|shen-cl.true?| (|cons?| W5117))
                                                       (let ((W5118 (|shen.lazyderef| (|hd| W5117) V4934)))
                                                         (if (|shen-cl.true?| (|shen-cl.equal?| W5118 '|type|))
                                                             (let ((W5119 (|shen.lazyderef| (|tl| W5117) V4934)))
                                                               (if (|shen-cl.true?| (|cons?| W5119))
                                                                   (let ((W5120 (|hd| W5119)))
                                                                     (let ((W5121 (|shen.lazyderef| (|tl| W5119) V4934)))
                                                                       (if (|shen-cl.true?| (|cons?| W5121))
                                                                           (let ((W5122 (|hd| W5121)))
                                                                             (let ((W5123 (|shen.lazyderef| (|tl| W5121) V4934)))
                                                                               (if (null W5123)
                                                                                   (progn (|shen.incinfs|)
                                                                                          (|shen.cut| V4934 V4935 W4938
                                                                                            (|freeze| (|is!| (|shen.rectify-type| (|shen.deref| W5122 V4934))
                                                                                                             V4932 V4934 V4935 W4938
                                                                                                             (|freeze| (|shen.system-S-h| W5120 V4932 V4933 V4934 V4935 W4938 V4937))))))
                                                                                   '|false|)))
                                                                           '|false|)))
                                                                   '|false|))
                                                             '|false|))
                                                       '|false|))
                                                 '|false|)))
                                  (if (eq W5116 '|false|)
                                      (let ((gc (|shen-cl.system-S-h-group-c| V4931 V4932 V4933 V4934 V4935 W4938 V4937)))
                                        (if (eq gc '|false|)
                                            (let ((W5140 (if (|shen-cl.true?| (|shen.unlocked?| V4935))
                                                             (let ((W5141 (|shen.newpv| V4934)))
                                                               (|shen.gc| V4934
                                                                (progn (|shen.incinfs|)
                                                                       (|shen.l-rules| V4933 W5141 '|false| V4934 V4935 W4938
                                                                                        (|freeze| (|shen.cut| V4934 V4935 W4938
                                                                                                   (|freeze| (|shen.system-S-h| V4931 V4932 W5141 V4934 V4935 W4938 V4937))))))))
                                                             '|false|)))
                                              (if (eq W5140 '|false|)
                                                  (let ((W5142 (if (|shen-cl.true?| (|shen.unlocked?| V4935))
                                                                   (progn (|shen.incinfs|)
                                                                          (|shen.search-user-datatypes| (list V4931 '|:| V4932) V4933 |shen.*datatypes*| V4934 V4935 W4938 V4937))
                                                                   '|false|)))
                                                    (if (eq W5142 '|false|)
                                                        (|shen.unlock| V4935 W4938)
                                                        W5142))
                                                  W5140))
                                            gc))
                                      W5116))
                                gb))
                          W4946))
                    ga))
              W4939)))
      (funcall |shen-cl.kernel-system-S-h| V4931 V4932 V4933 V4934 V4935 V4936 V4937)))

(defun |vector->| (vector n x)
  (if (zerop n)
      (error "cannot access 0th element of a vector~%")
      (|address->| vector n x)))

(defun |<-vector| (vector n)
  (if (zerop n)
    (error "cannot access 0th element of a vector~%")
    (let ((vector-element (svref vector n)))
      (if (eq vector-element (|fail|))
          (error "vector element not found~%")
          vector-element))))

(defun |variable?| (x)
  (if (and (symbolp x) (not (null x)) (upper-case-p (char (symbol-name x) 0)))
      '|true|
      '|false|))

(defun |shen.+string?| (x)
  (if (and (stringp x) (not (string-equal x "")))
      '|true|
      '|false|))

(defun |thaw| (f)
  (funcall f))

(defun |hash| (val bound)
  (mod (sxhash val) bound))

(defun |shen.dict| (size)
  (make-hash-table :size size))

(defun |shen.dict?| (dict)
  (if (hash-table-p dict) '|true| '|false|))

(defun |shen.dict-count| (dict)
  (hash-table-count dict))

(defun |shen.dict->| (dict key value)
 (setf (gethash key dict) value))

(defun |shen.<-dict| (dict key)
  (multiple-value-bind (result found) (gethash key dict)
    (if found
        result
        (error "value ~A not found in dict~%" key))))

(defun |shen.dict-rm| (dict key)
  (progn (remhash key dict) key))

(defun |shen.dict-fold| (f dict init)
  (let ((acc init))
    (maphash #'(lambda (k v)
                 (setf acc (funcall (funcall (funcall f k) v) acc))) dict)
    acc))

#+clisp
(defun |cl.exit| (code)
  (ext:exit code))

#+(and ccl (not windows))
(defun |cl.exit| (code)
  (ccl:quit code))

#+(and ccl windows)
(ccl::eval (ccl::read-from-string "(defun |cl.exit| (code) (#__exit code))"))

#+ecl
(defun |cl.exit| (code)
  (si:quit code))

#+sbcl
(defun |cl.exit| (code)
  (alien-funcall (extern-alien "exit" (function void int)) code))

(defun |shen-cl.exit| (code)
  (|cl.exit| code))

(defun |shen-cl.initialise| ()
  (progn
    (|shen-cl.initialise-compiler|)

    #+sbcl
    (setf *shen-lparallel-kernel* nil
          lparallel:*kernel* nil)

    (|put|      '|cl.exit| '|arity| 1 |*property-vector*|)
    (|put| '|shen-cl.exit| '|arity| 1 |*property-vector*|)

    (|declare|      '|cl.exit| (list '|number| '--> '|unit|))
    (|declare| '|shen-cl.exit| (list '|number| '--> '|unit|))

    (|shen-cl.read-eval| "(defmacro      cl.exit-macro      [cl.exit] -> [cl.exit 0])")
    (|shen-cl.read-eval| "(defmacro shen-cl.exit-macro [shen-cl.exit] -> [cl.exit 0])")))

#+(or ccl sbcl)
(defun |shen.read-char-code| (s)
  (let ((c (read-char s nil -1)))
    (if (eq c -1)
      -1
      (char-int c))))

#+(or ccl sbcl)
(defun |pr| (x s)
  (write-string x s)
  (when (or (eq s |*stoutput*|) (eq s |*stinput*|))
    (force-output s))
  x)

;; file reading

(defun |read-file-as-bytelist| (path)
  (with-open-file (stream (format nil "~A~A" |*home-directory*| path) :direction :input :element-type 'unsigned-byte)
    (let ((data (make-array (file-length stream) :element-type 'unsigned-byte :initial-element 0)))
      (read-sequence data stream)
      (coerce data 'list))))

(defun |shen.read-file-as-charlist| (path)
  (|read-file-as-bytelist| path))

(defun |shen.read-file-as-string| (path)
  (with-open-file (stream (format nil "~A~A" |*home-directory*| path) :direction :input)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

;; tuples

(defun |@p| (x y)
  (vector '|shen.tuple| x y))

;; vectors

(defun |vector| (n)
  (let ((vec (make-array (1+ n) :initial-element (|fail|))))
    (setf (svref vec 0) n)
    vec))

; Amend the REPL credits message to explain exit command
(setf (symbol-function '|shen-cl.original-credits|) #'|shen.credits|)

(defun |shen.credits| ()
  (|shen-cl.original-credits|)
  (format t "exit REPL with (cl.exit)"))

;; Compiler functions

(defun |shen-cl.cl| (symbol)
  (let* ((str (symbol-name symbol))
         (lispname (string-upcase str)))
    (|intern| lispname)))

(defun |shen-cl.lisp-prefixed?| (symbol)
  (|shen-cl.lisp-true?|
    (and (not (null symbol))
         (symbolp symbol)
         (|shen-cl.prefix?| (symbol-name symbol) "lisp."))))

(defun |shen-cl.remove-lisp-prefix| (symbol)
  (|intern| (subseq (symbol-name symbol) 5)))
