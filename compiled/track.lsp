(in-package :shen)

(DEFUN |shen.f-error| (V5766) (PROGN (|pr| (|cn| "partial function " (|shen.app| V5766 ";
" '|shen.a|)) (|stoutput|)) (IF (AND (NOT (|shen-cl.true?| (|shen.tracked?| V5766))) (|shen-cl.true?| (|y-or-n?| (|cn| "track " (|shen.app| V5766 "? " '|shen.a|))))) (|shen.track-function| (|ps| V5766)) '|shen.ok|) (|simple-error| "aborted")))

(DEFUN |shen.tracked?| (V5767) (|element?| V5767 |shen.*tracking*|))

(DEFUN |track| (V5768) (LET ((W5769 (|ps| V5768))) (|shen.track-function| W5769)))

(DEFUN |shen.track-function| (V5772) (BLOCK () (TAGBODY (IF (CONSP V5772) (LET ((|V5772/tl| (|tl| V5772))) (IF (AND (EQ (|hd| V5772) '|defun|) (CONSP |V5772/tl|)) (|LET*| ((|V5772/tl/hd| (|hd| |V5772/tl|)) (|V5772/tl/tl| (|tl| |V5772/tl|))) (IF (CONSP |V5772/tl/tl|) (|LET*| ((|V5772/tl/tl/hd| (|hd| |V5772/tl/tl|)) (|V5772/tl/tl/tl| (|tl| |V5772/tl/tl|))) (IF (AND (CONSP |V5772/tl/tl/tl|) (NULL (|tl| |V5772/tl/tl/tl|))) (RETURN (|LET*| ((W5773 (|cons| '|defun| (|cons| |V5772/tl/hd| (|cons| |V5772/tl/tl/hd| (|cons| (|shen.insert-tracking-code| |V5772/tl/hd| |V5772/tl/tl/hd| (|hd| |V5772/tl/tl/tl|)) '()))))) (W5774 (|eval-kl| W5773)) (W5775 (|set| '|shen.*tracking*| (|adjoin| |V5772/tl/hd| |shen.*tracking*|)))) |V5772/tl/hd|)) (GO |%%label373|))) (GO |%%label373|))) (GO |%%label373|))) (GO |%%label373|)) |%%label373| (RETURN (|simple-error| "implementation error in shen.track-function")))))

(DEFUN |shen.insert-tracking-code| (V5776 V5777 V5778) (|cons| '|do| (|cons| (|cons| '|set| (|cons| '|shen.*call*| (|cons| (|cons| '|+| (|cons| (|cons| '|value| (|cons| '|shen.*call*| '())) (|cons| 1 '()))) '()))) (|cons| (|cons| '|do| (|cons| (|cons| '|shen.input-track| (|cons| (|cons| '|value| (|cons| '|shen.*call*| '())) (|cons| V5776 (|cons| (|shen.cons-form| (|shen.prolog-track| V5778 V5777)) '())))) (|cons| (|cons| '|do| (|cons| (|cons| '|shen.terpri-or-read-char| '()) (|cons| (|cons| '|let| (|cons| '|Result| (|cons| V5778 (|cons| (|cons| '|do| (|cons| (|cons| '|shen.output-track| (|cons| (|cons| '|value| (|cons| '|shen.*call*| '())) (|cons| V5776 (|cons| '|Result| '())))) (|cons| (|cons| '|do| (|cons| (|cons| '|set| (|cons| '|shen.*call*| (|cons| (|cons| '|-| (|cons| (|cons| '|value| (|cons| '|shen.*call*| '())) (|cons| 1 '()))) '()))) (|cons| (|cons| '|do| (|cons| (|cons| '|shen.terpri-or-read-char| '()) (|cons| '|Result| '()))) '()))) '()))) '())))) '()))) '()))) '()))))

(DEFUN |shen.prolog-track| (V5779 V5780) (BLOCK () (IF (EQL (|occurrences| '|shen.incinfs| V5779) 0) (RETURN V5780) (RETURN (|shen.vector-dereference| V5780 (|shen.vector-parameter| V5780))))))

(DEFUN |shen.vector-parameter| (V5783) (BLOCK () (IF (NULL V5783) (RETURN '()) (IF (CONSP V5783) (LET ((|V5783/tl| (|tl| V5783))) (TAGBODY (IF (CONSP |V5783/tl|) (LET ((|V5783/tl/tl| (|tl| |V5783/tl|))) (IF (CONSP |V5783/tl/tl|) (LET ((|V5783/tl/tl/tl| (|tl| |V5783/tl/tl|))) (IF (AND (CONSP |V5783/tl/tl/tl|) (NULL (|tl| |V5783/tl/tl/tl|))) (RETURN (|hd| V5783)) (GO |%%label376|))) (GO |%%label376|))) (GO |%%label376|)) |%%label376| (RETURN (|shen.vector-parameter| |V5783/tl|)))) (RETURN (|shen.f-error| '|shen.vector-parameter|))))))

(DEFUN |shen.vector-dereference| (V5786 V5787) (BLOCK () (IF (NULL V5787) (RETURN V5786) (IF (CONSP V5786) (LET ((|V5786/tl| (|tl| V5786))) (TAGBODY (IF (CONSP |V5786/tl|) (LET ((|V5786/tl/tl| (|tl| |V5786/tl|))) (IF (CONSP |V5786/tl/tl|) (LET ((|V5786/tl/tl/tl| (|tl| |V5786/tl/tl|))) (IF (AND (CONSP |V5786/tl/tl/tl|) (NULL (|tl| |V5786/tl/tl/tl|))) (RETURN V5786) (GO |%%label379|))) (GO |%%label379|))) (GO |%%label379|)) |%%label379| (RETURN (|cons| (|cons| '|shen.deref| (|cons| (|hd| V5786) (|cons| V5787 '()))) (|shen.vector-dereference| |V5786/tl| V5787))))) (RETURN (|shen.f-error| '|shen.vector-dereference|))))))

(DEFUN |step| (V5790) (BLOCK () (IF (EQ V5790 '|+|) (RETURN (|set| '|shen.*step*| '|true|)) (IF (EQ V5790 '|-|) (RETURN (|set| '|shen.*step*| '|false|)) (RETURN (|simple-error| "step expects a + or a -.
"))))))

(DEFUN |shen.step?| () |shen.*step*|)

(DEFUN |spy| (V5793) (BLOCK () (IF (EQ V5793 '|+|) (RETURN (|set| '|shen.*spy*| '|true|)) (IF (EQ V5793 '|-|) (RETURN (|set| '|shen.*spy*| '|false|)) (RETURN (|simple-error| "spy expects a + or a -.
"))))))

(DEFUN |shen.spy?| () |shen.*spy*|)

(DEFUN |shen.terpri-or-read-char| () (IF (|shen-cl.true?| |shen.*step*|) (|shen.check-byte| (|read-byte| |*stinput*|)) (|nl| 1)))

(DEFUN |shen.check-byte| (V5796) (BLOCK () (IF (EQL V5796 94) (RETURN (|simple-error| "aborted")) (RETURN '|true|))))

(DEFUN |shen.input-track| (V5797 V5798 V5799) (PROGN (|pr| (|cn| "
" (|shen.app| (|shen.spaces| V5797) (|cn| "<" (|shen.app| V5797 (|cn| "> Inputs to " (|shen.app| V5798 (|cn| " 
" (|shen.app| (|shen.spaces| V5797) "" '|shen.a|)) '|shen.a|)) '|shen.a|)) '|shen.a|)) (|stoutput|)) (|shen.recursively-print| V5799)))

(DEFUN |shen.recursively-print| (V5802) (BLOCK () (IF (NULL V5802) (RETURN (|pr| " ==>" (|stoutput|))) (IF (CONSP V5802) (RETURN (PROGN (|print| (|hd| V5802)) (|pr| ", " (|stoutput|)) (|shen.recursively-print| (|tl| V5802)))) (RETURN (|simple-error| "implementation error in shen.recursively-print"))))))

(DEFUN |shen.spaces| (V5803) (BLOCK () (IF (EQL V5803 0) (RETURN "") (RETURN (|cn| " " (|shen.spaces| (|1-| V5803)))))))

(DEFUN |shen.output-track| (V5804 V5805 V5806) (|pr| (|cn| "
" (|shen.app| (|shen.spaces| V5804) (|cn| "<" (|shen.app| V5804 (|cn| "> Output from " (|shen.app| V5805 (|cn| " 
" (|shen.app| (|shen.spaces| V5804) (|cn| "==> " (|shen.app| V5806 "" '|shen.s|)) '|shen.a|)) '|shen.a|)) '|shen.a|)) '|shen.a|)) (|stoutput|)))

(DEFUN |untrack| (V5807) (PROGN (|set| '|shen.*tracking*| (|remove| V5807 |shen.*tracking*|)) (|trap-error| (|eval| (|ps| V5807)) (|lambda| Z5808 V5807)) V5807))

(DEFUN |remove| (V5809 V5810) (|shen.remove-h| V5809 V5810 '()))

(DEFUN |shen.remove-h| (V5820 V5821 V5822) (BLOCK () (IF (NULL V5821) (RETURN (|reverse| V5822)) (IF (CONSP V5821) (|LET*| ((|V5821/hd| (|hd| V5821)) (|V5821/tl| (|tl| V5821))) (IF (|shen-cl.absequal| V5820 |V5821/hd|) (RETURN (|shen.remove-h| |V5821/hd| |V5821/tl| V5822)) (RETURN (|shen.remove-h| V5820 |V5821/tl| (|cons| |V5821/hd| V5822))))) (RETURN (|simple-error| "implementation error in shen.remove-h"))))))

(DEFUN |profile| (V5823) (PROGN (|set| '|shen.*profiled*| (|cons| V5823 |shen.*profiled*|)) (|shen.profile-help| (|ps| V5823))))

(DEFUN |shen.profile-help| (V5826) (BLOCK () (TAGBODY (IF (CONSP V5826) (LET ((|V5826/tl| (|tl| V5826))) (IF (AND (EQ (|hd| V5826) '|defun|) (CONSP |V5826/tl|)) (|LET*| ((|V5826/tl/hd| (|hd| |V5826/tl|)) (|V5826/tl/tl| (|tl| |V5826/tl|))) (IF (CONSP |V5826/tl/tl|) (|LET*| ((|V5826/tl/tl/hd| (|hd| |V5826/tl/tl|)) (|V5826/tl/tl/tl| (|tl| |V5826/tl/tl|))) (IF (AND (CONSP |V5826/tl/tl/tl|) (NULL (|tl| |V5826/tl/tl/tl|))) (RETURN (|LET*| ((W5827 (|gensym| '|shen.f|)) (W5828 (|cons| '|defun| (|cons| |V5826/tl/hd| (|cons| |V5826/tl/tl/hd| (|cons| (|shen.profile-func| |V5826/tl/hd| |V5826/tl/tl/hd| (|cons| W5827 |V5826/tl/tl/hd|)) '()))))) (W5829 (|cons| '|defun| (|cons| W5827 (|cons| |V5826/tl/tl/hd| (|cons| (|subst| W5827 |V5826/tl/hd| (|hd| |V5826/tl/tl/tl|)) '()))))) (W5830 (|eval-kl| W5828)) (W5831 (|eval-kl| W5829))) |V5826/tl/hd|)) (GO |%%label391|))) (GO |%%label391|))) (GO |%%label391|))) (GO |%%label391|)) |%%label391| (RETURN (|simple-error| "Cannot profile.
")))))

(DEFUN |unprofile| (V5832) (PROGN (|set| '|shen.*profiled*| (|remove| V5832 |shen.*profiled*|)) (|trap-error| (|eval| (|ps| V5832)) (|lambda| Z5833 V5832))))

(DEFUN |shen.profiled?| (V5834) (|element?| V5834 |shen.*profiled*|))

(DEFUN |shen.profile-func| (V5835 V5836 V5837) (|cons| '|let| (|cons| '|Start| (|cons| (|cons| '|get-time| (|cons| '|run| '())) (|cons| (|cons| '|let| (|cons| '|Result| (|cons| V5837 (|cons| (|cons| '|let| (|cons| '|Finish| (|cons| (|cons| '|-| (|cons| (|cons| '|get-time| (|cons| '|run| '())) (|cons| '|Start| '()))) (|cons| (|cons| '|let| (|cons| '|Record| (|cons| (|cons| '|shen.put-profile| (|cons| V5835 (|cons| (|cons| '|+| (|cons| (|cons| '|shen.get-profile| (|cons| V5835 '())) (|cons| '|Finish| '()))) '()))) (|cons| '|Result| '())))) '())))) '())))) '())))))

(DEFUN |profile-results| (V5838) (|LET*| ((W5839 (|shen.get-profile| V5838)) (W5840 (|shen.put-profile| V5838 0))) (|@p| V5838 W5839)))

(DEFUN |shen.get-profile| (V5841) (|trap-error| (|get| V5841 '|profile| |*property-vector*|) (|lambda| Z5842 0)))

(DEFUN |shen.put-profile| (V5843 V5844) (|put| V5843 '|profile| V5844 |*property-vector*|))

