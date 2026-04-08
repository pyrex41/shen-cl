(in-package :shen)

(DEFUN |print| (V6779) (|LET*| ((W6780 (|shen.insert| V6779 "~S")) (W6781 (|pr| W6780 (|stoutput|)))) V6779))

(DEFUN |pr| (V6782 V6783) (IF (|shen-cl.true?| |*hush*|) V6782 (IF (|shen-cl.true?| (|shen.char-stoutput?| V6783)) (|shen.write-string| V6782 V6783) (|shen.write-chars| V6782 V6783 (|shen.string->byte| V6782 0) 1))))

(DEFUN |shen.string->byte| (V6784 V6785) (|trap-error| (|string->n| (|pos| V6784 V6785)) (|lambda| Z6786 '|shen.eos|)))

(DEFUN |shen.write-chars| (V6787 V6788 V6789 V6790) (BLOCK () (IF (EQ V6789 '|shen.eos|) (RETURN V6787) (RETURN (|shen.write-chars| V6787 V6788 (PROGN (|write-byte| V6789 V6788) (|shen.string->byte| V6787 V6790)) (|1+| V6790))))))

(DEFUN |shen.mkstr| (V6791 V6792) (BLOCK () (IF (STRINGP V6791) (RETURN (|shen.mkstr-l| (|shen.proc-nl| V6791) V6792)) (RETURN (|shen.mkstr-r| (|cons| '|shen.proc-nl| (|cons| V6791 '())) V6792)))))

(DEFUN |shen.mkstr-l| (V6797 V6798) (BLOCK () (IF (NULL V6798) (RETURN V6797) (IF (CONSP V6798) (RETURN (|shen.mkstr-l| (|shen.insert-l| (|hd| V6798) V6797) (|tl| V6798))) (RETURN (|simple-error| "implementation error in shen.mkstr-l"))))))

(DEFUN |shen.insert-l| (V6805 V6806) (BLOCK () (IF (EQUAL "" V6806) (RETURN "") (IF (|shen-cl.true?| (|shen.+string?| V6806)) (TAGBODY (IF (AND (EQUAL "~" (|hdstr| V6806)) (|shen-cl.true?| (|shen.+string?| (|tlstr| V6806)))) (IF (EQUAL "A" (|hdstr| (|tlstr| V6806))) (RETURN (|cons| '|shen.app| (|cons| V6805 (|cons| (|tlstr| (|tlstr| V6806)) (|cons| '|shen.a| '()))))) (IF (EQUAL "R" (|hdstr| (|tlstr| V6806))) (RETURN (|cons| '|shen.app| (|cons| V6805 (|cons| (|tlstr| (|tlstr| V6806)) (|cons| '|shen.r| '()))))) (IF (EQUAL "S" (|hdstr| (|tlstr| V6806))) (RETURN (|cons| '|shen.app| (|cons| V6805 (|cons| (|tlstr| (|tlstr| V6806)) (|cons| '|shen.s| '()))))) (GO |%%label418|)))) (GO |%%label418|)) |%%label418| (RETURN (|shen.factor-cn| (|cons| '|cn| (|cons| (|hdstr| V6806) (|cons| (|shen.insert-l| V6805 (|tlstr| V6806)) '())))))) (TAGBODY (IF (CONSP V6806) (|LET*| ((|V6806/hd| (|hd| V6806)) (|V6806/tl| (|tl| V6806))) (TAGBODY (IF (AND (EQ |V6806/hd| '|cn|) (CONSP |V6806/tl|)) (LET ((|V6806/tl/tl| (|tl| |V6806/tl|))) (IF (AND (CONSP |V6806/tl/tl|) (NULL (|tl| |V6806/tl/tl|))) (RETURN (|cons| '|cn| (|cons| (|hd| |V6806/tl|) (|cons| (|shen.insert-l| V6805 (|hd| |V6806/tl/tl|)) '())))) (GO |%%label416|))) (GO |%%label416|)) |%%label416| (IF (AND (EQ |V6806/hd| '|shen.app|) (CONSP |V6806/tl|)) (LET ((|V6806/tl/tl| (|tl| |V6806/tl|))) (IF (CONSP |V6806/tl/tl|) (LET ((|V6806/tl/tl/tl| (|tl| |V6806/tl/tl|))) (IF (AND (CONSP |V6806/tl/tl/tl|) (NULL (|tl| |V6806/tl/tl/tl|))) (RETURN (|cons| '|shen.app| (|cons| (|hd| |V6806/tl|) (|cons| (|shen.insert-l| V6805 (|hd| |V6806/tl/tl|)) |V6806/tl/tl/tl|)))) (GO |%%label415|))) (GO |%%label415|))) (GO |%%label415|)))) (GO |%%label415|)) |%%label415| (RETURN (|simple-error| "implementation error in shen.insert-l")))))))

(DEFUN |shen.factor-cn| (V6807) (BLOCK () (IF (CONSP V6807) (LET ((|V6807/tl| (|tl| V6807))) (IF (AND (EQ (|hd| V6807) '|cn|) (CONSP |V6807/tl|)) (|LET*| ((|V6807/tl/hd| (|hd| |V6807/tl|)) (|V6807/tl/tl| (|tl| |V6807/tl|))) (IF (CONSP |V6807/tl/tl|) (LET ((|V6807/tl/tl/hd| (|hd| |V6807/tl/tl|))) (IF (CONSP |V6807/tl/tl/hd|) (LET ((|V6807/tl/tl/hd/tl| (|tl| |V6807/tl/tl/hd|))) (IF (AND (EQ (|hd| |V6807/tl/tl/hd|) '|cn|) (CONSP |V6807/tl/tl/hd/tl|)) (|LET*| ((|V6807/tl/tl/hd/tl/hd| (|hd| |V6807/tl/tl/hd/tl|)) (|V6807/tl/tl/hd/tl/tl| (|tl| |V6807/tl/tl/hd/tl|))) (IF (AND (CONSP |V6807/tl/tl/hd/tl/tl|) (AND (NULL (|tl| |V6807/tl/tl/hd/tl/tl|)) (AND (NULL (|tl| |V6807/tl/tl|)) (AND (STRINGP |V6807/tl/hd|) (STRINGP |V6807/tl/tl/hd/tl/hd|))))) (RETURN (|cons| '|cn| (|cons| (|cn| |V6807/tl/hd| |V6807/tl/tl/hd/tl/hd|) |V6807/tl/tl/hd/tl/tl|))) (RETURN V6807))) (RETURN V6807))) (RETURN V6807))) (RETURN V6807))) (RETURN V6807))) (RETURN V6807))))

(DEFUN |shen.proc-nl| (V6810) (BLOCK () (IF (EQUAL "" V6810) (RETURN "") (IF (|shen-cl.true?| (|shen.+string?| V6810)) (IF (AND (EQUAL "~" (|hdstr| V6810)) (AND (|shen-cl.true?| (|shen.+string?| (|tlstr| V6810))) (EQUAL "%" (|hdstr| (|tlstr| V6810))))) (RETURN (|cn| (|n->string| 10) (|shen.proc-nl| (|tlstr| (|tlstr| V6810))))) (RETURN (|cn| (|hdstr| V6810) (|shen.proc-nl| (|tlstr| V6810))))) (RETURN (|simple-error| "implementation error in shen.proc-nl"))))))

(DEFUN |shen.mkstr-r| (V6815 V6816) (BLOCK () (IF (NULL V6816) (RETURN V6815) (IF (CONSP V6816) (RETURN (|shen.mkstr-r| (|cons| '|shen.insert| (|cons| (|hd| V6816) (|cons| V6815 '()))) (|tl| V6816))) (RETURN (|simple-error| "implementation error in shen.mkstr-r"))))))

(DEFUN |shen.insert| (V6817 V6818) (|shen.insert-h| V6817 V6818 ""))

(DEFUN |shen.insert-h| (V6827 V6828 V6829) (BLOCK () (IF (EQUAL "" V6828) (RETURN V6829) (IF (|shen-cl.true?| (|shen.+string?| V6828)) (TAGBODY (IF (AND (EQUAL "~" (|hdstr| V6828)) (|shen-cl.true?| (|shen.+string?| (|tlstr| V6828)))) (IF (EQUAL "A" (|hdstr| (|tlstr| V6828))) (RETURN (|cn| V6829 (|shen.app| V6827 (|tlstr| (|tlstr| V6828)) '|shen.a|))) (IF (EQUAL "R" (|hdstr| (|tlstr| V6828))) (RETURN (|cn| V6829 (|shen.app| V6827 (|tlstr| (|tlstr| V6828)) '|shen.r|))) (IF (EQUAL "S" (|hdstr| (|tlstr| V6828))) (RETURN (|cn| V6829 (|shen.app| V6827 (|tlstr| (|tlstr| V6828)) '|shen.s|))) (GO |%%label428|)))) (GO |%%label428|)) |%%label428| (RETURN (|shen.insert-h| V6827 (|tlstr| V6828) (|cn| V6829 (|hdstr| V6828))))) (RETURN (|simple-error| "implementation error in shen.insert-h"))))))

(DEFUN |shen.app| (V6830 V6831 V6832) (|cn| (|shen.arg->str| V6830 V6832) V6831))

(DEFUN |shen.arg->str| (V6836 V6837) (BLOCK () (IF (EQ V6836 (|fail|)) (RETURN "...") (IF (|shen-cl.true?| (|shen.list?| V6836)) (RETURN (|shen.list->str| V6836 V6837)) (IF (STRINGP V6836) (RETURN (|shen.str->str| V6836 V6837)) (IF (|shen-cl.true?| (|absvector?| V6836)) (RETURN (|shen.vector->str| V6836 V6837)) (RETURN (|shen.atom->str| V6836))))))))

(DEFUN |shen.list->str| (V6838 V6839) (BLOCK () (IF (EQ V6839 '|shen.r|) (RETURN (|@s| "(" (|@s| (|shen.iter-list| V6838 '|shen.r| (|shen.maxseq|)) ")"))) (RETURN (|@s| "[" (|@s| (|shen.iter-list| V6838 V6839 (|shen.maxseq|)) "]"))))))

(DEFUN |shen.maxseq| () |*maximum-print-sequence-size*|)

(DEFUN |shen.iter-list| (V6850 V6851 V6852) (BLOCK () (IF (NULL V6850) (RETURN "") (IF (EQL V6852 0) (RETURN "... etc") (IF (CONSP V6850) (|LET*| ((|V6850/hd| (|hd| V6850)) (|V6850/tl| (|tl| V6850))) (IF (NULL |V6850/tl|) (RETURN (|shen.arg->str| |V6850/hd| V6851)) (RETURN (|@s| (|shen.arg->str| |V6850/hd| V6851) (|@s| " " (|shen.iter-list| |V6850/tl| V6851 (|1-| V6852))))))) (RETURN (|@s| "|" (|@s| " " (|shen.arg->str| V6850 V6851)))))))))

(DEFUN |shen.str->str| (V6855 V6856) (BLOCK () (IF (EQ V6856 '|shen.a|) (RETURN V6855) (RETURN (|@s| (|n->string| 34) (|@s| V6855 (|n->string| 34)))))))

(DEFUN |shen.vector->str| (V6857 V6858) (IF (|shen-cl.true?| (|shen.print-vector?| V6857)) (FUNCALL (|fn| (|<-address| V6857 0)) V6857) (IF (|shen-cl.true?| (|vector?| V6857)) (|@s| "<" (|@s| (|shen.iter-vector| V6857 1 V6858 (|shen.maxseq|)) ">")) (|@s| "<" (|@s| "<" (|@s| (|shen.iter-vector| V6857 0 V6858 (|shen.maxseq|)) ">>"))))))

(DEFUN |shen.empty-absvector?| (V6859) (|shen-cl.equal?| V6859 |shen.*empty-absvector*|))

(DEFUN |shen.print-vector?| (V6860) (|and| (|not| (|shen.empty-absvector?| V6860)) (LET ((W6861 (|<-address| V6860 0))) (|or| (|shen-cl.equal?| W6861 '|shen.tuple|) (|or| (|shen-cl.equal?| W6861 '|shen.pvar|) (|or| (|shen-cl.equal?| W6861 '|shen.dictionary|) (|and| (|not| (|number?| W6861)) (|shen.fbound?| W6861))))))))

(DEFUN |shen.fbound?| (V6862) (|not| (|shen-cl.equal?| (|arity| V6862) -1)))

(DEFUN |shen.tuple| (V6863) (|cn| "(@p " (|shen.app| (|<-address| V6863 1) (|cn| " " (|shen.app| (|<-address| V6863 2) ")" '|shen.s|)) '|shen.s|)))

(DEFUN |shen.dictionary| (V6864) "(dict ...)")

(DEFUN |shen.iter-vector| (V6871 V6872 V6873 V6874) (BLOCK () (IF (EQL V6874 0) (RETURN "... etc") (RETURN (|LET*| ((W6875 (|trap-error| (|<-address| V6871 V6872) (|lambda| Z6876 '|shen.out-of-bounds|))) (W6877 (|trap-error| (|<-address| V6871 (|1+| V6872)) (|lambda| Z6878 '|shen.out-of-bounds|)))) (IF (EQ W6875 '|shen.out-of-bounds|) "" (IF (EQ W6877 '|shen.out-of-bounds|) (|shen.arg->str| W6875 V6873) (|@s| (|shen.arg->str| W6875 V6873) (|@s| " " (|shen.iter-vector| V6871 (|1+| V6872) V6873 (|1-| V6874)))))))))))

(DEFUN |shen.atom->str| (V6879) (|trap-error| (|str| V6879) (|lambda| Z6880 (|shen.funexstring|))))

(DEFUN |shen.funexstring| () (|@s| "" (|@s| "f" (|@s| "u" (|@s| "n" (|@s| "e" (|@s| (|shen.arg->str| (|gensym| '|x|) '|shen.a|) "")))))))

(DEFUN |shen.list?| (V6881) (|or| (|empty?| V6881) (|cons?| V6881)))

