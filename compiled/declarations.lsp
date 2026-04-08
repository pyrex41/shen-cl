(in-package :shen)

(DEFUN |datatypes| () (|map| (|lambda| Z880 (|shen.typename| Z880)) |shen.*alldatatypes*|))

(DEFUN |shen.included| () (|map| (|lambda| Z881 (|shen.typename| Z881)) |shen.*datatypes*|))

(DEFUN |shen.typename| (V884) (BLOCK () (IF (CONSP V884) (RETURN (|intern| (|shen.typename-h| (|str| (|hd| V884))))) (RETURN (|shen.f-error| '|shen.typename|)))))

(DEFUN |shen.typename-h| (V885) (BLOCK () (IF (EQUAL "#type" V885) (RETURN "") (IF (|shen-cl.true?| (|shen.+string?| V885)) (RETURN (|cn| (|hdstr| V885) (|shen.typename-h| (|tlstr| V885)))) (RETURN (|shen.f-error| '|shen.typename-h|))))))

(DEFUN |prolog-memory| (V886) (IF (|shen-cl.true?| (|shen-cl.less?| V886 0)) |shen.*prolog-memory*| (IF (|shen-cl.true?| (|integer?| V886)) (|set| '|shen.*prolog-memory*| V886) (|simple-error| "prolog memory expects an integer value
"))))

(DEFUN |arity| (V887) (|trap-error| (|get| V887 '|arity| |*property-vector*|) (|lambda| Z888 -1)))

(DEFUN |shen.initialise-arity-table| (V891) (BLOCK () (IF (NULL V891) (RETURN '()) (TAGBODY (IF (CONSP V891) (LET ((|V891/tl| (|tl| V891))) (IF (CONSP |V891/tl|) (RETURN (LET ((W892 (|put| (|hd| V891) '|arity| (|hd| |V891/tl|) |*property-vector*|))) (|shen.initialise-arity-table| (|tl| |V891/tl|)))) (GO |%%label446|))) (GO |%%label446|)) |%%label446| (RETURN (|simple-error| "implementation error in shen.initialise-arity-table"))))))

(DEFUN |systemf| (V893) (|LET*| ((W894 (|get| '|shen| '|shen.external-symbols| |*property-vector*|)) (W895 (|put| '|shen| '|shen.external-symbols| (|adjoin| V893 W894) |*property-vector*|))) V893))

(DEFUN |adjoin| (V896 V897) (IF (|shen-cl.true?| (|element?| V896 V897)) V897 (|cons| V896 V897)))

(DEFUN |shen.lambda-entry| (V898) (LET ((W899 (|arity| V898))) (IF (OR (EQL W899 -1) (EQL W899 0)) '() (|cons| V898 (|eval-kl| (|shen.lambda-function| (|cons| V898 '()) W899))))))

(DEFUN |shen.set-lambda-form-entry| (V900) (BLOCK () (IF (CONSP V900) (RETURN (|put| (|hd| V900) '|shen.lambda-form| (|tl| V900) |*property-vector*|)) (RETURN (|shen.f-error| '|shen.set-lambda-form-entry|)))))

(DEFUN |shen.build-lambda-table| (V901) (LET ((W902 (|map| (|lambda| Z903 (|shen.lambda-entry| Z903)) V901))) (|shen.for-each| (|lambda| Z904 (|shen.set-lambda-form-entry| Z904)) (|cons| (|cons| '|shen.tuple| (|lambda| Z905 (|shen.tuple| Z905))) (|cons| (|cons| '|shen.pvar| (|lambda| Z906 (|shen.pvar| Z906))) (|cons| (|cons| '|shen.dictionary| (|lambda| Z907 (|shen.dictionary| Z907))) (|cons| (|cons| '|shen.print-prolog-vector| (|lambda| Z908 (|shen.print-prolog-vector| Z908))) (|cons| (|cons| '|shen.print-freshterm| (|lambda| Z909 (|shen.print-freshterm| Z909))) (|cons| (|cons| '|shen.printF| (|lambda| Z910 (|shen.printF| Z910))) W902)))))))))

