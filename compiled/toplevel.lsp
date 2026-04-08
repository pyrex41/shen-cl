(in-package :shen)

(DEFUN |shen.repl| () (PROGN (|shen.credits|) (|shen.loop|)))

(DEFUN |shen.loop| () (PROGN (|shen.initialise_environment|) (|shen.prompt|) (|trap-error| (|shen.read-evaluate-print|) (|lambda| Z5643 (|shen.toplevel-display-exception| Z5643))) (|shen.loop|)))

(DEFUN |shen.toplevel-display-exception| (V5644) (PROGN (|pr| (|error-to-string| V5644) (|stoutput|)) (|nl| 0)))

(DEFUN |shen.credits| () (PROGN (|pr| "
Shen, www.shenlanguage.org, copyright (C) 2010-2024, Mark Tarver
" (|stoutput|)) (|pr| (|cn| "version: S" (|shen.app| |*version*| (|cn| ", language: " (|shen.app| |*language*| (|cn| ", platform: " (|shen.app| |*implementation*| (|cn| " " (|shen.app| |*release*| "
" '|shen.a|)) '|shen.a|)) '|shen.a|)) '|shen.a|)) (|stoutput|)) (|pr| (|cn| "port " (|shen.app| |*port*| (|cn| ", ported by " (|shen.app| |*porters*| "

" '|shen.a|)) '|shen.a|)) (|stoutput|))))

(DEFUN |shen.initialise_environment| () (PROGN (|set| '|shen.*call*| 0) (|set| '|shen.*infs*| 0)))

(DEFUN |shen.prompt| () (IF (|shen-cl.true?| |shen.*tc*|) (|pr| (|cn| "
(" (|shen.app| (|length| |shen.*history*|) "+) " '|shen.a|)) (|stoutput|)) (|pr| (|cn| "
(" (|shen.app| (|length| |shen.*history*|) "-) " '|shen.a|)) (|stoutput|))))

(DEFUN |shen.read-evaluate-print| () (|LET*| ((W5645 |shen.*package*|) (W5646 (|shen.package-user-input| W5645 (|lineread| (|stinput|)))) (W5647 (|shen.update-history|))) (|shen.evaluate-lineread| W5646 W5647 |shen.*tc*|)))

(DEFUN |shen.package-user-input| (V5648 V5649) (BLOCK () (IF (EQ V5648 '|null|) (RETURN V5649) (RETURN (|LET*| ((W5650 (|str| V5648)) (W5651 (|external| V5648))) (|map| (|lambda| Z5652 (|shen.pui-h| W5650 W5651 Z5652)) V5649))))))

(DEFUN |shen.pui-h| (V5657 V5658 V5659) (BLOCK () (IF (CONSP V5659) (|LET*| ((|V5659/hd| (|hd| V5659)) (|V5659/tl| (|tl| V5659))) (TAGBODY (IF (AND (EQ |V5659/hd| '|fn|) (CONSP |V5659/tl|)) (LET ((|V5659/tl/hd| (|hd| |V5659/tl|))) (IF (NULL (|tl| |V5659/tl|)) (RETURN (IF (|shen-cl.true?| (|shen.internal?| |V5659/tl/hd| V5657 V5658)) (|cons| '|fn| (|cons| (|shen.intern-in-package| V5657 |V5659/tl/hd|) '())) V5659)) (GO |%%label2|))) (GO |%%label2|)) |%%label2| (RETURN (IF (|shen-cl.true?| (|shen.internal?| |V5659/hd| V5657 V5658)) (|cons| (|shen.intern-in-package| V5657 |V5659/hd|) (|map| (|lambda| Z5660 (|shen.pui-h| V5657 V5658 Z5660)) |V5659/tl|)) (IF (CONSP |V5659/hd|) (|map| (|lambda| Z5661 (|shen.pui-h| V5657 V5658 Z5661)) V5659) (|cons| |V5659/hd| (|map| (|lambda| Z5662 (|shen.pui-h| V5657 V5658 Z5662)) |V5659/tl|))))))) (RETURN V5659))))

(DEFUN |shen.update-history| () (|set| '|shen.*history*| (|cons| (|shen.trim-it| (|it|)) |shen.*history*|)))

(DEFUN |shen.trim-it| (V5663) (BLOCK () (IF (AND (|shen-cl.true?| (|shen.+string?| V5663)) (|shen-cl.true?| (|shen.whitespace?| (|string->n| (|hdstr| V5663))))) (RETURN (|shen.trim-it| (|tlstr| V5663))) (RETURN V5663))))

(DEFUN |shen.evaluate-lineread| (V5682 V5683 V5684) (BLOCK () (TAGBODY (IF (AND (CONSP V5682) (AND (NULL (|tl| V5682)) (CONSP V5683))) (|LET*| ((|V5683/hd| (|hd| V5683)) (|V5683/tl| (|tl| V5683))) (IF (|shen-cl.true?| (|shen.+string?| |V5683/hd|)) (IF (EQUAL "!" (|hdstr| |V5683/hd|)) (IF (AND (|shen-cl.true?| (|shen.+string?| (|tlstr| |V5683/hd|))) (AND (EQUAL "!" (|hdstr| (|tlstr| |V5683/hd|))) (CONSP |V5683/tl|))) (LET ((|V5683/tl/hd| (|hd| |V5683/tl|))) (RETURN (|LET*| ((W5685 (|read-from-string| |V5683/tl/hd|)) (W5686 (|set| '|shen.*history*| (|cons| |V5683/tl/hd| |V5683/tl|))) (W5687 (|pr| (|shen.app| |V5683/tl/hd| "
" '|shen.a|) (|stoutput|)))) (|shen.evaluate-lineread| W5685 W5686 V5684)))) (RETURN (|LET*| ((W5688 (IF (EQUAL (|tlstr| |V5683/hd|) "") '() (|hd| (|read-from-string| (|tlstr| |V5683/hd|))))) (W5689 (|shen.use-history| W5688 (|tlstr| |V5683/hd|) |V5683/tl|)) (W5690 (|pr| (|shen.app| W5689 "
" '|shen.a|) (|stoutput|))) (W5691 (|read-from-string| W5689)) (W5692 (|set| '|shen.*history*| (|cons| W5689 |V5683/tl|)))) (|shen.evaluate-lineread| W5691 W5692 V5684)))) (IF (EQUAL "%" (|hdstr| |V5683/hd|)) (RETURN (|LET*| ((W5693 (IF (EQUAL (|tlstr| |V5683/hd|) "") '() (|hd| (|read-from-string| (|tlstr| |V5683/hd|))))) (W5694 (|shen.peek-history| W5693 (|tlstr| |V5683/hd|) |V5683/tl|)) (W5695 (|set| '|shen.*history*| |V5683/tl|))) (|abort|))) (GO |%%label5|))) (GO |%%label5|))) (GO |%%label5|)) |%%label5| (IF (EQ V5684 '|true|) (RETURN (|shen.check-eval-and-print| V5682)) (IF (EQ V5684 '|false|) (RETURN (|shen.eval-and-print| V5682)) (RETURN (|simple-error| "implementation error in shen.evaluate-lineread")))))))

(DEFUN |shen.use-history| (V5696 V5697 V5698) (IF (|shen-cl.true?| (|integer?| V5696)) (|nth| (|1+| V5696) (|reverse| V5698)) (IF (|shen-cl.true?| (|symbol?| V5696)) (|shen.string-match| V5697 V5698) (|simple-error| "! expects a number or a symbol
"))))

(DEFUN |shen.peek-history| (V5699 V5700 V5701) (IF (|shen-cl.true?| (|integer?| V5699)) (|pr| (|cn| "
" (|shen.app| (|nth| (|1+| V5699) (|reverse| V5701)) "" '|shen.a|)) (|stoutput|)) (IF (OR (EQUAL V5700 "") (|shen-cl.true?| (|symbol?| V5699))) (|shen.recursive-string-match| 0 V5700 (|reverse| V5701)) (|simple-error| "% expects a number or a symbol
"))))

(DEFUN |shen.string-match| (V5711 V5712) (BLOCK () (IF (NULL V5712) (RETURN (|simple-error| "
input not found")) (IF (CONSP V5712) (LET ((|V5712/hd| (|hd| V5712))) (IF (|shen-cl.true?| (|shen.string-prefix?| V5711 |V5712/hd|)) (RETURN |V5712/hd|) (RETURN (|shen.string-match| V5711 (|tl| V5712))))) (RETURN (|simple-error| "implementation error in shen.string-match"))))))

(DEFUN |shen.string-prefix?| (V5720 V5721) (BLOCK () (IF (EQUAL "" V5720) (RETURN '|true|) (IF (AND (|shen-cl.true?| (|shen.+string?| V5720)) (|shen-cl.true?| (|shen.whitespace?| (|string->n| (|hdstr| V5720))))) (RETURN (|shen.string-prefix?| (|tlstr| V5720) V5721)) (TAGBODY (IF (|shen-cl.true?| (|shen.+string?| V5721)) (IF (|shen-cl.true?| (|shen.whitespace?| (|string->n| (|hdstr| V5721)))) (RETURN (|shen.string-prefix?| V5720 (|tlstr| V5721))) (IF (EQUAL "(" (|hdstr| V5721)) (RETURN (|shen.string-prefix?| V5720 (|tlstr| V5721))) (GO |%%label11|))) (GO |%%label11|)) |%%label11| (IF (AND (|shen-cl.true?| (|shen.+string?| V5720)) (AND (|shen-cl.true?| (|shen.+string?| V5721)) (|shen-cl.absequal| (|hdstr| V5720) (|hdstr| V5721)))) (RETURN (|shen.string-prefix?| (|tlstr| V5720) (|tlstr| V5721))) (RETURN '|false|)))))))

(DEFUN |shen.recursive-string-match| (V5732 V5733 V5734) (BLOCK () (IF (NULL V5734) (RETURN '|shen.skip|) (IF (CONSP V5734) (LET ((|V5734/hd| (|hd| V5734))) (RETURN (PROGN (IF (|shen-cl.true?| (|shen.string-prefix?| V5733 |V5734/hd|)) (|pr| (|shen.app| V5732 (|cn| ". " (|shen.app| |V5734/hd| "
" '|shen.a|)) '|shen.a|) (|stoutput|)) '|shen.skip|) (|shen.recursive-string-match| (|1+| V5732) V5733 (|tl| V5734))))) (RETURN (|simple-error| "implementation error in shen.recursive-string-match"))))))

