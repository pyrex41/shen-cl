(in-package :shen)

(DEFUN |load| (V1215) (|LET*| ((W1216 |shen.*tc*|) (W1217 (|LET*| ((W1218 (|get-time| '|run|)) (W1219 (|shen.load-help| W1216 (|read-file| V1215))) (W1220 (|get-time| '|run|)) (W1221 (|shen-cl.subtract| W1220 W1218)) (W1222 (|pr| (|cn| "
run time: " (|cn| (|str| W1221) " secs
")) (|stoutput|)))) W1219)) (W1223 (IF (|shen-cl.true?| W1216) (|pr| (|cn| "
typechecked in " (|shen.app| (|inferences|) " inferences
" '|shen.a|)) (|stoutput|)) '|shen.skip|))) '|loaded|))

(DEFUN |shen.load-help| (V1226 V1227) (BLOCK () (IF (EQ V1226 '|false|) (RETURN (|shen.eval-and-print| V1227)) (RETURN (|shen.check-eval-and-print| V1227)))))

(DEFUN |shen.eval-and-print| (V1228) (|shen.for-each| (|lambda| Z1229 (|pr| (|shen.app| (|eval-kl| (|shen.shen->kl| Z1229)) "
" '|shen.s|) (|stoutput|))) V1228))

(DEFUN |shen.check-eval-and-print| (V1230) (|LET*| ((W1231 (|mapcan| (|lambda| Z1232 (|shen.typetable| Z1232)) V1230)) (W1233 (|trap-error| (|shen.assumetypes| W1231) (|lambda| Z1234 (|shen.unwind-types| Z1234 W1231))))) (|trap-error| (|shen.work-through| V1230) (|lambda| Z1235 (|shen.unwind-types| Z1235 W1231)))))

(DEFUN |shen.typetable| (V1240) (BLOCK () (IF (CONSP V1240) (LET ((|V1240/tl| (|tl| V1240))) (IF (AND (EQ (|hd| V1240) '|define|) (CONSP |V1240/tl|)) (|LET*| ((|V1240/tl/hd| (|hd| |V1240/tl|)) (|V1240/tl/tl| (|tl| |V1240/tl|))) (IF (AND (CONSP |V1240/tl/tl|) (EQ (|hd| |V1240/tl/tl|) '{)) (RETURN (|cons| |V1240/tl/hd| (|cons| (|shen.rectify-type| (|shen.type-F| |V1240/tl/hd| (|tl| |V1240/tl/tl|))) '()))) (RETURN (|simple-error| (|cn| "missing { in " (|shen.app| |V1240/tl/hd| "
" '|shen.a|)))))) (RETURN '()))) (RETURN '()))))

(DEFUN |shen.type-F| (V1247 V1248) (BLOCK () (IF (CONSP V1248) (LET ((|V1248/hd| (|hd| V1248))) (IF (EQ |V1248/hd| '}) (RETURN '()) (RETURN (|cons| |V1248/hd| (|shen.type-F| V1247 (|tl| V1248)))))) (RETURN (|simple-error| (|cn| "missing } in " (|shen.app| V1247 "
" '|shen.a|)))))))

(DEFUN |shen.assumetypes| (V1251) (BLOCK () (IF (NULL V1251) (RETURN '()) (TAGBODY (IF (CONSP V1251) (LET ((|V1251/tl| (|tl| V1251))) (IF (CONSP |V1251/tl|) (RETURN (PROGN (|declare| (|hd| V1251) (|hd| |V1251/tl|)) (|shen.assumetypes| (|tl| |V1251/tl|)))) (GO |%%label396|))) (GO |%%label396|)) |%%label396| (RETURN (|simple-error| "implementation error in shen.assumetype"))))))

(DEFUN |shen.unwind-types| (V1256 V1257) (BLOCK () (TAGBODY (IF (CONSP V1257) (LET ((|V1257/tl| (|tl| V1257))) (IF (CONSP |V1257/tl|) (RETURN (PROGN (|destroy| (|hd| V1257)) (|shen.unwind-types| V1256 (|tl| |V1257/tl|)))) (GO |%%label398|))) (GO |%%label398|)) |%%label398| (RETURN (|simple-error| (|error-to-string| V1256))))))

(DEFUN |shen.work-through| (V1260) (BLOCK () (IF (NULL V1260) (RETURN '()) (IF (CONSP V1260) (|LET*| ((|V1260/hd| (|hd| V1260)) (|V1260/tl| (|tl| V1260))) (TAGBODY (IF (CONSP |V1260/tl|) (LET ((|V1260/tl/tl| (|tl| |V1260/tl|))) (IF (AND (CONSP |V1260/tl/tl|) (EQ (|hd| |V1260/tl|) '|:|)) (RETURN (LET ((W1261 (|shen.typecheck| |V1260/hd| (|hd| |V1260/tl/tl|)))) (IF (EQ W1261 '|false|) (|shen.type-error|) (|LET*| ((W1262 (|eval-kl| (|shen.shen->kl| |V1260/hd|))) (W1263 (|pr| (|shen.app| W1262 (|cn| " : " (|shen.app| (|shen.pretty-type| W1261) "
" '|shen.r|)) '|shen.s|) (|stoutput|)))) (|shen.work-through| (|tl| |V1260/tl/tl|)))))) (GO |%%label400|))) (GO |%%label400|)) |%%label400| (RETURN (|shen.work-through| (|cons| |V1260/hd| (|cons| '|:| (|cons| 'A |V1260/tl|))))))) (RETURN (|simple-error| "implementation error in shen.work-through"))))))

(DEFUN |shen.pretty-type| (V1265) (BLOCK () (IF (CONSP V1265) (|LET*| ((|V1265/hd| (|hd| V1265)) (|V1265/tl| (|tl| V1265))) (TAGBODY (IF (CONSP |V1265/hd|) (LET ((|V1265/hd/tl| (|tl| |V1265/hd|))) (IF (AND (EQ (|hd| |V1265/hd|) '|list|) (AND (CONSP |V1265/hd/tl|) (AND (NULL (|tl| |V1265/hd/tl|)) (CONSP |V1265/tl|)))) (LET ((|V1265/tl/tl| (|tl| |V1265/tl|))) (IF (AND (EQ (|hd| |V1265/tl|) '|-->|) (CONSP |V1265/tl/tl|)) (LET ((|V1265/tl/tl/hd| (|hd| |V1265/tl/tl|))) (IF (CONSP |V1265/tl/tl/hd|) (LET ((|V1265/tl/tl/hd/tl| (|tl| |V1265/tl/tl/hd|))) (IF (AND (EQ (|hd| |V1265/tl/tl/hd|) '|str|) (CONSP |V1265/tl/tl/hd/tl|)) (|LET*| ((|V1265/tl/tl/hd/tl/hd| (|hd| |V1265/tl/tl/hd/tl|)) (|V1265/tl/tl/hd/tl/tl| (|tl| |V1265/tl/tl/hd/tl|))) (IF (CONSP |V1265/tl/tl/hd/tl/hd|) (LET ((|V1265/tl/tl/hd/tl/hd/tl| (|tl| |V1265/tl/tl/hd/tl/hd|))) (IF (AND (EQ (|hd| |V1265/tl/tl/hd/tl/hd|) '|list|) (AND (CONSP |V1265/tl/tl/hd/tl/hd/tl|) (AND (NULL (|tl| |V1265/tl/tl/hd/tl/hd/tl|)) (AND (CONSP |V1265/tl/tl/hd/tl/tl|) (AND (NULL (|tl| |V1265/tl/tl/hd/tl/tl|)) (AND (NULL (|tl| |V1265/tl/tl|)) (|shen-cl.absequal| (|hd| |V1265/hd/tl|) (|hd| |V1265/tl/tl/hd/tl/hd/tl|)))))))) (RETURN (|cons| |V1265/tl/tl/hd/tl/hd| (|cons| '|==>| |V1265/tl/tl/hd/tl/tl|))) (GO |%%label402|))) (GO |%%label402|))) (GO |%%label402|))) (GO |%%label402|))) (GO |%%label402|))) (GO |%%label402|))) (GO |%%label402|)) |%%label402| (RETURN (|map| (|lambda| Z1266 (|shen.pretty-type| Z1266)) V1265)))) (RETURN V1265))))

(DEFUN |shen.type-error| () (|simple-error| "type error
"))

(DEFUN |bootstrap| (V1267) (|LET*| ((W1268 (|shen.klfile| V1267)) (W1269 (|read-file| V1267)) (W1270 (|open| W1268 '|out|)) (W1271 (|map| (|lambda| Z1272 (|shen.partial| (|shen.shen->kl-h| Z1272))) W1269)) (W1273 (|shen.write-kl| W1271 W1270))) W1268))

(DEFUN |shen.partial| (V1274) (BLOCK () (IF (CONSP V1274) (LET ((|V1274/tl| (|tl| V1274))) (IF (AND (EQ (|hd| V1274) '|shen.f-error|) (AND (CONSP |V1274/tl|) (NULL (|tl| |V1274/tl|)))) (RETURN (|cons| '|simple-error| (|cons| (|cn| "partial function " (|str| (|hd| |V1274/tl|))) '()))) (RETURN (|map| (|lambda| Z1275 (|shen.partial| Z1275)) V1274)))) (RETURN V1274))))

(DEFUN |shen.write-kl| (V1278 V1279) (BLOCK () (IF (NULL V1278) (RETURN (|close| V1279)) (IF (CONSP V1278) (|LET*| ((|V1278/hd| (|hd| V1278)) (|V1278/tl| (|tl| V1278))) (IF (CONSP |V1278/hd|) (RETURN (|shen.write-kl| |V1278/tl| (PROGN (|shen.write-kl-h| |V1278/hd| V1279) V1279))) (RETURN (|shen.write-kl| |V1278/tl| V1279)))) (RETURN (|shen.f-error| '|shen.write-kl|))))))

(DEFUN |shen.write-kl-h| (V1282 V1283) (BLOCK () (TAGBODY (IF (CONSP V1282) (LET ((|V1282/tl| (|tl| V1282))) (IF (AND (EQ (|hd| V1282) '|defun|) (CONSP |V1282/tl|)) (LET ((|V1282/tl/tl| (|tl| |V1282/tl|))) (IF (AND (EQ (|hd| |V1282/tl|) '|fail|) (CONSP |V1282/tl/tl|)) (LET ((|V1282/tl/tl/tl| (|tl| |V1282/tl/tl|))) (IF (AND (NULL (|hd| |V1282/tl/tl|)) (AND (CONSP |V1282/tl/tl/tl|) (NULL (|tl| |V1282/tl/tl/tl|)))) (RETURN (|pr| "(defun fail () shen.fail!)" V1283)) (GO |%%label407|))) (GO |%%label407|))) (GO |%%label407|))) (GO |%%label407|)) |%%label407| (RETURN (|pr| (|shen.app| V1282 "

" '|shen.r|) V1283)))))

(DEFUN |shen.klfile| (V1284) (BLOCK () (IF (EQUAL "" V1284) (RETURN ".kl") (IF (EQUAL ".shen" V1284) (RETURN ".kl") (IF (|shen-cl.true?| (|shen.+string?| V1284)) (RETURN (|@s| (|hdstr| V1284) (|shen.klfile| (|tlstr| V1284)))) (RETURN (|shen.f-error| '|shen.klfile|)))))))

