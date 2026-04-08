(in-package :shen)

(DEFUN |shen.dict| (V4138) (BLOCK () (IF (|shen-cl.true?| (|shen-cl.less?| V4138 1)) (RETURN (|simple-error| (|cn| "invalid initial dict size: " (|shen.app| V4138 "" '|shen.s|)))) (RETURN (|LET*| ((W4139 (|absvector| (|+| 3 V4138))) (W4140 (|address->| W4139 0 '|shen.dictionary|)) (W4141 (|address->| W4139 1 V4138)) (W4142 (|address->| W4139 2 0)) (W4143 (|shen.fillvector| W4139 3 (|+| 2 V4138) '()))) W4139)))))

(DEFUN |shen.dict?| (V4144) (|and| (|absvector?| V4144) (|shen-cl.equal?| (|trap-error| (|<-address| V4144 0) (|lambda| Z4145 '|shen.not-dictionary|)) '|shen.dictionary|)))

(DEFUN |shen.dict-capacity| (V4146) (|<-address| V4146 1))

(DEFUN |shen.dict-count| (V4147) (|<-address| V4147 2))

(DEFUN |shen.dict-count->| (V4148 V4149) (|address->| V4148 2 V4149))

(DEFUN |shen.<-dict-bucket| (V4150 V4151) (|<-address| V4150 (|+| 3 V4151)))

(DEFUN |shen.dict-bucket->| (V4152 V4153 V4154) (|address->| V4152 (|+| 3 V4153) V4154))

(DEFUN |shen.dict-update-count| (V4155 V4156 V4157) (LET ((W4158 (|shen-cl.subtract| (|length| V4157) (|length| V4156)))) (|shen.dict-count->| V4155 (|+| W4158 (|shen.dict-count| V4155)))))

(DEFUN |shen.dict->| (V4159 V4160 V4161) (|LET*| ((W4162 (|hash| V4160 (|shen.dict-capacity| V4159))) (W4163 (|shen.<-dict-bucket| V4159 W4162)) (W4164 (|shen.assoc-set| V4160 V4161 W4163)) (W4165 (|shen.dict-bucket->| V4159 W4162 W4164)) (W4166 (|shen.dict-update-count| V4159 W4163 W4164))) V4161))

(DEFUN |shen.<-dict| (V4167 V4168) (|LET*| ((W4169 (|hash| V4168 (|shen.dict-capacity| V4167))) (W4170 (|shen.<-dict-bucket| V4167 W4169)) (W4171 (|assoc| V4168 W4170))) (IF (NULL W4171) (|simple-error| (|cn| "value " (|shen.app| V4168 " not found in dict
" '|shen.a|))) (|tl| W4171))))

(DEFUN |shen.dict-rm| (V4172 V4173) (|LET*| ((W4174 (|hash| V4173 (|shen.dict-capacity| V4172))) (W4175 (|shen.<-dict-bucket| V4172 W4174)) (W4176 (|shen.assoc-rm| V4173 W4175)) (W4177 (|shen.dict-bucket->| V4172 W4174 W4176)) (W4178 (|shen.dict-update-count| V4172 W4175 W4176))) V4173))

(DEFUN |shen.dict-fold| (V4179 V4180 V4181) (LET ((W4182 (|shen.dict-capacity| V4180))) (|shen.dict-fold-h| V4179 V4180 V4181 0 W4182)))

(DEFUN |shen.dict-fold-h| (V4184 V4185 V4186 V4187 V4188) (BLOCK () (IF (|shen-cl.absequal| V4187 V4188) (RETURN V4186) (RETURN (|LET*| ((W4189 (|shen.<-dict-bucket| V4185 V4187)) (W4190 (|shen.bucket-fold| V4184 W4189 V4186))) (|shen.dict-fold-h| V4184 V4185 W4190 (|1+| V4187) V4188))))))

(DEFUN |shen.bucket-fold| (V4191 V4192 V4193) (BLOCK () (IF (NULL V4192) (RETURN V4193) (TAGBODY (IF (CONSP V4192) (LET ((|V4192/hd| (|hd| V4192))) (IF (CONSP |V4192/hd|) (RETURN (FUNCALL (FUNCALL (FUNCALL V4191 (|hd| |V4192/hd|)) (|tl| |V4192/hd|)) (|shen.bucket-fold| V4191 (|tl| V4192) V4193))) (GO |%%label167|))) (GO |%%label167|)) |%%label167| (RETURN (|shen.f-error| '|shen.bucket-fold|))))))

(DEFUN |shen.dict-keys| (V4194) (|shen.dict-fold| (|lambda| Z4195 (|lambda| Z4196 (|lambda| Z4197 (|cons| Z4195 Z4197)))) V4194 '()))

(DEFUN |shen.dict-values| (V4198) (|shen.dict-fold| (|lambda| Z4199 (|lambda| Z4200 (|lambda| Z4201 (|cons| Z4200 Z4201)))) V4198 '()))

