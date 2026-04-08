(in-package :shen)

(DEFUN |thaw| (V3743) (FUNCALL V3743))

(DEFUN |eval| (V3744) (|eval-kl| (|shen.shen->kl| (|shen.process-applications| (|macroexpand| V3744) (|shen.find-types| V3744)))))

(DEFUN |external| (V3745) (BLOCK () (IF (EQ V3745 '|null|) (RETURN '()) (RETURN (|trap-error| (|get| V3745 '|shen.external-symbols| |*property-vector*|) (|lambda| Z3746 (|simple-error| (|cn| "package " (|shen.app| V3745 " does not exist.
;" '|shen.a|)))))))))

(DEFUN |internal| (V3747) (BLOCK () (IF (EQ V3747 '|null|) (RETURN '()) (RETURN (|trap-error| (|get| V3747 '|shen.internal-symbols| |*property-vector*|) (|lambda| Z3748 (|simple-error| (|cn| "package " (|shen.app| V3747 " does not exist.
;" '|shen.a|)))))))))

(DEFUN |fail-if| (V3749 V3750) (IF (|shen-cl.true?| (FUNCALL V3749 V3750)) (|fail|) V3750))

(DEFUN |@s| (V3751 V3752) (|cn| V3751 V3752))

(DEFUN |tc?| () |shen.*tc*|)

(DEFUN |occurs?| () |shen.*occurs*|)

(DEFUN |factorise?| () |shen.*factorise?*|)

(DEFUN |tracked| () |shen.*tracking*|)

(DEFUN |ps| (V3753) (|trap-error| (|get| V3753 '|shen.source| |*property-vector*|) (|lambda| Z3754 (|simple-error| (|shen.app| V3753 " not found.
" '|shen.a|)))))

(DEFUN |stinput| () |*stinput*|)

(DEFUN |vector| (V3755) (|LET*| ((W3756 (|absvector| (|1+| V3755))) (W3757 (|address->| W3756 0 V3755)) (W3758 (IF (EQL V3755 0) W3757 (|shen.fillvector| W3757 1 V3755 (|fail|))))) W3758))

(DEFUN |shen.fillvector| (V3760 V3761 V3762 V3763) (BLOCK () (IF (|shen-cl.absequal| V3761 V3762) (RETURN (|address->| V3760 V3762 V3763)) (RETURN (|shen.fillvector| (|address->| V3760 V3761 V3763) (|1+| V3761) V3762 V3763)))))

(DEFUN |vector?| (V3764) (|and| (|absvector?| V3764) (LET ((W3765 (|trap-error| (|<-address| V3764 0) (|lambda| Z3766 -1)))) (|and| (|number?| W3765) (|shen-cl.greater-than-or-equal-to?| W3765 0)))))

(DEFUN |vector->| (V3767 V3768 V3769) (IF (EQL V3768 0) (|simple-error| "cannot access 0th element of a vector
") (|address->| V3767 V3768 V3769)))

(DEFUN |<-vector| (V3770 V3771) (IF (EQL V3771 0) (|simple-error| "cannot access 0th element of a vector
") (LET ((W3772 (|<-address| V3770 V3771))) (IF (EQ W3772 (|fail|)) (|simple-error| "vector element not found
") W3772))))

(DEFUN |shen.posint?| (V3773) (|and| (|integer?| V3773) (|shen-cl.greater-than-or-equal-to?| V3773 0)))

(DEFUN |limit| (V3774) (|<-address| V3774 0))

(DEFUN |symbol?| (V3775) (BLOCK () (IF (OR (|shen-cl.true?| (|boolean?| V3775)) (OR (NUMBERP V3775) (OR (STRINGP V3775) (OR (CONSP V3775) (OR (NULL V3775) (|shen-cl.true?| (|vector?| V3775))))))) (RETURN '|false|) (IF (|shen-cl.true?| (|element?| V3775 (|cons| '{ (|cons| '} (|cons| '|:| (|cons| '|;| (|cons| '|,| '()))))))) (RETURN '|true|) (RETURN (|trap-error| (LET ((W3776 (|str| V3775))) (|shen.analyse-symbol?| W3776)) (|lambda| Z3777 '|false|)))))))

(DEFUN |shen.analyse-symbol?| (V3780) (BLOCK () (IF (|shen-cl.true?| (|shen.+string?| V3780)) (RETURN (|and| (|shen.alpha?| (|string->n| (|hdstr| V3780))) (|shen.alphanums?| (|tlstr| V3780)))) (RETURN (|simple-error| "implementation error in shen.analyse-symbol?")))))

(DEFUN |shen.alphanums?| (V3783) (BLOCK () (IF (EQUAL "" V3783) (RETURN '|true|) (IF (|shen-cl.true?| (|shen.+string?| V3783)) (RETURN (LET ((W3784 (|string->n| (|hdstr| V3783)))) (|and| (|or| (|shen.alpha?| W3784) (|shen.digit?| W3784)) (|shen.alphanums?| (|tlstr| V3783))))) (RETURN (|simple-error| "implementation error in shen.alphanums?"))))))

(DEFUN |variable?| (V3785) (BLOCK () (IF (OR (|shen-cl.true?| (|boolean?| V3785)) (OR (NUMBERP V3785) (STRINGP V3785))) (RETURN '|false|) (RETURN (|trap-error| (LET ((W3786 (|str| V3785))) (|shen.analyse-variable?| W3786)) (|lambda| Z3787 '|false|))))))

(DEFUN |shen.analyse-variable?| (V3790) (BLOCK () (IF (|shen-cl.true?| (|shen.+string?| V3790)) (RETURN (|and| (|shen.uppercase?| (|string->n| (|hdstr| V3790))) (|shen.alphanums?| (|tlstr| V3790)))) (RETURN (|simple-error| "implementation error in shen.analyse-variable?")))))

(DEFUN |gensym| (V3791) (|concat| V3791 (|set| '|shen.*gensym*| (|1+| |shen.*gensym*|))))

(DEFUN |concat| (V3792 V3793) (|intern| (|cn| (|str| V3792) (|str| V3793))))

(DEFUN |@p| (V3794 V3795) (|LET*| ((W3796 (|absvector| 3)) (W3797 (|address->| W3796 0 '|shen.tuple|)) (W3798 (|address->| W3796 1 V3794)) (W3799 (|address->| W3796 2 V3795))) W3796))

(DEFUN |fst| (V3800) (|<-address| V3800 1))

(DEFUN |snd| (V3801) (|<-address| V3801 2))

(DEFUN |tuple?| (V3802) (|and| (|absvector?| V3802) (|shen-cl.equal?| '|shen.tuple| (|trap-error| (|<-address| V3802 0) (|lambda| Z3803 '|shen.not-tuple|)))))

(DEFUN |append| (V3808 V3809) (BLOCK () (IF (NULL V3808) (RETURN V3809) (IF (CONSP V3808) (RETURN (|cons| (|hd| V3808) (|append| (|tl| V3808) V3809))) (RETURN (|simple-error| "attempt to append a non-list"))))))

(DEFUN |@v| (V3810 V3811) (|LET*| ((W3812 (|limit| V3811)) (W3813 (|vector| (|1+| W3812))) (W3814 (|vector->| W3813 1 V3810))) (IF (EQL W3812 0) W3814 (|shen.@v-help| V3811 1 W3812 W3814))))

(DEFUN |shen.@v-help| (V3816 V3817 V3818 V3819) (BLOCK () (IF (|shen-cl.absequal| V3817 V3818) (RETURN (|shen.copyfromvector| V3816 V3819 V3818 (|1+| V3818))) (RETURN (|shen.@v-help| V3816 (|1+| V3817) V3818 (|shen.copyfromvector| V3816 V3819 V3817 (|1+| V3817)))))))

(DEFUN |shen.copyfromvector| (V3820 V3821 V3822 V3823) (|trap-error| (|vector->| V3821 V3823 (|<-vector| V3820 V3822)) (|lambda| Z3824 V3821)))

(DEFUN |hdv| (V3825) (|trap-error| (|<-vector| V3825 1) (|lambda| Z3826 (|simple-error| "hdv needs a non-empty vector as an argument
"))))

(DEFUN |tlv| (V3827) (LET ((W3828 (|limit| V3827))) (IF (EQL W3828 0) (|simple-error| "cannot take the tail of the empty vector
") (IF (EQL W3828 1) (|vector| 0) (LET ((W3829 (|vector| (|1-| W3828)))) (|shen.tlv-help| V3827 2 W3828 (|vector| (|1-| W3828))))))))

(DEFUN |shen.tlv-help| (V3831 V3832 V3833 V3834) (BLOCK () (IF (|shen-cl.absequal| V3832 V3833) (RETURN (|shen.copyfromvector| V3831 V3834 V3833 (|1-| V3833))) (RETURN (|shen.tlv-help| V3831 (|1+| V3832) V3833 (|shen.copyfromvector| V3831 V3834 V3832 (|1-| V3832)))))))

(DEFUN |assoc| (V3846 V3847) (BLOCK () (IF (NULL V3847) (RETURN '()) (IF (CONSP V3847) (LET ((|V3847/hd| (|hd| V3847))) (IF (AND (CONSP |V3847/hd|) (|shen-cl.absequal| V3846 (|hd| |V3847/hd|))) (RETURN |V3847/hd|) (RETURN (|assoc| V3846 (|tl| V3847))))) (RETURN (|simple-error| "attempt to search a non-list with assoc
"))))))

(DEFUN |shen.assoc-set| (V3851 V3852 V3853) (BLOCK () (IF (NULL V3853) (RETURN (|cons| (|cons| V3851 V3852) '())) (IF (CONSP V3853) (|LET*| ((|V3853/hd| (|hd| V3853)) (|V3853/tl| (|tl| V3853))) (TAGBODY (IF (CONSP |V3853/hd|) (LET ((|V3853/hd/hd| (|hd| |V3853/hd|))) (IF (|shen-cl.absequal| V3851 |V3853/hd/hd|) (RETURN (|cons| (|cons| |V3853/hd/hd| V3852) |V3853/tl|)) (GO |%%label102|))) (GO |%%label102|)) |%%label102| (RETURN (|cons| |V3853/hd| (|shen.assoc-set| V3851 V3852 |V3853/tl|))))) (RETURN (|shen.f-error| '|shen.assoc-set|))))))

(DEFUN |shen.assoc-rm| (V3857 V3858) (BLOCK () (IF (NULL V3858) (RETURN '()) (IF (CONSP V3858) (|LET*| ((|V3858/hd| (|hd| V3858)) (|V3858/tl| (|tl| V3858))) (IF (AND (CONSP |V3858/hd|) (|shen-cl.absequal| V3857 (|hd| |V3858/hd|))) (RETURN |V3858/tl|) (RETURN (|cons| |V3858/hd| (|shen.assoc-rm| V3857 |V3858/tl|))))) (RETURN (|shen.f-error| '|shen.assoc-rm|))))))

(DEFUN |boolean?| (V3861) (BLOCK () (IF (EQ V3861 '|true|) (RETURN '|true|) (IF (EQ V3861 '|false|) (RETURN '|true|) (RETURN '|false|)))))

(DEFUN |nl| (V3862) (BLOCK () (IF (EQL V3862 0) (RETURN 0) (RETURN (PROGN (|pr| "
" (|stoutput|)) (|nl| (|1-| V3862)))))))

(DEFUN |difference| (V3869 V3870) (BLOCK () (IF (NULL V3869) (RETURN '()) (IF (CONSP V3869) (|LET*| ((|V3869/hd| (|hd| V3869)) (|V3869/tl| (|tl| V3869))) (RETURN (IF (|shen-cl.true?| (|element?| |V3869/hd| V3870)) (|difference| |V3869/tl| V3870) (|cons| |V3869/hd| (|difference| |V3869/tl| V3870))))) (RETURN (|simple-error| "attempt to find the difference with a non-list
"))))))

(DEFUN |do| (V3871 V3872) V3872)

(DEFUN |element?| (V3884 V3885) (BLOCK () (IF (NULL V3885) (RETURN '|false|) (IF (CONSP V3885) (IF (|shen-cl.absequal| V3884 (|hd| V3885)) (RETURN '|true|) (RETURN (|element?| V3884 (|tl| V3885)))) (RETURN (|simple-error| "attempt to find an element in a non-list
"))))))

(DEFUN |empty?| (V3888) (BLOCK () (IF (NULL V3888) (RETURN '|true|) (RETURN '|false|))))

(DEFUN |fix| (V3889 V3890) (|shen.fix-help| V3889 V3890 (FUNCALL V3889 V3890)))

(DEFUN |shen.fix-help| (V3896 V3897 V3898) (BLOCK () (IF (|shen-cl.absequal| V3897 V3898) (RETURN V3898) (RETURN (|shen.fix-help| V3896 V3898 (FUNCALL V3896 V3898))))))

(DEFUN |put| (V3899 V3900 V3901 V3902) (|LET*| ((W3903 (|trap-error| (|shen.<-dict| V3902 V3899) (|lambda| Z3904 '()))) (W3905 (|shen.assoc-set| V3900 V3901 W3903)) (W3906 (|shen.dict->| V3902 V3899 W3905))) V3901))

(DEFUN |unput| (V3907 V3908 V3909) (|LET*| ((W3910 (|trap-error| (|shen.<-dict| V3909 V3907) (|lambda| Z3911 '()))) (W3912 (|shen.assoc-rm| V3908 W3910)) (W3913 (|shen.dict->| V3909 V3907 W3912))) V3907))

(DEFUN |get| (V3914 V3915 V3916) (|LET*| ((W3917 (|trap-error| (|shen.<-dict| V3916 V3914) (|lambda| Z3918 (|simple-error| (|shen.app| V3914 (|cn| " has no attributes: " (|shen.app| V3915 "
" '|shen.s|)) '|shen.a|))))) (W3919 (|assoc| V3915 W3917))) (IF (NULL W3919) (|simple-error| (|cn| "attribute " (|shen.app| V3915 (|cn| " not found for " (|shen.app| V3914 "
" '|shen.s|)) '|shen.s|))) (|tl| W3919))))

(DEFUN |hash| (V3920 V3921) (LET ((W3922 (|shen.mod| (|shen.hashkey| V3920) V3921))) (IF (EQL W3922 0) 1 W3922)))

(DEFUN |shen.hashkey| (V3923) (LET ((W3924 (|map| (|lambda| Z3925 (|string->n| Z3925)) (|explode| V3923)))) (|shen.prodbutzero| W3924 1)))

(DEFUN |shen.prodbutzero| (V3926 V3927) (BLOCK () (IF (NULL V3926) (RETURN V3927) (IF (CONSP V3926) (|LET*| ((|V3926/hd| (|hd| V3926)) (|V3926/tl| (|tl| V3926))) (IF (EQL |V3926/hd| 0) (RETURN (|shen.prodbutzero| |V3926/tl| V3927)) (RETURN (IF (|shen-cl.true?| (|shen-cl.greater?| V3927 10000000000)) (|shen.prodbutzero| |V3926/tl| (|+| V3927 |V3926/hd|)) (|shen.prodbutzero| |V3926/tl| (|*| V3927 |V3926/hd|)))))) (RETURN (|shen.f-error| '|shen.prodbutzero|))))))

(DEFUN |shen.mod| (V3928 V3929) (|shen.modh| V3928 (|shen.multiples| V3928 (|cons| V3929 '()))))

(DEFUN |shen.multiples| (V3934 V3935) (BLOCK () (IF (CONSP V3935) (LET ((|V3935/hd| (|hd| V3935))) (IF (|shen-cl.true?| (|shen-cl.greater?| |V3935/hd| V3934)) (RETURN (|tl| V3935)) (RETURN (|shen.multiples| V3934 (|cons| (|*| 2 |V3935/hd|) V3935))))) (RETURN (|simple-error| "implementation error in shen.multiples")))))

(DEFUN |shen.modh| (V3942 V3943) (BLOCK () (IF (EQL V3942 0) (RETURN 0) (IF (NULL V3943) (RETURN V3942) (IF (CONSP V3943) (|LET*| ((|V3943/hd| (|hd| V3943)) (|V3943/tl| (|tl| V3943))) (IF (|shen-cl.true?| (|shen-cl.greater?| |V3943/hd| V3942)) (RETURN (IF (NULL |V3943/tl|) V3942 (|shen.modh| V3942 |V3943/tl|))) (RETURN (|shen.modh| (|shen-cl.subtract| V3942 |V3943/hd|) V3943)))) (RETURN (|simple-error| "implementation error in shen.modh")))))))

(DEFUN |sum| (V3946) (BLOCK () (IF (NULL V3946) (RETURN 0) (IF (CONSP V3946) (RETURN (|+| (|hd| V3946) (|sum| (|tl| V3946)))) (RETURN (|simple-error| "attempt to sum a non-list
"))))))

(DEFUN |head| (V3951) (BLOCK () (IF (CONSP V3951) (RETURN (|hd| V3951)) (RETURN (|simple-error| "head expects a non-empty list
")))))

(DEFUN |tail| (V3956) (BLOCK () (IF (CONSP V3956) (RETURN (|tl| V3956)) (RETURN (|simple-error| "tail expects a non-empty list
")))))

(DEFUN |hdstr| (V3957) (|pos| V3957 0))

(DEFUN |intersection| (V3964 V3965) (BLOCK () (IF (NULL V3964) (RETURN '()) (IF (CONSP V3964) (|LET*| ((|V3964/hd| (|hd| V3964)) (|V3964/tl| (|tl| V3964))) (RETURN (IF (|shen-cl.true?| (|element?| |V3964/hd| V3965)) (|cons| |V3964/hd| (|intersection| |V3964/tl| V3965)) (|intersection| |V3964/tl| V3965)))) (RETURN (|simple-error| "attempt to find the intersection with a non-list
"))))))

(DEFUN |reverse| (V3966) (|shen.reverse-help| V3966 '()))

(DEFUN |shen.reverse-help| (V3971 V3972) (BLOCK () (IF (NULL V3971) (RETURN V3972) (IF (CONSP V3971) (RETURN (|shen.reverse-help| (|tl| V3971) (|cons| (|hd| V3971) V3972))) (RETURN (|simple-error| "attempt to reverse a non-list
"))))))

(DEFUN |union| (V3977 V3978) (BLOCK () (IF (NULL V3977) (RETURN V3978) (IF (CONSP V3977) (|LET*| ((|V3977/hd| (|hd| V3977)) (|V3977/tl| (|tl| V3977))) (RETURN (IF (|shen-cl.true?| (|element?| |V3977/hd| V3978)) (|union| |V3977/tl| V3978) (|cons| |V3977/hd| (|union| |V3977/tl| V3978))))) (RETURN (|simple-error| "attempt to find the union with a non-list
"))))))

(DEFUN |y-or-n?| (V3979) (|LET*| ((W3980 (|pr| (|shen.proc-nl| V3979) (|stoutput|))) (W3981 (|pr| " (y/n) " (|stoutput|))) (W3982 (|shen.app| (|read| (|stinput|)) "" '|shen.s|))) (IF (EQUAL "y" W3982) '|true| (IF (EQUAL "n" W3982) '|false| (PROGN (|pr| "please answer y or n
" (|stoutput|)) (|y-or-n?| V3979))))))

(DEFUN |not| (V3983) (IF (|shen-cl.true?| V3983) '|false| '|true|))

(DEFUN |abort| () (|simple-error| ""))

(DEFUN |subst| (V3989 V3990 V3991) (BLOCK () (IF (|shen-cl.absequal| V3990 V3991) (RETURN V3989) (IF (CONSP V3991) (RETURN (|cons| (|subst| V3989 V3990 (|hd| V3991)) (|subst| V3989 V3990 (|tl| V3991)))) (RETURN V3991)))))

(DEFUN |explode| (V3992) (|shen.explode-h| (|shen.app| V3992 "" '|shen.a|)))

(DEFUN |shen.explode-h| (V3995) (BLOCK () (IF (EQUAL "" V3995) (RETURN '()) (IF (|shen-cl.true?| (|shen.+string?| V3995)) (RETURN (|cons| (|hdstr| V3995) (|shen.explode-h| (|tlstr| V3995)))) (RETURN (|simple-error| "implementation error in explode-h"))))))

(DEFUN |cd| (V3996) (|set| '|*home-directory*| (IF (EQUAL V3996 "") "" (|shen.app| V3996 "/" '|shen.a|))))

(DEFUN |shen.for-each| (V3997 V3998) (BLOCK () (IF (NULL V3998) (RETURN '|true|) (IF (CONSP V3998) (RETURN (LET ((W3999 (FUNCALL V3997 (|hd| V3998)))) (|shen.for-each| V3997 (|tl| V3998)))) (RETURN (|shen.f-error| '|shen.for-each|))))))

(DEFUN |map| (V4000 V4001) (|shen.map-h| V4000 V4001 '()))

(DEFUN |shen.map-h| (V4002 V4003 V4004) (BLOCK () (IF (NULL V4003) (RETURN (|reverse| V4004)) (IF (CONSP V4003) (RETURN (|shen.map-h| V4002 (|tl| V4003) (|cons| (FUNCALL V4002 (|hd| V4003)) V4004))) (RETURN (|shen.f-error| '|shen.map-h|))))))

(DEFUN |length| (V4005) (|shen.length-h| V4005 0))

(DEFUN |shen.length-h| (V4010 V4011) (BLOCK () (IF (NULL V4010) (RETURN V4011) (RETURN (|shen.length-h| (|tl| V4010) (|1+| V4011))))))

(DEFUN |occurrences| (V4017 V4018) (BLOCK () (IF (|shen-cl.absequal| V4017 V4018) (RETURN 1) (IF (CONSP V4018) (RETURN (|+| (|occurrences| V4017 (|hd| V4018)) (|occurrences| V4017 (|tl| V4018)))) (RETURN 0)))))

(DEFUN |nth| (V4023 V4024) (BLOCK () (IF (AND (EQL V4023 1) (CONSP V4024)) (RETURN (|hd| V4024)) (IF (CONSP V4024) (RETURN (|nth| (|1-| V4023) (|tl| V4024))) (RETURN (|simple-error| (|cn| "nth applied to " (|shen.app| V4023 (|cn| ", " (|shen.app| V4024 "
" '|shen.a|)) '|shen.a|))))))))

(DEFUN |integer?| (V4025) (|and| (|number?| V4025) (LET ((W4026 (|shen.abs| V4025))) (|shen.integer-test?| W4026 (|shen.magless| W4026 1)))))

(DEFUN |shen.abs| (V4027) (IF (|shen-cl.true?| (|shen-cl.greater?| V4027 0)) V4027 (|shen-cl.subtract| 0 V4027)))

(DEFUN |shen.magless| (V4028 V4029) (LET ((W4030 (|*| V4029 2))) (IF (|shen-cl.true?| (|shen-cl.greater?| W4030 V4028)) V4029 (|shen.magless| V4028 W4030))))

(DEFUN |shen.integer-test?| (V4034 V4035) (BLOCK () (IF (EQL V4034 0) (RETURN '|true|) (IF (|shen-cl.true?| (|shen-cl.greater?| 1 V4034)) (RETURN '|false|) (RETURN (LET ((W4036 (|shen-cl.subtract| V4034 V4035))) (IF (|shen-cl.true?| (|shen-cl.greater?| 0 W4036)) (|integer?| V4034) (|shen.integer-test?| W4036 V4035))))))))

(DEFUN |mapcan| (V4043 V4044) (BLOCK () (IF (NULL V4044) (RETURN '()) (IF (CONSP V4044) (RETURN (|append| (FUNCALL V4043 (|hd| V4044)) (|mapcan| V4043 (|tl| V4044)))) (RETURN (|simple-error| "attempt to mapcan over a non-list
"))))))

(DEFUN |==| (V4050 V4051) (BLOCK () (IF (|shen-cl.absequal| V4050 V4051) (RETURN '|true|) (RETURN '|false|))))

(DEFUN |bound?| (V4052) (|and| (|symbol?| V4052) (LET ((W4053 (|trap-error| (|value| V4052) (|lambda| Z4054 '|shen.this-symbol-is-unbound|)))) (IF (EQ W4053 '|shen.this-symbol-is-unbound|) '|false| '|true|))))

(DEFUN |shen.string->bytes| (V4055) (BLOCK () (IF (EQUAL "" V4055) (RETURN '()) (RETURN (|cons| (|string->n| (|pos| V4055 0)) (|shen.string->bytes| (|tlstr| V4055)))))))

(DEFUN |maxinferences| (V4056) (IF (|shen-cl.true?| (|shen-cl.less?| V4056 0)) |shen.*maxinferences*| (IF (|shen-cl.true?| (|integer?| V4056)) (|set| '|shen.*maxinferences*| V4056) (|simple-error| "maxinferences expects an integer value
"))))

(DEFUN |inferences| () |shen.*infs*|)

(DEFUN |protect| (V4057) V4057)

(DEFUN |sterror| () |*sterror*|)

(DEFUN |stoutput| () |*stoutput*|)

(DEFUN |string->symbol| (V4058) (LET ((W4059 (|intern| V4058))) (IF (|shen-cl.true?| (|symbol?| W4059)) W4059 (|simple-error| (|cn| "cannot intern " (|shen.app| V4058 " to a symbol" '|shen.s|))))))

(DEFUN |optimise| (V4062) (BLOCK () (IF (EQ V4062 '|+|) (RETURN (|set| '|shen.*optimise*| '|true|)) (IF (EQ V4062 '|-|) (RETURN (|set| '|shen.*optimise*| '|false|)) (RETURN (|simple-error| "optimise expects a + or a -.
"))))))

(DEFUN |os| () |*os*|)

(DEFUN |language| () |*language*|)

(DEFUN |version| () |*version*|)

(DEFUN |port| () |*port*|)

(DEFUN |porters| () |*porters*|)

(DEFUN |implementation| () |*implementation*|)

(DEFUN |release| () |*release*|)

(DEFUN |package?| (V4063) (BLOCK () (IF (EQ V4063 '|null|) (RETURN '|true|) (RETURN (|trap-error| (PROGN (|external| V4063) '|true|) (|lambda| Z4064 '|false|))))))

(DEFUN |fail| () '|shen.fail!|)

(DEFUN |userdefs| () |shen.*userdefs*|)

(DEFUN |optimise?| () |shen.*optimise*|)

(DEFUN |hush?| () |*hush*|)

(DEFUN |system-S?| () |shen.*shen-type-theory-enabled?*|)

(DEFUN |enable-type-theory| (V4067) (BLOCK () (IF (EQ V4067 '|+|) (RETURN (|set| '|shen.*shen-type-theory-enabled?*| '|true|)) (IF (EQ V4067 '|-|) (RETURN (|set| '|shen.*shen-type-theory-enabled?*| '|false|)) (RETURN (|simple-error| "enable-type-theory expects a + or a -
"))))))

(DEFUN |hush| (V4070) (BLOCK () (IF (EQ V4070 '|+|) (RETURN (|set| '|*hush*| '|true|)) (IF (EQ V4070 '|-|) (RETURN (|set| '|*hush*| '|false|)) (RETURN (|simple-error| "hush expects a + or a -
"))))))

(DEFUN |tc| (V4073) (BLOCK () (IF (EQ V4073 '|+|) (RETURN (|set| '|shen.*tc*| '|true|)) (IF (EQ V4073 '|-|) (RETURN (|set| '|shen.*tc*| '|false|)) (RETURN (|simple-error| "tc expects a + or -"))))))

(DEFUN |destroy| (V4074) (PROGN (|set| '|shen.*sigf*| (|shen.unassoc| V4074 |shen.*sigf*|)) V4074))

(DEFUN |shen.unassoc| (V4084 V4085) (BLOCK () (IF (NULL V4085) (RETURN '()) (IF (CONSP V4085) (|LET*| ((|V4085/hd| (|hd| V4085)) (|V4085/tl| (|tl| V4085))) (IF (AND (CONSP |V4085/hd|) (|shen-cl.absequal| V4084 (|hd| |V4085/hd|))) (RETURN |V4085/tl|) (RETURN (|cons| |V4085/hd| (|shen.unassoc| V4084 |V4085/tl|))))) (RETURN (|simple-error| "implementation error in shen.unassoc"))))))

(DEFUN |in-package| (V4086) (IF (|shen-cl.true?| (|package?| V4086)) (|set| '|shen.*package*| V4086) (|simple-error| (|cn| "package " (|shen.app| V4086 " does not exist
" '|shen.a|)))))

(DEFUN |write-to-file| (V4087 V4088) (|LET*| ((W4089 (|open| V4087 '|out|)) (W4090 (IF (STRINGP V4088) V4088 (|shen.app| V4088 "" '|shen.s|))) (W4091 (|pr| W4090 W4089)) (W4092 (|close| W4089))) V4088))

(DEFUN |fresh| () (|shen.freshterm| (|gensym| '|shen.t|)))

(DEFUN |update-lambda-table| (V4093 V4094) (|LET*| ((W4095 (|put| V4093 '|arity| V4094 |*property-vector*|)) (W4096 (|shen.lambda-entry| V4093)) (W4097 (|shen.set-lambda-form-entry| (|cons| V4093 W4096)))) V4093))

(DEFUN |specialise| (V4100 V4101) (BLOCK () (IF (EQL V4101 0) (RETURN (PROGN (|set| '|shen.*special*| (|remove| V4100 |shen.*special*|)) (|set| '|shen.*extraspecial*| (|remove| V4100 |shen.*extraspecial*|)) V4100)) (IF (EQL V4101 1) (RETURN (PROGN (|set| '|shen.*special*| (|adjoin| V4100 |shen.*special*|)) (|set| '|shen.*extraspecial*| (|remove| V4100 |shen.*extraspecial*|)) V4100)) (IF (EQL V4101 2) (RETURN (PROGN (|set| '|shen.*special*| (|remove| V4100 |shen.*special*|)) (|set| '|shen.*extraspecial*| (|adjoin| V4100 |shen.*extraspecial*|)) V4100)) (RETURN (|simple-error| "specialise requires values of 0, 1 or 2
")))))))

(DEFUN |absolute| (V4102) (|set| '|*absolute*| (|cons| V4102 |*absolute*|)))

(DEFUN |unabsolute| (V4103) (|set| '|*absolute*| (|remove| V4103 |*absolute*|)))

