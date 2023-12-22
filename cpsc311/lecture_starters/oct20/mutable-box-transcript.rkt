Welcome to DrRacket, version 8.6 [cs].
Language: Determine language from source; memory limit: 256 MB.
> (define a (box 120))
> a
'#&120
> (define b a)
> b
'#&120
> (unbox a)
120
> (unbox b)
120
> (set-box! a 220)
> (void)
> a
'#&220
> (unbox a)
220
> (unbox b)
220
> (define w (box a))
> w
'#&#&220
> (unbox w)
'#&220
> (equal? (unbox w) a)
#t
> (define y (box 220))
> y
'#&220
> (equal? (unbox w) y)
#t
> (eq? (unbox w) y)
#f
> (eq? (unbox w) a)
#t
> (set-box! a 320)
> a
'#&320
> y
'#&220
> (unbox w)
'#&320
> (set-box! a a)
> a
#0='#&#0#
> (unbox w)
#0='#&#0#
> (eq? (unbox w) a)
#t
> (equal? (unbox w) a)
#t
> (define z (box (void)))
> z
'#&#<void>
> (set-box! z z)
> z
#0='#&#0#
> a
#0='#&#0#
> (equal? z a)
#t
> (eq? z a)
#f
> (set-box! z 4)
> (equal? z a)
#f
> w
'#&#0=#&#0#
> a
#0='#&#0#
> (equal? w a)
#t
> (define kenny (box w))
> kenny
'#&#&#0=#&#0#
> w
'#&#0=#&#0#
> (equal? kenny w)
#t
> 