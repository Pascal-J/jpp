Note 'continations' 
A continuable function form is one where all parameters have default values, and no external state accessed.
An ideal continuable function is in the y arg to ^: ie. all arguments returned.
)

loc_z_=: (,&'_'@[ ,&'_'@, ":@>@])"1 0 boxopen
locs_z_ =: 1:: m loc 18!:5 ''
deflt =: 1:: dflt&( m
yield =: (u aar n tiD ,&< y `:` x u aar tiD n tiD ,&< y  bb. daFx  NB. independent params from resut
yval =: unar@{:
yieldR =: (a =. tr u aar ti y  `u aar a tiD ,&< a `:` a =. tr x u aar tiD ti y  ` x u aar tiD a tiD ,&< a  bb. 1..  NB. new params are resut
yieldD =: ([: ti u ti (dflt&n)   ,&< y `:` x u aar tiD dflt&n tiD ,&< y  bb. daFx NB. overridable continuation
continueR =:  tr ([ ,&<~ [ar oa..  _1} ]) ]  NB. rebuilds/updates yield structure
continueD =: a:&$: : ( 4:: (tr y) x   NB. dyad overrides params.  up to function to return a continuation or not.
continueDT =: a:&$: : ((  b =. (2 tr@{ y) x ` a =. (tr y) x `(dflt&b  ar 2} y) ,&< a  bb. 4..  NB. fixes default vals to passed vals.  a: would be constant function. 
continueDR =: a:&$: : ((  a =. (tr y) x `(dflt&a  ar 2} y) ,&< a  bb. 4..  NB. fixes default vals to return value(s).

ticket=: >: 0 yield 0

assert 1 -:  1. continueR 0 {:: ticket

Note 'simplified version'
just keep functional gerund, and read return value as last item
gerund should use [: fork for monad to be compatible with dyad.
)
yld =: (u ti (u y) `:` x ti u ti (x u y)  bb.1..
yld =: (u ti  y `:` x ti u ti y  bb.1..
closure=: tr ([ar oa.. _1} ]) ]

NB.SNR =:  'FNAME =. m locs'&; '1 :' oa@:cutLF@:(' 0 :' Cloak)@]
NB. SNR =:  ('FNAME =. m locs';'(FNAME) =)&strbracket '1 :' oa@:cutLF@:(' 0 :' Cloak)@]
SNR1_z_ =:  ( (m locs) =: (('FNAME =. ' ,  quote m locs) &; '1 :' oa@:cutLF@:(' 0 :' Cloak)@]) n 2...
SNR3_z_ =:  ( (m locs) =: (('FNAME =. ' ,  quote m locs) &; '3 :' oa@:cutLF@:(' 0 :' Cloak)@]) n 2...
SNR4_z_ =:  ( (m locs) =: (('FNAME =. ' ,  quote m locs) &; '4 :' oa@:cutLF@:(' 0 :' Cloak)@]) n 2...
test2 SNR1.: 2 + y

NB. ticket SNR3.: closure assignwith FNAME~ [ (FNAME) =:  >: yld 0

ticket=: >: yld 0
cont =: _1 unar@{ closure assignwith  NB. y arg is name that holds simple continuation.  updates name value and returns val
assert 1 -: cont:: ticket

NB. transitive closures, have 1 branch determine next function, the other return result
cycle SNR4.: (x ar, (< FNAME) ,(>: y)ar) ,&< (y{x)
cycle SNR4.: (x  FNAME aar tiD ((#x) | >: y) tiD ) ,&< (y{x)

mycycle =: (10 + i.4) cycle 0
cont2 =: 1 {:: tr@0. assignwith  NB. for var that holds gerund(new transformation) ,&< result
assert 11 -: cont2:: mycycle