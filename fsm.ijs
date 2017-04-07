
NB.http://code.jsoftware.com/wiki/User:Pascal_Jasmin/sequential_machine_intro

Note 'fsm intermediate language'
see above link for info.
state description language.  use names instead of state numbers.  Letter codes for actions.
state names that include number can be copied up to a currently hard coded constant (see copynumeric) number of times.
state transitions can be accessed with =(same core name) + higher number same prefix or - lower number.
A state suffix code (upper case letter) can be used for programatic copy of non-suffixed code. And as special markers for tracking actions that don't exist within fsm.
State suffix code 'S' seems generally useful way to go to a variation of the state where no word has been started yet.  (Code may be written to copy transitions from base-state with modifications)
see parens noun created from multiline definition below. (tough you may wish to set the depth parameter in copynumeric to 10 or so first)
parenw_z_ uses the example fsm to recursively transform a parenthesized phrase into a tree.  The component functions and fsm are not hard coded to '()' tokens, though final is.
makem creates "mj" from boxes with other as 0 index (column) in sj.
)

cocurrent 'fsm'
parsenum =: maybenum each@:((0 `> +.@:". each cut every (1j1 2j1;1j0 2j2;1j2 2j0 bs.  `a. e. '0123456789' C:: ; &;:

numerify =: 0&".^:(2 = 3!:0)
maybenum =: 0&".^:(] -:&linearize ":@:numerify)

addhalt =: ] , 'halt' ; (< 'haltH') #~ <:@{:@$
parsestate =: (}: ; {:) each@] forfirstdrop 1 (] ; parsenum) each@] forfirst 1 at"1 @:addhalt
parsestate =: (}: ; {:) each@] forfirstdrop 1 (] ; parsenum) each@] forfirst 1 at"1 

copynumeric =: 250 A 1::  ] , (] }.@{~ <@[ i.~ 0 { 0."1) ;"1~ [: (;@:(":each) ; ])"1  m (  0. ;"1 0 ((>:@] +  i.@-) 1.)) parsenum@[

replpme =: ( rplc&( '=' ; ( ;@:(": each)@] 2 {. }. 0. y)) leaf@:] forfirstdrop 1 rplc&(( '-' ; 1.@0. ,&": (<:@:(2.@0. :: 0:))) y) leaf@:] forfirstdrop 1  rplc&(( '+' ; 1.@0. ,&": (>:@:(2.@0. :: 0:))) y) leaf@:] forfirstdrop 1 y 3... "1

chkunreferenced =:  ( 0 ,@:({"1) 0."1) (] #~ -.@e.~) [: ~.@, 0&{ every@:}."1)
fixunreferenced =: (chkunreferenced ( rplc&(x (, <)"0 1 'halt') leaf@] forfirstdrop 1"1  y   4... ]) linearize leaf at
chkunaccessed =: (<'start') -.~ ( 0 ,@:({"1) 0."1) ([ #~ -.@e.) [: ~.@, 0&{ every@:}."1)
toactioncodes =:  ( < ;/ 'NSWwVvH' ) <@i. amdt 1 each  forfirstdrop 1"1 ]  NB. bad code 7 if any invalid
tostatecodes =:  ((0 { 0.)"1 < at <@i. amdt 0 each  }."1) > at > at


parens =:  toactioncodes fixunreferenced replpme 'p2' copynumeric parsestate@:addhalt cut every@:cutLF 0.:
start p0S p1SS haltH 
p0 =N +Sw -H 
p1 =N +N startw 
p1S p1S p2S startN 
p2 =N +N -N  
)


fsmmakem_z_ =: linearize@:>:@i.@#  >./^:(1 < #@$)@:* (a. e."1  ,@>)"0

Note '2 approaches'
parenw uses the boxword function code 0:  transforms input, and needs to insert nulls between )(, and drops () bc it cannot find them would need to insert nulls there too.

parenwf2 uses function code 2:  more flexibly analyses gaps to get it correct.  
Implementation could find multiple consecutive ()() (by considering the gap count rather than just checking for a gap), but simply collapses them into 1.

f2 is slower than boxed approach when only 1 null inserted between )( and no nulls added between ().  But boxed approach is slowed down when inserting long nulls.
f2 is equal speed when depth reaches 33 
 parenwf2A_fsm_ fixparen '(x)zbsdfghgdfg' {~ ?. 10000 $ 7
an "incorrect" parenw version
  parenw_fsm_ fixparen '(x)zbsdfghgdfg' {~ ?. 100 $ 7
)


parenw0 =: 1:: (] (a: , ])^:(({.m) = {.@[) (0; (tostatecodes parens);(fsmmakem ;: m))&;: :: (a:"_)) 
parenw3 =: 1:: ( (2; (tostatecodes parens);(makem ;: m))&;: :: (a:"_)) 
parenw1 =: '()' parenw0

NB.parenw2 =: [: ]`($:^:('(' e. ]) each) AltM parenw1
parenw2 =: [: a:"_^:((1 {. a.) -: >)`($:^:('(' e. ]) each) AltM  parenw1

gapsf2 =: (] , 0. - 2.)"1@:(] ,. 0 (, }:) >:@+/"1)
gapsf4 =: ( ] ,"1 0 A  0 (, }:@:,) 2."1)@:((] , 0. - 3.)"1@:(] ,. 0 (, }:) ([: >:@+/ 2&{.)"1)) NB. func code 4, adds 
NB.gapsf4 =: (] , 0. - 3.)"1@:(] ,. 0 (, }.) ([: >:@+/ 2&{.)"1)
ifgapapnd0 =: _4 }. ,`(0 0 0 0 , ,)@.(0 < {:@[)/@:((,:0 0 0 0) ,~ gapsf2)
NB.ifgapapnd0 =: _4 }. ,`((0 0 0 0 #~ {:@[) , ,)@.(0 < {:@[)/@:((,:0 0 0 0) ,~ gapsf2)
getfromf2 =: 4:: _4 (x <@:{~ (+ i.)/@(2&{.))\ y NB. x is original parsed string, y is f=2 fsm output that may have additional columns.
getwgapf2 =: 4:: ;@:(((a: #~ 3&{) , x <@:{~(+ i.)/@(2&{.))&.>)@:(<"1) gapsf2_fsm_ y
fillgapf2just1 =: 1:: ] getfromf2_fsm_ ifgapapnd0_fsm_@:u
fillgapf2 =: 1:: ] getwgapf2_fsm_ u


fsmmakes_z_ =: 1:: > +.@:". each cut every m bs
NB. default sj is 3draw, 1d boxed rows of complex, or string to be cut on `or LF then on ;
fsm_z_ =: ((> +.@:". each cut every (1j1 2j1;1j0 2j2;1j2 2j0 bs.`fsmmakem ''''   `0`0 _1 0 _1 gg. dfltG) (1:: '`s m f ij' =.  m if. (L. m) +. 2 = 3!:0 m do. m =. fsmmakem m end. if. 3 ~: #@$ s do. if. 1 = L.s do. s =.> +.@:". each cut every s else. if. LF e. s do. s =. cutLF s else. s =. s bb end. s =. tostatecodes toactioncodes fixunreferenced replpme  parsestate cut every s end. end. (f;s;m;ij)&;: 

parenwf2 =: ( tostatecodes_fsm_ parens_fsm_ ) ti ( fsmmakem '()' ) ti 2 fsm fillgapf2  NB.1:: ] getfromf2 ifgapapnd0@:u
parenw_z_ =: parenwf2A =: [: ]`($:^:('(' e. ]) each) AltM  parenwf2_fsm_  NB. unfortunate hard code of ( need mRS for single def.
NB. parenw_z_ =: parenw2_fsm_@: (rplc&(')('; 41 0 40 {a.)) NB. could insert 50 or 100 nulls to make improbable collision.

NB. COLS: other escape delim 
escapes =:  tostatecodes toactioncodes fixunreferenced replpme  parsestate cut every@:cutLF 0.:
s   wS escS sN 
esc wS wS wS 
w   wN escw sw 
)

escapes2 =:  tostatecodes toactioncodes fixunreferenced replpme  parsestate@:addhalt cut every@:cutLF 0.:
s   wS escS sN 
esc haltS wN wN 
w   wN escN sw 
)

escapes3 =: tostatecodes toactioncodes fixunreferenced replpme  parsestate cut every@:cutLF 0.:
s   wS escS 
esc wS wS 
w   wN escw  
)
NB. esc_z_ =: 1::  ;@:(escapes3_fsm_ ti (fsmmakem_fsm_ {.m) ti 0  fsm^:(0 <#)) each@:( escapes2_fsm_ ti (fsmmakem_fsm_ m) ti 2  fsm fillgapf2_fsm_)
esc_z_ =: 1::  (escapes3_fsm_ ti (fsmmakem_fsm_ {.m) ti 1  fsm^:(0 <#)) each@:( escapes2_fsm_ ti (fsmmakem_fsm_ m) ti 2  fsm fillgapf2_fsm_)

NB. escD family not necessary as doesn't speed up over variable adverb.
escD1 =: escapes2_fsm_ ti (fsmmakem_fsm_ '\+') ti 2  fsm fillgapf2_fsm_  NB. replace with default esc-sep
escD2 =: (escapes3_fsm_ ti (fsmmakem_fsm_ '\') ti 1  fsm
escD_z_ =: escD2_fsm_^:(0 <#) each@:escD1_fsm_

delims =: tostatecodes toactioncodes fixunreferenced replpme  parsestate cut every@:cutLF 0.:
s   wS sN 
w   wN sw  
)

cutA_fsm_ =: 1:: delims_fsm_ ti (fsmmakem_fsm_ m) ti 0  fsm
cut_fsm_ =: ' '&$: : (4:: delims_fsm_ ti (fsmmakem_fsm_ x) ti 0  fsm y) :.joinstring NB. allows grouped cut on either symbol passed as box.
NB. cut_fsm_ =: ' '&$: : (4:: delims_fsm_ ti (a. e. x) ti 0  fsm y) :.joinstring
cut_fsm_ =: ' '&$: : (4 : '(0;(2 2 2$1 1 0 0 1 0 0 3);(a. e. x))&;: y') :.joinstring
 
cocurrent 'base'

assert (0$0);'\a';'bd\+';(,'f');(0$0);(0$0);'gfaa+d';(0$0);(0$0);'+ggg' -:  ;@:(' escapes3_fsm_ ` fsmmakem_fsm_ ''\'' ` 0' gg fsm^:(0 <#)) each ' escapes2_fsm_ ` fsmmakem_fsm_ ''\+'' ` 2' gg fsm fillgapf2_fsm_ '+\\a+bd\\\++f+++gfaa\+d+++\+ggg'
assert (0$0);'\a';'bd\+';(,'f');(0$0);(0$0);'gfaa+d';(0$0);(0$0);'+ggg' -:  '\+' esc '+\\a+bd\\\++f+++gfaa\+d+++\+ggg'
assert ((<0$0),(<'sdfsdf'),(<0$0),(<(<0$0),(<(<,'s'),(<(0$0);(,'d');,'f'),<,'s'),(<' dfgdfg'),<' safd dfg'),<'sdf') -: parenw '(sdfsdf)((s((d)f)s) dfgdfg( safd dfg))sdf'

