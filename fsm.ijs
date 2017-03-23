
NB.http://code.jsoftware.com/wiki/User:Pascal_Jasmin/sequential_machine_intro

Note 'fsm intermediate language'
see above link for info.
state description language.  use names instead of state numbers.  Letter codes for actions.
state names that include number can be copied up to a currently hard coded constant (see copynumeric) number of times.
state transitions can be accessed with =(same core name) + higher number same prefix or - lower number.
A state suffix code (upper case letter) can be used for programatic copy of non-suffixed code. And as special markers for tracking actions that don't exist within fsm.
State suffix code 'S' seems generally useful way to go to a variation of the state where no word has been started yet.  (Code may be written to copy transitions from base-state with modifications)
see parens noun created from multiline definition below.
parenw_z_ uses the example fsm to recursively transform a parenthesized phrase into a tree.  The component functions and fsm are not hard coded to '()' tokens, though final is.
makem creates "mj" from boxes with other as 0 index (column) in sj.
)

cocurrent 'fsm'
parsenum =: maybenum each@:((0 `> +.@:". each cut every (1j1 2j1;1j0 2j2;1j2 2j0 bs.  `a. e. '0123456789' C:: ; &;:

numerify =: 0&".^:(2 = 3!:0)
maybenum =: 0&".^:(] -:&linearize ":@:numerify)

addhalt =: ] , 'halt' ; (< 'haltH') #~ <:@{:@$
parsestate =: (}: ; {:) each@] forfirstdrop 1 (] ; parsenum) each@] forfirst 1 at"1 @:addhalt

copynumeric =: 20 A 1::  ] , (] }.@{~ <@[ i.~ 0 { 0."1) ;"1~ [: (;@:(":each) ; ])"1  m (  0. ;"1 0 ((>:@] +  i.@-) 1.)) parsenum@[

replpme =: ( rplc&( '=' ; ( ;@:(": each)@] 2 {. }. 0. y)) leaf@:] forfirstdrop 1 rplc&(( '-' ; 1.@0. ,&": (<:@:(2.@0. :: 0:))) y) leaf@:] forfirstdrop 1  rplc&(( '+' ; 1.@0. ,&": (>:@:(2.@0. :: 0:))) y) leaf@:] forfirstdrop 1 y 3... "1

chkunreferenced =:  ( 0 ,@:({"1) 0."1) (] #~ -.@e.~) [: ~.@, 0&{ every@:}."1)
fixunreferenced =: (chkunreferenced ( rplc&(x (, <)"0 1 'halt') leaf@] forfirstdrop 1"1  y   4... ]) linearize leaf at
chkunaccessed =: (<'start') -.~ ( 0 ,@:({"1) 0."1) ([ #~ -.@e.) [: ~.@, 0&{ every@:}."1)
toactioncodes =:  ( < ;/ 'NSWwVvH' ) <@i. amdt 1 each  forfirstdrop 1"1 ]  NB. bad code 7 if any invalid
tostatecodes =:  ((0 { 0.)"1 < at <@i. amdt 0 each  }."1) > at > at
makem =: linearize@:>:@i.@#  >./^:(1 < #@$)@:* (a. e."1  ,@>)"0


parens =:  toactioncodes fixunreferenced replpme 'p2' copynumeric parsestate cut every@:cutLF 0.:
start p0S p1SS haltH 
p0 =N +Sw -H 
p1 =N +N startw 
p1S p1S p2S startN 
p2 =N +N -N  
)


parenw0 =: 1:: (] (a: , ])^:(({.m) = {.@[) (0; (tostatecodes parens);(makem ;: m))&;: :: (a:"_)) 
parenw1 =: '()' parenw0

NB.parenw2 =: [: ]`($:^:('(' e. ]) each) AltM parenw1
parenw2 =: [: a:"_^:((1 {. a.) -: >)`($:^:('(' e. ]) each) AltM  parenw1

cocurrent 'z'
parenw =: parenw2_fsm_@: (rplc&(')('; 41 0 40 {a.)) NB. could insert 50 or 100 nulls to make improbable collision.

assert ((<0$0),(<'sdfsdf'),(<0$0),(<(<0$0),(<(<,'s'),(<(0$0);(,'d');,'f'),<,'s'),(<' dfgdfg'),<' safd dfg'),<'sdf') -: parenw '(sdfsdf)((s((d)f)s) dfgdfg( safd dfg))sdf'