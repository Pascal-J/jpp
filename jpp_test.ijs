NB.jpp2.ijs must be loaded first
 MYDIR =: getpath_j_  '\/' rplc~ > (4!:4<'thisfile'){(4!:3)  thisfile=:'' NB. boilerplate to set the working directory

require MYDIR, 'continuations.ijs'

cocurrent:: jpp
test =: 4:: x + y
test2 =: (-&0. * +&1.) (if. y >22 do. *: y else. y end. 3... at  NB. 0. 1. are dyadic with &0.  0.~ gets 0 element from x

pD 1 test 5 3 test2 2 1

pD 2 (+ 5&*@:(5 8"_) `1 3"_`]  gg.} i.5

cocurrent:: base

test =: 3.: : 4.:
x + y
)
2 test y
)

pD timespacex (. test"0 i.1000

C c =:: 100000 1000
pD |: (bb Cloak.. every cutLF  0.:)  ((a: , ]) , ( 1.~ ; timex each@:] [ ".@0.~)"1 _) (a i. a` a i.b`b i. a`a e. b` b e. a`a +/@:e. b`a (e. i. 1:) b`~.a`~:a`/:a`/:~a   bb.
b=: (<.-:#a)+ c ?. c [ a=: C ?. C ` intsr
b=: (c?.#a){a [ a=: C ?@$ <:2^63 `  intbr
b=: (c?.#a){a [ a=: >,.~":each <"0 [C ?@$ <:2^63 ` char
b=: 0.1+(c?.#a){a [ a=: 0.1+C ?@$ <:2^63  `    float
)


pD  + - (u ( u` 1... (u`) 1...


leadswnamecolon =: _1 (~: {.)"1 '\A[[:alpha:]][[:alnum:]_]+:\Z' rxmatch 0.@:;:
funcdict =:  ((] {.~ i.&':')each@:{. , }. ,~  (] dltb@}.~ >:@i.&':')each@:{.)each@:(] <;.1~  1 A  0},at leadswnamecolon every)@:cutLF ({. (, <) }.)every at
locsuffix =: 2:: n , forlast 1 m locs
FN =: 2 : '(m  locsuffix ''DICT'') =: funcdict 0 : 0'  NB. must be quoted to prevent multiline trigger.

] testf  FN.. ML
head asdf
DOC: asdfasdf
FN: +/ % #
TEST:
1
2
JUNK:
)