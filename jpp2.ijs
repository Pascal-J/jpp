NB. require 'debug/dissect'  NB. convenience

Note 'jpp'
extends J with auto parenthesizing, and quoting for custom parsers.  User defined primitives.
All enhancements are at the parsing level, with no code execution itself.

recommend the following 2 userkeys.cfg lines, and load of this file in config/startup.ijs

F12;0;jpp line;DoWithMacro@findline_jpp_ ''
F11;0;jpp section;DoWithMacro2@findline2_jpp_ ''

jpp replaces the load command (core renamed to loadj and requirej).  Creates temp file in same dir to work with code tricks to find file's directory.
f12 runs a line like ctrl-R in an ijs edit window or a console line.  hope to add a version that adds to command log.
F11 will run enclosing multiline definition.  Not required to be at top.  works with multiple multilines.

"fancy" ways of initiating multiline definitions should use the constant ML instead of 0 so that multiline defs are detected.
the verb nummultiline should be adjusted if you have a new undetectable means of producing multiline definitions.

see BRAILLECODES for new primitives.  
). or word. adds ) on left, and quotes enclosing () auto adding ( if needed for balance.
): or word: adds ( on right and quotes that enclosure.
).: or word.: does both.
word.. quotes token to left
word:: quotes token to right
(. quotes self paren group
Brailcodes can generate word or paren codes, and are processed in first pass.
Passes operate left to right except for (.

)

Note 'Parser helpers'
A or Aa are useful delimeters.  Idendity adverb 2 A 3 produces 2 phrase tokens.
gg produces gerunds without internal parenthesizing.  cut on ` and implied parentheses within each cut.
bb is simple cut on `
"macros" to auto quote sections of code/lines permits easier custom parsing
)

Note 'double/multi adverbs'
provides strand notation, and gerund constructors.
ti replaces `.  nouns get encoded as noun ars
tie is similar but allows nesting gerunds.
tieA  will encode a string, if it is a name of a verb or modifier, or evaluates to a modifer, as a modifier/name
ar and aar are the 2 atomic representation adverbs.
tiD tieD tieAD are double adverb versions of the above.
)

coclass 'jpp'
 MYDIR =: getpath_j_  '\/' rplc~ > (4!:4<'thisfile'){(4!:3)  thisfile=:'' NB. boilerplate to set the working directory

require MYDIR, 'doubleadverb2.ijs'



cocurrent 'z'

ML_z_ =: 0

NoteV_z_ =: 1 : '(0 (0 :)) label_. 0 (m :)'
nummultiline =: 3 NoteV 
 This is a double multiline definition.  top comment section followed by a (3 :) def.
 nummultiline returns count of expected terminating parentheses to a line.
 define, : 0 , ML, ML_z_, Note, NoteV are scanned, but combinations are excluded from double counting: ML : 0, ML define
 though the combinations are bad form, ML intended to be used only as 0 for n in m : n
)
t=. ;: :: 0: y
if. t-:0 do. 0 return. end.
e =. (;: 'define ML ML_z_') +./@:(=/) t
e =. e +. (,each ':0') E. t
e =. e - (;:'ML : 0')  E. t
e =. e - (;:'ML : ML')  E. t
e =. e - (;:'ML define')  E. t
+/ e + 2 * (< 'NoteV') = t
)


nummultiline =: 3 : 0
t=. ;: :: 0: y
if. t-:0 do. 0 return. end.
if. (<'Note') = {.t do. 1 return. end.
e =. (;: '0.: 1.: 2.: 3.: 4.: define ML') +./@:(=/) t
NB.e =. (;: '0:: 1:: 2:: 3:: 4:: define ML') +/@:(=/) t
e =. e +. (,each ':0') E. t
+/ e + 2 * (< 'NoteV') = t
)

 Y =: (&{::)(@:])
 X =: (&{::)(@:[)
del1 =: ] ({.~ , [ }.~ >:@]) i.~
delitem =: ;@:((0 X {. ]) , (0 X + 1 X) }. ])  NB. idx len :X  str :Y
insitem =:  (1 X {. ]) , 0 X , ( 1 X) }. ] NB.  item idx :x list :Y
replitem =: (0 X ; (1;0) X) insitem 1 X delitem ] NB.  item ; idx len  :x list :Y

swapchar =: ( ,"1 0 {~("1 0) 2 <./@:+ _1 _2 * ="0 1)
permbase2 =: ([ ({.@] #:&> (- leaf {:)) ] ((2 #~ >:@]) ; {~) 1 i:~ >:)&(0 , +/\ 2 ^ >: i.8)
permbase2 =: }.@(2 #.inv@+ ]) :. (2 -~ 2 #. 1,[)
um =: ')('&$: :(#`((] 0:`]@.(#@[ > ]) i:&0) + {: (i:~ }:) ] [`(}.~)@.(#@[ > ]) i:&0)`(_1 i.~ ])@.(*@{:)`(_1 i.~ ])@.(_1&e.)@:(+/\)@:(2 (] * >) 2 <:@* i.))
JNAME=: '[[:alpha:]][[:alnum:]_]+'
CONTROLW =: ;: 'if. else. do. end. for. return. while. break. case. elseif.  select. fcase. try. catch. catcht. whilst. throw. assert. continue.'
CONTROLWRE =: rxcomp '|for_[[:alnum:]]+\.|label_[[:alnum:]]*\.|goto_[[:alnum:]]+\.' ,~ '|' joinstring '\.',~ leaf }:each  CONTROLW
QUOTESRE =: rxcomp '''(?:[^'']+|'''')+'''
matchstartend =: ((1 {:: [) -: ] {. 1 -@#@{:: [ )*. (0 {:: [) -: ] {. 0 #@{:: [
fixparenl =: '()'&$: :  ( ([ $: {.@[ , ])^:(_1 e. depth) )
fixparen =: '()'&$: :  ([ ({:@[ ,~ ])^:({:@:depth) fixparenl)


take =: ((*@[ * |@[ <. #@:]) {. ]) 
forfirst =: 2 : '(v }. ]) ,~^:(0 < #@[) [ u v take ]'
forfirstE =: 2 : 0 
i =. v"_ y
((i }. ]) ,~^:(0 < #@[) [: u i take ]) y
:
i =. x v"_ y
x ((i }. ]) ,~^:(0 < #@[) [: u i take ]) y
)

formiddle =: 2 : 0 NB. v results in starti, len
i =.  v"_ y
(( (({: i) +^:(0 > [) {. i) take ]) ,^:(0 < #@[) ((({: i) -~^:(0 > [) +/ i) }. ]) ,~^:(0 < #@[) [: u ((#y) (] #~ >) (+ *@] * i.)/ i) {  ]) y
:
i =. x v"_ y
x (( (({: i) +^:(0 > [) {. i) take ]) ,^:(0 < #@[) ((({: i) -~^:(0 > [) +/ i) }. ]) ,~^:(0 < #@[) [ u ((#y) (] #~ >) (+ *@] * i.)/ i) {  ])y
)

forfirstdrop  =: 2 : '(v take ]) ,^:(0 < #@[) [ u v }. ]'
forfirstlogm =: 2 : ' u  forfirst (n * 2 >:@>.@^. #@])'
forlast =: 2 : '(-@(v"_) }. ]) ,^:(0 < #@[) [ u -@(v"_) take ]'

depth2 =: '()'&$: :([: (+/\ + _1&=) =/\@(''''&~:)@] * 1 _1 0 {~ i.)
cutCRLF =: }:^:(CR = {:) each@:cutLF
cut =: ' '&$: :([: -.&a: <;._2@,~) :. joinstring
scriptdepth =: 3 : 0
skip =. 0
o =. ''
for_i. y do.
 if. i = ;: ')' do.    skip =. <: skip  [ o =. o , _1
   elseif. skip > 0 do. o =. o , 0
   elseif. do.  o =. o , skip =. nummultiline > i end.
end.
o
)
amendwhere =: 'v (] u@:{~ I.@[)`(I.@[)`]} ]' daF
scriptdepthmarks =: (-.@] >. (0 2&E.}:@:+. 0 1&E.)@:(0&,))@:(+/\ + _1&=)@:scriptdepth
NB. scriptdepthmarks =: (-.@] >. (0 2&E.}:@:+. 0 1&E.)@:(0&,))@:(+/\)@:scriptdepth
scriptdepthmarks =: (-. >.  2 ((0 = [) *. 0 < ])/\ 0&,)@:(+/\ + _1&=)@:scriptdepth
scriptdepthmarks2 =: (-. >.  2 ((0 = [) *. 0 < ])/\ 0&,)@:(+/\ )@:scriptdepth

jppf =: jpp each scriptdepthmarks amendwhere LF&joinstring at@:cutCRLF
jppf2 =: jpp each scriptdepthmarks amendwhere LF&joinstring at@:cutCRLF
NB.jppf =: jpp each (-.@] >. 0 }:@:, 0 2&E. +. 0 1&E.)@:(+/\ + _1&=)@:scriptdepth amendwhere > at@:cutCRLF
NB. y is a string containing parentheses, or (a string containing delimiters);(start,end delimiters)
NB. x is [possibly a list of] indexes into y
NB. Result is table of (start,end) for the minimal enclosing parentheses
NB. for each point of x.
mep =: ((( 1 _1  0 {~ '()' i. ]) toG) dfltG) (1 : 0)("1)
:
NB. if. L. y do. 'y delims' =. y else. delims =. '()' end.
u =. m 5!:0
NB. Assign nesting level to each character; make level of ) match (
nlevel =. (+/\ + _1&=) class =. u y
NB. Create records for sorting:
NB. First, the parentheses:  level,position,_1 or #x based on class   (to sort in order start,ref,end)
NB. Then, the requested indexes:  level,position,(item# in x)
lpc =. (({&nlevel ,. ] ,. (0 _1,#x) {~ ({&class)) I. class ~: 0) , ({&nlevel ,. ] ,. i.@#) x
NB. Sort into order; assign interval number to each point; select interval numbers for points
NB. of interest; sort back into order of the original x
(((-.@(e.&(_1,#x)) (# C.^:_1 (# +/\@:(_1&=))) ])@] { 0 0 , _2 ]\ ((# 1&{"1)~ e.&(_1,#x)))  (2&{"1)) /:~ lpc
)
parG =: ( 1 1 1 _1 _1 _1 _1 0 {~ (;:'( (. (: ) ). ): ).:') i. ]) mep

Note 'order'
fixparen, do (. (:
add parens from other braille
fixparen, quote items left to right priority, remove braille.
)
NB. code, pre (or self trans) , post, transform
BRAILLECODES =: TAB cut every cutLF 0 : 0
3...	 	(3 :)	 quote
2...	 	(2 :)	 quote
1...	 	(1 :)	 quote
4...	 	(4 :)	 quote
0...	 	(0 :)	 quote
3..	(3 :)
2..	(2 :)	 
1..	(1 :)	 
4..	(4 :)	 
0..	(0 :)	 
3::	3 :	 	 quote
2::	2 :	 	 quote
1::	1 :	 	 quote
4::	4 :	 	 quote
0::	0 :	 	 quote
3.:	(0 : ML) (3 :)
0.:	(0 : 0)
1.:	(1 : 0)
2.:	(2 : 0)
4.:	(4 : 0)
0.	(0 {:: ])
1.	(1 {:: ])
2.	(2 {:: ])
3.	(3 {:: ])
4.	(4 {:: ])
_1.	(_1 {:: ])
@..	gg. if
^::	gg. pow
C::	gg. cc::
C:	gg. cc:
T.	timespacex	 	quote 
=..	 	=.	quote
=::	 	=:	quote
)



parenloopC =: 3 : 0
a =. ;: y
for_i. i. # (  (;: '). ): ).:') I.@:(+./@:(="0 1)) ])  a  do.
   n =. i { (  (;: '). ): ).:') I.@:(+./@:(="0 1)) ])  a
 if. (;: '): ).:') e.~  a {~  n do. a =. (< , '(') ; (>: n) F insitem a  end.
 if. ((;: '). ).: ')) e.~ a {~ n do. 
    a=. (< , ')') ; ( ( (] , >:)  n{b) {:@I.@E. 0 , n {. b =.  +/\@:( 1 1 1 _1 _1 _1 _1 0 {~ (;:'( (. (: ) ). ): ).:') i. ]) a) F insitem a
 end.
end.
;: inv a
)

JNAME2 =: ('\A' , JNAME , '([\.:]+)\Z')
parenloopW =: 3 : 0
NB.for_i. pD n =. {:"2 (JNAME , '([\.:]+)')&rxmatch every  a =. ;: fixparen y do.
for_i.  n =. {:"2 JNAME2&rxmatch every  a =. ;:  y do.
 if. (_1 ~: {. i) do.
  if.  0 = CONTROLWRE {.@rxE i_index {:: a do.
   t =. < ((i_index {:: a) {~ (+ i.)/) i
NB.   if.  (;: '..') e.~ < ((i_index {:: a) {~ (+ i.)/) i do.  a =.  _2 (' aar' ,~ quote@:}.) each amdt (<: i_index) a else.
    if.  (;: '..') e.~ t do.  a =.  _2 }. each amdt ( i_index) quote each@:] amdt (<: i_index) a 
    elseif. (;: '::') e.~ t do.  a =. _2 }. each amdt ( i_index) quote each@:] amdt (>: i_index) a elseif. do.
      if. (;: ': .:') e.~ t do.  a =. '(' ,~ each amdt i_index a end.
      if. (;: '. .:') e.~ t do.  a =. ')' , each amdt i_index a end. 
end. end. end. end.
;: inv a
)

parenloopB =: 3 : 0
NB.for_i.   a =. ;: fixparen y do.
for_i.   a =. ;: y do.
 if. i e. {."1 BRAILLECODES  do. ndx =. i i.~ {."1 BRAILLECODES
  if. a: -: 3 { ndx { BRAILLECODES do. a =.  (1 { ndx { BRAILLECODES) [ amdt i_index a else.
     if.  '' -.@-: dltb 1 {::  ndx { BRAILLECODES do.   a =. '(' ,~ each amdt i_index a end.
     if.  '' -.@-: dltb 2 {::  ndx { BRAILLECODES do.   a =. ')' , each amdt i_index a end.
end. end. end.
;: inv a
)
quoteloopC =: 3 : 0
 a =. ;:  y 
 while. 0 < {:  n =. {.  ( ] parG~ (;: '). ): ).:') I.@:(+./@:(="0 1)) ]) a do.
 if. (;: '): ).:') e.~  a {~  {: n do.  a =. <@quote@:dltb@:(;:inv)@:}.@}: formiddle ( ([ , >:@-~)/  {. (>:@{: n) parG a) a end.
 if. ((;: '). ).: ')) e.~ a {~ {: n do.  a =. (< ,')') ( {: n)} a 
      a =. <@quote@:dltb@:(;:inv)@:}.@}: formiddle (([ , >:@-~)/ {. (<:@{. n) parG a) a 
 else. a =. (< ,')') ( {: n)} a  end.
 end.
 ;: inv a
)

quoteloopW =: 3 : 0
a =. ;: y
while. (#n) > i =. 1 i.~ (0 = CONTROLWRE {.@rxE every a) *. (_1 < {."1)  n =. {:"2 JNAME2&rxmatch every  a  do.
   t =. < ((i {:: a) {~ (+ i.)/) i { n
      if. (;: ': .:') e.~ t do.   a =. <@quote@:dltb@:(;:inv)@:}.@}: formiddle ( ([ , >:@-~)/  {. (>: i) parG a) a  end.
      if. (;: '. .:') e.~ t do.  a =. '.:' -.~ leaf amdt i a 
	  a =. <@quote@:dltb@:(;:inv)@:}.@}: formiddle ( ([ , >:@-~)/ {. (<: i) parG a) a 
      else. a =. '.:' -.~ leaf amdt i a   end.
end.
;: inv a
)

quoteloopO =: 3 : 0
 a =. ;:  y 
 while. 0 < {: n =. {:  ( ] parG~ (< '(.') I.@:((="0 1)) ]) a do.
  a =. <@quote@:dltb@:(;:inv)@:}.@}: formiddle (([ , >:@-~)/ n) a 
 end.
 ;: inv a
)

quoteloopB =: 3 : 0
a =. ;: y
b =. (#~ a: -.@-: {:"1) BRAILLECODES
while. 0 < # n =. I. ({."1 b) e.~ a  do.
 i =. {. n
 ndx =. (i{a) i.~ {."1 BRAILLECODES
     if.  '' -.@-: dltb 1 {::  ndx { BRAILLECODES do.    a =. <@quote@:dltb@:(;:inv)@:}.@}: formiddle ( ([ , >:@-~)/  {. (>: i) parG a) a  
     a =. (1 { ndx { BRAILLECODES) i} a
     else.  a =. (2 { ndx { BRAILLECODES) i} a
      a =. <@quote@:dltb@:(;:inv)@:}.@}: formiddle ( ([ , >:@-~)/  {. (<: i) parG a) a  end.
end. 
;: inv a
)


findline_jpp_ =: 3 : 0  NB. y is added to line or selected text
'' findline y
:
  ft =. 7 u: WinText_jqtide_
   fs =. WinSelect_jqtide_
NB.   NB. If a single value is selected, take the whole line; otherwise the selected region
  if. 1 < # ~. fs do.
   x,~ 8 u:  (-~/\ fs) (];.0~ ,.)~ ft 
  else.
    fs =.  {.fs
 
    x,~ 8 u:  (LF taketo&.|. fs {. ft) , LF taketo fs }. ft
  end.
)


findline2_jpp_ =: 3 : 0  NB. y is added to line or selected text
'' findline2_jpp_ y
:
  ft =. 7 u: WinText_jqtide_
   fs =. WinSelect_jqtide_
 linenum =. (LF +/@:= ])  ({. fs) {. ft
 fl =. (1 i:~ 0 < (>:linenum) {. ])  s =. scriptdepthmarks t =. -.&(CR , LF) each LF ([: -.&a: <;.2@,~) ft

 ll =. >: 1 i.~  (>: fl) }. s

LF joinstring  (fl + i. ll) { t
NB.   NB. If a single value is selected, take the whole line; otherwise the selected region
)

stripNB =:  }:^:('NB.' -: 3 {. _1 {:: ])&.;:
jpp =: quoteloopO@:quoteloopC@:quoteloopW@:quoteloopB@:fixparen@:parenloopC@:parenloopW@:parenloopB@:stripNB^:(  '' ( -.@-:) dltb@])
jpp =: quoteloopO@:quoteloopC@:quoteloopW@:quoteloopB@:fixparen@:parenloopC@:parenloopW@:parenloopB@:stripNB^:(  '' (((,')') -: ])  -.@+. -:)  dltb@])
ee_z_ =: 1 : '(jpp m) eval'
es_z_ =: 1 : '0!:101 jpp m'
esf_z_ =: 1 : '0!:101 jppf m'
DoWithMacro_z_ =: 'es' Cloak
DoWithMacro2_z_ =: 'esf' Cloak

loadj =: 3 : 0
0 loadj y
:
fls=. getscripts_j_ y
fn=. ('script',x#'d')~
for_fl. fls do.
  if. Displayload_j_ do. smoutput > fl end.
  if. -. fexist fl do.
    smoutput 'not found: ',>fl
  end.
  fn fl
  Loaded_j_=: ~. Loaded_j_,fl
end.
empty''
)



requirej =: 3 : 0
fls=. Loaded_j_ -.~ getscripts_j_ y
if. # fls do. loadj fls else. empty'' end.
)

scriptjpp =: [: 3 : '0!:100  jppf fread y ' jpath_z_&.:>

load =: loadjpp =: 3 : 0
0 loadjpp y
:
fls=. getscripts_j_ y
fn=. ('script',x#'d')~
for_fl. fls do.
  if. Displayload_j_ do. smoutput > fl end.
  if. -. fexist fl do.
    smoutput 'not found: ',>fl
  end.
  NB. weird bug in plot demo
  NB.fl2n =. 'JPPTEMP' , leaf forlast 1&. ('/'&cut) , > fl
  fl2n =. '/' joinstring 'JPPTEMP' , leaf forlast 1  ('/'&cut)  > fl
  fl2n fwrite~ jppf fread >  fl
 NB. if. IFQT do. wd 'msgs' end.
  fn fl2n
  NB.1!:55 < fl2n
  Loaded_j_=: ~. Loaded_j_,fl
end.
empty''
)

unloadjpp =: 3 : 'if. y = 0 do. load_z_ =: loadjpp_z_ f. else. load_z_ =: loadj_z_ f. end. y'
