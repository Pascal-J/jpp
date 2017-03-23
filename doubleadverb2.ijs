cocurrent 'z'
ML_z_ =: 0
pD_z_ =: (1!:2&2) : (] [ (1!:2&2)@:(,&<)) 
amdt_z_ =: 2 : '(u (v{ ]))`(v"_@:])`]} ]' NB. use u@] for monad amenditems, u@[ for function applied to y instead of v{y.  or u for dyad amenditem.
v2a =: 1 : ( 'a =. (''('' , u lrA ,'') m '') label_. 1 : (''m  1 :'' , quote a)')
eval_z_ =:  1 : 'if. 2 ~: 3!:0 m do. m else. a: 1 :  m end.' NB.1 : ' a: 1 :  m'

daF =: 1 : ('a =. (''2 : '', (quote m) , '' u'') label_. 1 : (''u  1 :'' , quote a)')
daFx =: (0 : ) 1 : ('a =. (''2 : ('', (lr m) , '') u'') label_. 1 : (''u  1 :'' , quote a)') NB. explicit. call with 0 left arg
c2da =: 1 : ( 'a =. (m ,'' u'') label_. 1 : (''u  1 :'' , quote a)')
NB. c2da =: 1 : ( 'a =. ''('' , m ,'' u)'' label_. 1 : (''u  1 :'' , quote a)')
dlbind =: c2daS =: 1 : ( 'a =. ((": m) ,~''u '') label_. 1 : (''u  1 :'' , quote a)') 
dbind =: 1 : ( 'a =. (( u lrA) ,~''u '') label_. 1 : (''u  1 :'' , quote a)') 
da2c =: 2 : '(u  , '' '', v) eval'  NB. both u and v can be string reps of adverbs, or just v is modifier.  many combination of lrs.
evalC =: 2 : ' v u eval' NB. u is string that can be modifier.  v is verb or noun
da2v =: 4 : '(x da2c y)'

assign_z_ =: 4 : '(x) =: y'
assignwith =: 1 : ('(y) =: u ". y';':';'(y) =: x u ". y')
NB. lassignwith=: 1 : ('y lassign f. u y~';':';'".''y lassign f. x u y~''') 
assignwithC =: 2 : ('(y) =: u (y~ [ ]) :: (n"_) 1';':';'(y) =: x u (y~ [ ]) :: (n"_) 1 ')
defaults1 =: ([`]@.(0=#@>@[))
defaults =: defaults1"0 0 f.
fixlenx =: 1 : (':';'(linearize (#y) {. x) u y')
dflt =: defaults fixlenx & boxopen 
dflt =: ((#@] linearize@{. [) ([`]@.(0=#@>@[))"0 0 ])&boxopen 

linearize =: (, $~ 1 -.~ $) 
strbracketF =: 0&{::@:[ , ] , 1&{::@:[
strbracket =: (0&({)@:[ , ] , 1&({)@:[)
strinsert =: 1 : ' [ , u , ]'

parenA =: '()'&strbracket@:
lr_z_ =: 3 : '5!:5 < ''y'''
lrA_z_ =: 1 : '5!:5 < ''u'''
quotejuststrings =:  quote^:([: *./ '''' = {., {:) each &. ;:
lrq =: quotejuststrings@:lr

itemamend =: 4 : '((2}.$y) $"1 0 x)} y'
filtermod =: 2 : 'v itemamend ] ,: v # inv [: u v # ]' 
NB. (<'aa')"0  filtermod (('sdf')&-: &>) &. ;:'sdf sdfsdf sd fds sdf'
filtermodA =: 1 : 'u itemamend ] ,:  (#inv~ u)'
curry =: 4 : '(boxopen y) (a: = ]) filtermodA x'
curryB=: 2 : 'u hook (n&curry)'
ql=: 2 : '(quote m) , n'
qr=: 2 : '( m) , (quote n)'

isNoun_z_ =: (0 = 4!:0 ( :: 0:))@:< 
ar =: 1 : '5!:1 <''u'''
aar =: 1 : 'if. isNoun ''u'' do. q =. m eval else. q =. u end. 5!:1 < ''q'' '
ismodstring =: 1 : 'if. 0 = 4!:0 <''u'' do. try. q =.  m eval catch. 0 return. end. 1 2 e.~ 4!:0 <''q''else. 0 end. '
ncS=:3 :'z=.y 1 :y label_. 4!:0 <''z'' ' :: _2: NB. nameclass of string
lrP =: 'if. 0~: 4!:0 <''v'' do. v =. v lrA end. if. u ismodstring do.  u=. m else. u =. ''('', u lrA , '')'' end.  u , '' '' , v ' daF
ncA =: 1 : 'if. 3 ~: 4!:0 < ''u'' do. if. m ismodstring do. m ; ncS m else. 0 ;~ ''('', m lrA ,'')'' end. else. 3;~ ''('', u lrA ,'')'' end.'
NB.tieB =: 2 : 'if. 2 ~: #@$ n do. n =. ,: n end. pD n if. u ismodstring do.  u=. m else. u =. ''('', u lrA , '')'' end. n ,~ u; ncS  u'
tieB =: 'if. 1 = #@$ n do. n =. ,: n end. n ,~ u ncA' daF
NB.tieB =: 2 :'if. 1 = #@$ n do. n =. ,: n end. n ,~ u ncA' 
lrB =: 2 : 0
if. 0 = L. n do. ,: n;ncS n end.
)
lrX =: 1 : ('''('', u lrA , '') '' , lr y';':';'''('', (lr x) , '') ('' , u lrA , '') '' , lr y')
Aa =: A_z_ =: 1 : 'u'



an =: 1 : '<@:((,''0'') ,&< ]) m'  NB. atomic rep of noun as an adverb (right arg)
tie =: '`'c2da
tie =: 2 : 'if.  u isgerundA do. if. v isgerundA do. m ar , v ar else. m , v ar end. else. if. v isgerundA do. u ar , n  else. u ar , v ar end. end. '
tieA =: 2 : 'if. isNoun ''u'' do. if. isgerund m do. m , v aar else. u ar , v aar end. else. u ar , v aar end.'
tieA =: 2 : 'if.  u isgerundA do. if. v isgerundA do. m ar , v ar else. m , v aar end. else. if. v isgerundA do. u aar , n  else. u aar , v aar end. end. '
ti =: 2 : 0
if. -. u isgerundA do. u =. u ar end.
if. -. v isgerundA do. v =. v ar end.
u,v
)
tiA =: 2 : 0
if. -. u isgerundA do. u =. u aar end.
if. -. v isgerundA do. v =. v aar end.
u,v
)
tiD =: 'u ti v' daF
tiAD=: 'u tiA v' daF
tieD =: 'u tie v' daF
tieDs =: 'v tie u' daF
tieAD =: 'u tieA v' daF
tieADs =: 'v tieA u' daF

NB. tie =: 0 dafX
NB. if. 0 -.@-: 1{:: a =. v ncA do. if. 0 = L. n =. ,: a end.
NB. )
at =: 'u (v@:) 'daF
atop =: 'u (v@) 'daF
def =: 'u (v :) 'daF
nd =: 'u (v&)' daF


lrX =: 1 : ('''('', u lrA , '') '' , lr y';':';'''('', (lr x) , '') ('' , u lrA , '') '' , lr y')

lrT =: ML daFx  NB. v (first right parm) cannot be noun (can be lr of adverb/modifier)
if. 0 -.@-: 1{:: a =. v ncA do. n =. ,: a end.
if. '9:' -: u lrA do. n else. 
(u ncA ,  n) lrT  end.
)

toG =: 1 : ' if. 1 2 e.~ 1 {:: u ncA do. a =. (m) eval else. a=.u end. 5!:1 < ''a''' NB.makes gerund from anything. turning string modifiers into gerund versions. 
bG =: 2 : 0  NB. builds gerund.  recurses until n = 0
select. n case. 0 ;1;_1 do. u case. 2 do.  tieA u  case. _2 do.   (u tieA) case. (;/ _2 - i.20) do. (u tieA)(bG (n+1)) case. do. ( tieA u)(bG (n-1)) end.
)
bG9 =:  'if. ''9:'' -: u lrA do.  n else.  (u tieA v) bG9 end.' daF
G =: 2 : 0  NB. builds gerund.  recurses until n = 0
select. n case. 0 ;1;_1 do. u case. 2 do.  tie u  case. _2 do.   (u tie) case. (;/ _2 - i.20) do. (u tie)(G (n+1)) case. do. ( tie u)(G (n-1)) end.
)
G9 =:  'if. ''9:'' -: u lrA do.  n else.  (u tie v) G9 end.' daF

ll =:   '''('' , u lrA , '') '' , n' daF
lj =: ' u , '' '' , n' daF
NB.  + (-  '/' lrA ll   'lrB tieB tieB {."1 v2a arS' AA)
NB.names =. ;: inv  <"1 'abcdefghijklmnopqrstuvwxyz'{~ 26 #. inv i.# m
arS =: 1 : '". every ,&'' toG'' each  quote^:('')'' ~: {:) each m'
lr2ar =: 1 :  '({."1 m) arS'
ar2lr =: 3 : 'o =. i.0 for_i. y do. a =. (i 5!:0) label_. o =. o , < 5!:5 <''a''  end. o'
Gs =:  ML daFx  NB. lrT equiv
if. 0 ~: v ncA do. n =. v toG`'' elseif. 0 = L. n do. n =. v toG`'' end.
if. '9:' -: u lrA do. n else. NB.pD (n =. u toG   n)
 (n) Gs  end.
)
Gs =: ML daFx  NB. v (first right parm) cannot be noun (can be lr of adverb/modifier)
if. 0 -.@-: 1{:: a =. v ncA do. n =. ,: a end.
if. '9:' -: u lrA do. ({.@|: n) arS else. 
(u ncA ,  n) Gs  end.
)
evaly =: 3 : 'y eval lrA'  NB. removes 1 level of parens from lrT boxes
isgerund =: 0:`(0 -.@e. 3 : ('y (5!:0)';'1')"0)@.(0 < L.) :: 0:
isgerundA =: 1 : ' if. isNoun ''u'' do. isgerund m else. 0 end.'

aatrain =: ML daFx  NB. perfectly solves the composability of double adverbs.  But fails if modifier params included.
if. 0 -.@-: 1{:: a =. v ncA do. n =. ,: a end.
if.  1 = 1 {:: (;: inv {."1 a =.(u ncA , n)) ncA do.  a aatrain else.
 (;: inv {."1 a) eval end.
)

displayandrun =: 1 : 0
pD '>>> ' , u lrX y
u y
:
pD '>>> ' , x u lrX y
x u y
)
reduce =: 1 : '<"_1@[ ([: u (&.>)/(>@:) ,) <@:]'
Ambi =: (1 : 'u : (u reduce)')
pDr =: pD@:('    ' , ":)"1@:":
AmbiD =: 1 : 'u [ ''    '' pDr@, u lrX'
evaleach =: 1 : 0
for_i. m do. pDr^:(i_index < <:#m) (i eval) :: ((i eval)@:]) y end.
:
for_i. m do. pDr^:(i_index < <:#m) x (i eval) :: ((i eval)@:]) y end.
)
lrT2lrA =: ( {."1) (([: ;: inv  evaly each)@:) v2a
lrS =: 'lrT {."1 v2a' aatrain

BL =:  (<@[`)(`])(`:6)
BR =:  ([`)(`(<@]))(`:6)
Assert =: 1 : '] [ [ 13!:8^:((0 e. ])`(12"_)) u@]'

lrS=: 'lrT {."1 v2a' aatrain
lvProcsRestT =: (1 : '( 0 {:: {: m) eval }: m') 
lvProcsRest =: (1 : '(> {: m) eval }: m') 
lvProcsStrand =: 'lrS lvProcsRest eval' aatrain

evoke =: 4 : 'y `:x'
Cloak=: aar(0:`)(,^:)
train =: '`:'Cloak&6
oa =: 1 :'u Cloak @:' 
CLapply =: 2 : 'u@:] ''tie''Cloak ''`:6''oa v'  NB. consider auto-adding @[ to v
Dadverb =: ':'&; '1 :' oa@:cutLF@:(' 0 :' Cloak)
Dconj =: ':'&; '2 :' oa@:cutLF@:(' 0 :' Cloak)

C =:   toG 'G9 comp' aatrain
cc =: 2 : 'm`(v aar)@.(_1 strinsert/ i.#m)'
cp =: '()'&strbracket@fixparen each@:('`'&cut )
gg =: 1 : '(''ti'' joinstring cp m) eval' 
s2b =: 1 : 'm rplc '' `'''
bb =: 1 :  'dltb each (''`''&cut) m'
bs =: 1 :  'dltb each ('';''&cut) m'
tr =: '`:6' Cloak
unar =: '5!:0' Cloak
Cat =: '@:' tieAD C
combG =: '(`:6)toG' Cloak
Advsert=: 'if. -. v isgerundA do. n =. v toG end. (combG"1 m ,.(#m) $ n)' daF  NB. 2 gerund arguments. n coerced to one if not.
AltM =: 1 : '}.@:(((#m) # ,: }.@] ,~ [ , G 4)  m ''(@{.)(@])''  Advsert Advsert/)@:({. , ])'

depth =: '()'&$: :  ([: +/\ =/\@(''''&~:)@] * 1 _1 0 {~  i.)
cutP =: ({:@] ,~ ]) <;._2~ 1 ,~ (] ~: 0 , }:)@:(1 <. ])@:depth

cutAP=: '()'&$: : (4 : '] ]`$:@.(({.x) e. ;)@:cutP each tieD AltM cutP y') :. uncutAP
uncutAP =: '()'&$: : (4 : ';@:(< (<{.x)strinsert(<{:x)strinsert tieD/&.>  tieD@.(1 < L.)"0)^:_ ; at@:((<{.x)strinsert (<{:x)strinsert tieD/) y') :. cutAP
NB. mRS =: ((((((`'')(((`'')(&(<&.:(,^:(0:``:)&6 :.(<@:((,'0') ,&< ])))@:[)))((`_)(`:6))))(`,))(`]))(`:6))(,^:(0:``:)&6 :.(<@:((,'0') ,&< ]))@:))(&:(<@:((,'0') ,&< ])))  NB. see wicked
NB. dRS =: ((((((((`'')(((`'')(&(<&.:(,^:(0:``:)&6 :.(<@:((,'0') ,&< ])))@:[)))((`_)(`:6))))(`,))(`]))(,`))([`))(`:6))(,^:(0:``:)&6 :.(<@:((,'0') ,&< ]))@:))(&:(<@:((,'0') ,&< ])))
NB. stripxtraP =: ] (1 { ]) tieD@.((3 = #) *. (a:,a:) -: {. , {:)  ]`$: @.(2 < L.) at each mRS&.cutAP ^:_  NB. needs mRS def wicked

F =: '(G 3) (`:6)' aatrain
H =: 'u v' daF
dfltG =:  'if. -. u  isgerundA do. m =. u toG end. linearize n [^:((a: ar) -: ])"0 (#n) {.!.(a: ar) m' daF
av =:  'if. 3 = 1 {:: u ncA do. u tiA (v toG) else. (m eval) (tiA (v toG)) end. ' daF

NB. av supports n any, a v, a c, v c. if u adverb, then result is an adverb that will bind u with param as gerund.
NB.av =:  'ti (u tiA v)' daF
Grun =: 4 : 'x tie y `:6'
S =: 2 : ' ((s: cut m) ; v ar)'

