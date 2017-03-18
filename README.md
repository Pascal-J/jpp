# jpp
J Plus Plus


# 'jpp'
extends J with auto parenthesizing, and quoting for custom parsers.  User defined primitives.  
All enhancements are at the (pre)parsing level, with no code execution itself.  

recommend the following 2 userkeys.cfg lines, and load of jpp2.ijs file in config/startup.ijs  

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



# 'Parser helpers'
A or Aa are useful delimeters.  Idendity adverb 2 A 3 produces 2 phrase tokens.  
gg produces gerunds without internal parenthesizing.  cut on \` and implied parentheses within each cut.  
bb is simple cut on \`  
"macros" to auto quote sections of code/lines permits easier custom parsing.  


# 'double/multi adverbs'
provides strand notation, and gerund constructors.  
ti replaces \`.  nouns get encoded as noun ars  
tie is similar but allows nesting gerunds.  
tieA  will encode a string, if it is a name of a verb or modifier, or evaluates to a modifer, as a modifier/name  
ar and aar are the 2 atomic representation adverbs.  
tiD tieD tieAD are double adverb versions of the above.  

