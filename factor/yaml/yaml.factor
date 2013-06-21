! Copyright (C) 2013 Aurélien Martin
! See http://factorcode.org/license.txt for BSD license.
USING: peg.ebnf ;
IN: yaml

: parse-yaml ( str -- data ) [EBNF
c-printable = . 
c-flow-indicator = “,” | “[” | “]” | “{” | “}”
c-break = "\r" | "\n"
nb-char = !(c-break) c-printable
b-break = "\r" | "\n" | "\r\n"
blank = " " | "\t"
ns-char = !(blank) nb-char
ns-dec-digit = [0-9]
ns-hex-digit = ns-dec-digit | [a-fA-F]
ns-ascii-letter = [a-zA-Z]
ns-word-char = ns-dec-digit | ns-ascii-letter | "-"
ns-uri-char	= ( “%” ns-hex-digit ns-hex-digit ) | ns-word-char | “#” | “;” | “/” | “?” | “:” | “@” | “&” | “=” | “+” | “$” | “,” | “_” | “.” | “!” | “~” | “*” | “'” | “(” | “)” | “[” | “]”
ns-tag-char = !("!") !(c-flow-indicator) ns-uri-char
c-ns-esc-char = "\0" | "\a" | "\t" | "\n" | "\v" | "\f" | "\r" | "\e" | "\ " | "\"" | "\/" | "\N" | "\_" | "\L" | "\P"i | "\x" | "\u" | "\U" 
litteral = (.)* ((blank)*)~ (("#" (.)*)?)~ "\n"~ 
blockseq = .
flowseq = .
blockmapping = .
flowmapping = .
seq = blockseq | flowseq
mapping = blockmapping | flowmapping
somelement = seq | mapping | litteral
main = c-printable
EBNF] ;
