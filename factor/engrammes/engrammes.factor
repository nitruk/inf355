! Copyright (C) 2013 Aurélien Martin (si si).
! See http://factorcode.org/license.txt for BSD license.
USING: kernel math math.functions sequences math.primes math.primes.factors assocs math.parser peg.ebnf vectors continuations ;
IN: engrammes

: >engramme ( n -- str ) dup 1 <=
[ 0 = [ "0" ] [ "1" ] if ]
[ dup [ primes-upto ] dip
group-factors [ at 0 or >engramme ] curry map concat "(" ")" surround ] if ;

ERROR: malformed ;

: parsengramme ( str -- x ) [ [EBNF
number = [0-1] => [[ 48 - ]]
parenthesis = "("~ (engramme)* ")"~
engramme = number | parenthesis 
EBNF] ] [ malformed ] recover ;

: >integer ( v -- n ) [ length nprimes ] keep [ dup vector? [ >integer ] when ^ ] 2map product ;

: engramme> ( str -- n ) parsengramme >integer ;
