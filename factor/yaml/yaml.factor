! Copyright (C) 2013 Aurélien Martin
! See http://factorcode.org/license.txt for BSD license.
USING: kernel strings sequences math combinators peg peg.parsers make ;
IN: yaml

: upto ( -- char ) 0x2d ;

: c-class ( seq -- parser ) >string range-pattern ;

: -1+1 ( a b -- c² ) [ ensure-not ] dip 2seq ;
: -2+1 ( a b c -- d ) [ [ ensure-not ] bi@ ] dip 3seq ;

: tokens ( seq -- parser ) [ token ] map choice ;

! Characters

! Character set

: c-printable ( -- parser ) [ 0x9 , 0xa , 0xd , 0x20 , upto , 0x7e , 0x85 , 0xa0 , upto , 0xd7ff , 0xe000 , upto , 0xfffd , 0x10000 , upto , 0x10ffff , ] { } make c-class ; 

: nb-json ( -- parser ) { 0x9 0x20 0x2d 0x10ffff } c-class ;

! Character encoding

: c-byte-order-mark ( -- parser ) 0xfeff 1token ;

! Indicator characters

: c-sequence-entry ( -- parser ) "-" token ; 
: c-mapping-key ( -- parser ) "?" token ; 
: c-mapping-value ( -- parser ) ":" token ; 

: c-collect-entry ( -- parser ) "," token ; 
: c-sequence-start ( -- parser ) "[" token ; 
: c-sequence-end ( -- parser ) "]" token ; 
: c-mapping-start ( -- parser ) "{" token ; 
: c-mapping-end ( -- parser ) "}" token ; 

: c-flow-indicator ( -- parser ) { c-collect-entry c-sequence-start c-sequence-end c-mapping-start c-mapping-end } choice ; 

: c-comment ( -- parser ) "#" token ;

: c-anchor ( -- parser ) "&" token ; 
: c-alias ( -- parser ) "*" token ; 
: c-tag ( -- parser ) "!" token ; 

: c-literal ( -- parser ) "l" token ;
: c-folded ( -- parser ) ">" token ; 

: c-single-quote ( -- parser ) "'" token ; 
: c-double-quote ( -- parser ) "\"" token ; 

: c-directive ( -- parser ) "%" token ; 

: c-reserved ( -- parser ) "@" token "`" token 2choice ;

: c-indicator ( -- parser ) { c-sequence-entry c-mapping-key c-mapping-value c-flow-indicator c-comment c-anchor c-alias c-tag c-literal c-folded c-single-quote c-double-quote c-directive c-reserved } choice ;

! Line break characters

: b-line-feed ( -- parser ) 0xa 1token ;
: b-carriage-return ( -- parser ) 0xd 1token ;
: b-char ( -- parser ) { b-line-feed b-carriage-return } choice ;

: nb-char ( -- parser ) b-char c-byte-order-mark c-printable -2+1 ;

: b-break ( -- parser ) b-carriage-return b-line-feed 2seq b-carriage-return b-line-feed 3choice [ drop "\n" ] action ;

: b-non-content ( -- parser ) b-break ;

! White space characters

: s-space ( -- parser ) 0x20 1token ;
: s-tab ( -- parser ) 0x9 1token ;
: s-white ( -- parser ) s-space s-tab 2choice ;
: ns-char ( -- parser ) s-white nb-char -1+1 ;

! Miscellaneous characters

: ns-dec-digit ( -- parser ) 'digit' ;

: ns-hex-digit ( -- parser ) ns-dec-digit "A-Fa-f" range-pattern 2choice ; 

: ns-ascii-letter ( -- parser ) "A-Za-z" range-pattern ;

: ns-word-char ( -- parser ) ns-dec-digit ns-ascii-letter "-" token 3choice ;

: ns-uri-char ( -- parser ) "%" token ns-hex-digit ns-hex-digit 3seq ns-word-char { "#" ";" "/" "?" ":" "@" "&" "=" "+" "$" "," "_" "." "!" "~" "*" "'" "(" ")" "[" "]" } tokens 3choice ; 

: ns-tag-char ( -- parser ) "!" token c-flow-indicator ns-uri-char -2+1 ;

! Escaped characters

! 0x5c = \ => just to avoid breaking the whole file's coloration by vim with "\\"
: ns-esc-char ( -- parser ) 0x5c 1token { "0" "a" "b" "t" "n" "v" "f" "r" "e" " " "\"" "/" "N" "_" "L" "P" } tokens 0x9 1token 2choice "x" 2 "u" 4 "U" 8 [ ns-hex-digit swap exactly-n 2seq ] 2tri@ 4choice  2seq ;

! Basic structures

! Indentation spaces

: s-indent ( n -- parser ) s-space swap exactly-n ;
: s-indent-le ( n -- parser ) s-space swap at-most-n ;
: s-indent-lt ( n -- parser ) 1 - s-indent-le ;

! Separation spaces

: s-separate-in-line ( -- parser ) s-white repeat1 ;

! Line prefixes

: s-block-line-prefix ( n -- parser ) s-indent ;
: s-flow-line-prefix ( n -- parser ) s-indent s-separate-in-line optional 2seq ;

: s-line-prefix ( n context -- parser ) { { "block-out"  [ s-block-line-prefix ] } { "block-in" [ s-block-line-prefix ] } { "flow-out"  [ s-flow-line-prefix ] } { "flow-in" [ s-flow-line-prefix ] } } case ;

! Empty lines

: l-empty ( n context -- parser ) [ dup ] dip s-line-prefix [ s-indent-lt ] dip swap 2choice b-break 2seq ;

! Line folding

: b-l-trimmed ( n context -- parser ) l-empty repeat1 b-non-content swap 2seq ;

: b-as-space ( -- parser ) b-break ;

: b-l-folded ( n context -- parser ) b-l-trimmed b-as-space 2choice ;

: s-flow-folded ( n -- parser ) dup s-flow-line-prefix [ "flow-in" b-l-folded ] dip [ s-separate-in-line optional ] 2dip 3seq ;

! Comments

: c-nb-comment-text ( -- parser ) "#" token nb-char repeat0 2seq ;
: b-comment ( -- parser ) b-non-content ;
: s-b-comment ( -- parser ) s-separate-in-line c-nb-comment-text optional 2seq optional b-comment 2seq [ drop ] action ; 

: l-comment ( -- parser ) s-separate-in-line c-nb-comment-text optional b-comment 3seq [ drop ] action ;

: s-l-comments ( -- parser ) s-b-comment l-comment 2seq ;

! Separation lines

: s-separate-lines ( n -- parser ) s-flow-line-prefix s-l-comments swap 2seq s-separate-in-line 2choice ;

: s-separate ( n context -- parser ) { { "block-out" [ s-separate-lines ] } { "block-in" [ s-separate-lines ] } { "flow-out" [ s-separate-lines ] } { "flow-in" [ s-separate-lines ] } { "block-key" [ drop s-separate-in-line ] } { "flow-key" [ drop s-separate-in-line ] }  } case ; 

! Directives

: ns-yaml-version ( -- parser ) ns-dec-digit repeat1 dup "." token swap 3seq ;
: ns-yaml-directive ( -- parser ) "YAML" token s-separate-in-line ns-yaml-version 3seq ;

: c-primary-tag-handle ( -- parser ) "!" token ;
: c-secondary-tag-handle ( -- parser ) "!!" token ;
: c-named-tag-handle ( -- parser ) "!" token dup ns-word-char repeat1 swap 3seq ;
: c-tag-handle ( -- parser ) c-named-tag-handle c-secondary-tag-handle c-primary-tag-handle 3choice ;

: c-ns-local-tag-prefix ( -- parser ) "!" token ns-uri-char repeat0 2seq ;
: ns-global-tag-prefix ( -- parser ) ns-tag-char ns-uri-char repeat0 2seq ;
: ns-tag-prefix ( -- parser ) c-ns-local-tag-prefix ns-global-tag-prefix 2choice ;

: ns-tag-directive ( -- parser ) [ "TAG" token , s-separate-in-line dup , c-tag-handle , , ns-tag-prefix , ] seq* ;

: ns-directive-name ( -- parser ) ns-char repeat1 ;
: ns-directive-parameter ( -- parser ) ns-char repeat1 ;
: ns-reserved-directive ( -- parser ) ns-directive-name s-separate-in-line ns-directive-parameter 2seq repeat0 2seq ;

: l-directive ( -- parser ) "%" token ns-yaml-directive ns-tag-directive ns-reserved-directive 3choice s-l-comments 3seq ;

! Node properties

: c-verbatim-tag ( -- parser ) "!<" ">" [ token ] bi@ ns-uri-char repeat1 swap 3seq ; 
: c-ns-shorthand-tag ( -- parser ) c-tag-handle ns-tag-char repeat1 2seq ; 
: c-non-specific-tag ( -- parser ) "!" token ;
: c-ns-tag-property ( -- parser ) c-verbatim-tag c-ns-shorthand-tag c-non-specific-tag 3choice ;

: ns-anchor-char ( -- parser ) c-flow-indicator ns-char -1+1 ;
: ns-anchor-name ( -- parser ) ns-anchor-char repeat1 ;
: c-ns-anchor-property ( -- parser ) "&" token ns-anchor-name 2seq ;

: s-ns-properties ( n context -- parser ) s-separate [ c-ns-tag-property swap c-ns-anchor-property 2seq optional 2seq ] [ c-ns-anchor-property swap c-ns-tag-property 2seq optional 2seq ] bi 2choice ;
