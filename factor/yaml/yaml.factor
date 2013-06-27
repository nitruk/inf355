! Copyright (C) 2013 Aurélien Martin
! See http://factorcode.org/license.txt for BSD license.
USING: kernel strings sequences math combinators peg peg.parsers make generalizations ;
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
: c-ns-esc-char ( -- parser ) 0x5c 1token { "0" "a" "b" "t" "n" "v" "f" "r" "e" " " "\"" "/" "N" "_" "L" "P" } tokens 0x9 1token 2choice "x" 2 "u" 4 "U" 8 [ ns-hex-digit swap exactly-n 2seq ] 2tri@ 4choice  2seq ;

! Basic structures

! Indentation spaces

: s-indent ( n -- parser ) dup "n/a" = [ drop s-space repeat0 ] [ s-space swap exactly-n ] if ;
: s-indent-le ( n -- parser ) s-space swap at-most-n ;
: s-indent-lt ( n -- parser ) 1 - s-indent-le ;

! Separation spaces

! s-white+ | /* Start of line */
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
! b-non-content  | /* End of file */
: b-comment ( -- parser ) b-non-content ;
: s-b-comment ( -- parser ) s-separate-in-line c-nb-comment-text optional 2seq optional b-comment 2seq [ drop ] action ; 

: l-comment ( -- parser ) s-separate-in-line c-nb-comment-text optional b-comment 3seq [ drop ] action ;

! s-b-comment | /* Start of line */
: s-l-comments ( -- parser ) s-b-comment l-comment repeat0 2seq ;

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

: c-ns-properties ( n context -- parser ) s-separate [ c-ns-tag-property swap c-ns-anchor-property 2seq optional 2seq ] [ c-ns-anchor-property swap c-ns-tag-property 2seq optional 2seq ] bi 2choice ;

! Flow styles

! Alias nodes

: c-ns-alias-node ( -- parser ) "*" token ns-anchor-name 2seq ;

! Empty nodes

: e-scalar ( -- parser ) "" token ;

: e-node ( -- parser ) e-scalar ;

! Flow scalar styles

! Double-quoted style

: nb-double-char ( -- parser ) c-ns-esc-char 0x5c 1token "\"" token nb-json -2+1 2choice ;

: ns-double-char ( -- parser ) s-white nb-double-char -1+1 ;

: nb-double-one-line ( -- parser ) nb-double-char repeat0 ;

: s-double-escaped  ( n -- parser )  [ [ s-flow-line-prefix ] [ "flow-in" l-empty ] bi s-white repeat0 , 0x5c 1token , b-non-content , , , ] seq* ;
: s-double-break ( n -- parser ) [ s-double-escaped ] [ s-flow-folded ] bi 2choice ;

: nb-ns-double-in-line ( -- parser ) s-white repeat0 ns-double-char 2seq repeat0 ;
: s-double-next-line ( n -- parser ) dup s-double-break ns-double-char nb-ns-double-in-line 3 1 mnswap s-double-next-line s-white repeat0 2choice 3seq optional 2seq ;
: nb-double-multi-line ( n -- parser ) s-double-next-line nb-ns-double-in-line swap s-white repeat0 2choice 2seq ;

: nb-double-text ( n context -- parser ) { { "flow-out" [ nb-double-multi-line ] } { "flow-in" [ nb-double-multi-line ] } { "block-out" [ drop nb-double-one-line ] } { "block-in" [ drop nb-double-one-line ] } } case ;
: c-double-quoted ( n context -- parser ) "\"" token dup nb-double-text swap 3seq ;

! Single-quoted style

: c-quoted-quote ( -- parser ) "''" token ;
: nb-single-char ( -- parser ) c-quoted-quote "'" token nb-json -1+1 2choice ;
: ns-single-char ( -- parser ) s-white nb-single-char -1+1 ;

: nb-single-one-line ( -- parser ) nb-single-char repeat0 ;

: nb-ns-single-in-line ( -- parser ) s-white repeat0 ns-single-char 2seq repeat0 ;
: s-single-next-line ( n -- parser ) [ s-flow-folded ] [ s-single-next-line s-white repeat0 2choice ns-single-char nb-ns-single-in-line 2 1 mnswap 3seq optional ] bi 2seq ;
: nb-single-multi-line ( n -- parser ) s-single-next-line nb-ns-single-in-line swap s-white repeat0 2choice 2seq ;

: nb-single-text ( n context -- parser ) { { "flow-out" [ nb-single-multi-line ] } { "flow-in" [ nb-single-multi-line ] } { "block-out" [ drop nb-single-one-line ] } { "block-in" [ drop nb-single-one-line ] } } case ;
: c-single-quoted ( n context -- parser ) nb-single-text "'" surrounded-by ;

! Plain style

: ns-plain-safe-out ( -- parser ) ns-char ;
: ns-plain-safe-in ( -- parser ) c-flow-indicator ns-char -1+1 ;
: ns-plain-safe ( context -- parser ) { { "flow-out" [ ns-plain-safe-out ] } { "flow-in" [ ns-plain-safe-in ] } { "block-key" [ ns-plain-safe-out ] } { "flow-key" [ ns-plain-safe-in ] } } case ;

: ns-plain-first ( context -- parser ) c-indicator ns-char -1+1 { "?" ":" "-" } tokens 2 1 mnswap  ns-plain-safe ensure 2seq 2choice ;
! /* An ns-char preceding */ "#"
: ns-plain-char ( context -- parser ) ns-plain-safe [ "#" ":" [ token ] bi@ ] keep -2+1 ns-char "#" token 2seq ":" 3 1 mnswap ensure 2seq 3choice ;

: nb-ns-plain-in-line ( context -- parser ) ns-plain-char s-white repeat0 swap 2seq repeat0 ;
: ns-plain-one-line ( context -- parser ) [ ns-plain-first ] [ nb-ns-plain-in-line ] bi 2seq ;

: s-ns-plain-next-line ( n context -- parser ) dup [ s-flow-folded ] [ ns-plain-char ] [ nb-ns-plain-in-line ] tri* 3seq ;
: ns-plain-multi-line ( n context -- parser ) dup ns-plain-one-line 1 2 mnswap s-ns-plain-next-line repeat0 2seq ; 

: ns-plain ( n context -- parser )  dup { { "flow-out" [ ns-plain-multi-line ] } { "flow-in" [ ns-plain-multi-line ] } { "block-key" [ [ drop ] [ ns-plain-one-line ] bi* ] } { "flow-key" [ [ drop ] [ ns-plain-one-line ] bi* ] } } case ;

! Flow collection styles

: in-flow ( context -- context ) { { "flow-out" [ "flow-in" ] } { "flow-in" [ "flow-in" ] } { "block-key" [ "flow-key" ] } { "flow-key" [ "flow-key" ] } } case ;

! Flow sequences

DEFER: ns-flow-pair
DEFER: ns-flow-node

: ns-flow-seq-entry ( n context -- parser ) [ ns-flow-pair ] [ ns-flow-node ] 2bi 2choice ;
: ns-s-flow-seq-entries ( n context -- parser ) [ ns-flow-seq-entry ] [ s-separate optional dup ] 2bi [ "," token ] 2dip 3seq list-of ;
: c-flow-sequence ( n context -- parser ) [ s-separate ] [ in-flow ns-s-flow-seq-entries ] 2bi [ optional ] bi@ 2seq "[" "]" surrounded-by ; 

! Flow mappings

DEFER: ns-flow-yaml-node
DEFER: c-flow-json-node

: c-ns-flow-map-separate-value ( n context -- parser ) [ nip ns-plain-safe ensure-not ":" token swap 2seq ] [ s-separate ] [ ns-flow-node ] 2tri 2seq e-node 2choice 2seq ;
: ns-flow-map-yaml-key-entry ( n context -- parser ) [ ns-flow-yaml-node ] [ s-separate optional ] [ c-ns-flow-map-separate-value ] 2tri 2seq e-node 2choice 2seq ;
: c-ns-flow-map-empty-key-entry ( n context -- parser ) [ e-node ] 2dip c-ns-flow-map-separate-value 2seq ;
: c-ns-flow-map-adjacent-value ( n context -- parser ) [ s-separate optional ] [ ns-flow-node ] 2bi 2seq e-node 2choice ":" token swap 2seq ;
: c-ns-flow-map-json-key-entry ( n context -- parser ) [ c-flow-json-node ] [ s-separate optional ] [ c-ns-flow-map-adjacent-value ] 2tri 2seq e-node 2choice 2seq ;

: ns-flow-map-implicit-entry ( n context -- parser ) [ ns-flow-map-yaml-key-entry ] [ c-ns-flow-map-empty-key-entry ] [ c-ns-flow-map-json-key-entry ] 2tri 3choice ;

: ns-flow-map-explicit-entry ( n context -- parser ) ns-flow-map-implicit-entry e-node dup 2seq 2choice ;
: ns-flow-map-entry ( n context -- parser ) [ s-separate ] [ ns-flow-map-explicit-entry ] [ ns-flow-map-implicit-entry ] 2tri [ "?" token ] 3dip [ 3seq ] dip 2choice ;

: ns-s-flow-map-entries ( n context -- parser ) [ ns-flow-map-entry ] [ s-separate optional dup ] 2bi  [ "," token ] dip 3seq list-of ;
: c-flow-mapping ( n context -- parser ) [ s-separate ] [ in-flow ns-s-flow-map-entries ] 2bi [ optional ] bi@ 2seq "{" "}" surrounded-by ; 

! /* At most 1024 characters altogether */
: ns-s-implicit-yaml-key ( context -- parser ) "n/a" swap ns-flow-yaml-node s-separate-in-line optional 2seq ;
! /* At most 1024 characters altogether */
: c-s-implicit-json-key ( context -- parser ) "n/a" swap c-flow-json-node s-separate-in-line optional 2seq ;
: ns-flow-pair-yaml-key-entry ( n context -- parser ) c-ns-flow-map-separate-value "flow key" ns-s-implicit-yaml-key swap 2seq ;
: c-ns-flow-pair-json-key-entry ( n context -- parser ) c-ns-flow-map-adjacent-value "flow-key" c-s-implicit-json-key swap 2seq ;
: ns-flow-pair-entry ( n context -- parser ) [ ns-flow-pair-yaml-key-entry ] [ c-ns-flow-map-empty-key-entry ] [ c-ns-flow-pair-json-key-entry ] 2tri 3choice ;

: ns-flow-pair ( n context -- parser ) [ s-separate "?" token swap ] [ ns-flow-map-explicit-entry ] [ ns-flow-pair-entry ] 2tri [ 3seq ] dip 2choice ;

! Flow nodes

: ns-flow-yaml-content ( n context -- parser ) ns-plain ;
: c-flow-json-content ( n context -- parser ) { [ c-flow-sequence ] [ c-flow-mapping ] [ c-single-quoted ] [ c-double-quoted ] } 2cleave 4choice ;
: ns-flow-content ( n context -- parser ) [ ns-flow-yaml-content ] [ c-flow-json-content ] 2bi 2choice ; 

: ns-flow-yaml-node ( n context -- parser ) { [ ns-flow-yaml-content ] [ c-ns-properties  ] [ s-separate ] [ ns-flow-yaml-content ] } 2cleave 2seq e-scalar 2choice 2seq [ c-ns-alias-node ] 2dip 3choice ;
: c-flow-json-node ( n context -- parser ) [ c-ns-properties ] [ s-separate ] [ c-flow-json-content ] 2tri [ 2seq optional ] dip 2seq ; 
: ns-flow-node ( n context -- parser ) { [ ns-flow-content ] [ c-ns-properties  ] [ s-separate ] [ ns-flow-content ] } 2cleave 2seq e-scalar 2choice 2seq [ c-ns-alias-node ] 2dip 3choice ; 

! Block scalar styles

! Block scalar headers

: indent-auto-detect ( -- n ) 10 ;

: c-indentation-indicator ( -- parser ) "0" token ns-dec-digit -1+1 "" token [ drop indent-auto-detect ] action 2choice ;

: c-chomping-indicator ( -- parser ) "-" [ drop "strip" ] "+" [ drop "keep" ] "" [drop "clip" ] [ [ token ] dip action  ] 2tri@ 3choice  ;

! /!\ Error, error, errooor !
: c-b-block-header ( -- m t ) [ c-indentation-indicator ] [ c-chomping-indicator ] bi* dup swap [ 2seq ] 2bi@ 2choice s-b-comment 2seq ;

: b-chomped-last ( t -- parser ) { { "strip" [ b-non-content ] } { "clip" [ b-as-line-feed ] } { "keep" [ b-as-line-feed ] } } case ;

: l-chomped-empty ( n t -- parser ) { { "strip" [ l-strip-empty ] } { "clip" [ l-strip-empty ] } { "keep" [ l-keep-empty ] } } case ;

: l-trail-comments ( n -- parser ) s-indent-lt c-nb-comment-text b-comment l-comment repeat0 4seq ;

: c-l+literal ( n -- parser ) "|" swap c-b-block-header [ + ] dip l-literal-content ;

: l-nb-literal-text ( n -- parser ) [ "block-in" l-empty repeat0 ] [ s-indent ] bi nb-char repeat1 3seq ;
: b-nb-literal-next ( n -- parser ) l-nb-literal-text b-as-line-feed swap 2seq ;
: l-literal-content ( n t -- parser ) [ [ [ l-nb-literal-text ] [ b-nb-literal-next repeat0 ] bi ] [ b-chomped-last ] bi* 3seq optional ] [ l-chomped-empty ] bi@ 2seq ;

: c-l+folded ( n -- parser ) ">" swap c-b-block-header [ + ] dip l-folded-content ;

: s-nb-folded-text ( n -- parser ) s-indent ns-char nb-char repeat0 ;
: s-nb-folded-lines ( n -- parser ) [ s-nb-folded-text ] [ "block-in" b-l-folded ] [ s-nb-folded-text ] 2seq repeat0 2seq ;

: s-nb-spaced-text ( n -- parser ) s-indent s-white nb-char repeat0 3seq ;
: b-l-spaced ( n -- parser ) b-as-line-feed swap "block-in" l-empty repeat0 2seq ; 
: l-nb-spaced-lines ( n -- parser ) [ s-nb-spaced-text ] [ b-l-spaced ] [ s-b-spaced-text ] tri 2seq repeat0 seq ;

: l-nb-same-lines ( n -- parser ) [ "block-in" l-empty repeat0 ] [ l-nb-folded-lines ] [ l-nb-spaced-lines ] tri 2choice 2seq ;
: l-nb-diff-lines ( n -- parser ) [ l-nb-same-lines ] [ l-nb-same-lines ] bi b-as-line-feed swap 2seq repeat0 2seq ;

: l-folded-content ( n t -- parser ) [ [ l-nb-diff-lines ] [ b-chomped-last ] bi* optional ] [ l-chomped-empty ] 2bi 2seq ;

! /* For some fixed auto-detected m > 0 */
: l+block-sequence ( m n -- parser ) + [ s-indent ] [ c-l-block-seq-entry ] bi 2seq repeat1 ;
: c-l-block-seq-entry ( n -- parser ) "block-in" s-l+block-indented [ "-" token ns-char ensure-not ] dip 3seq ;

: s-l+block-indented ( m n c -- parser ) [ [ s-indent ] keep ] 2dip idupd [ 1 + + [ ns-l-compact-sequence ] [ ns-l-compact-mapping ] bi ] 2dip 2choice 2seq s-l+block-node e-node s-l-comments 2seq 3choice ;

