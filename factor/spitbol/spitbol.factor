! Copyright (C) 2013 Aurélien Martin
! See http://factorcode.org/license.txt for BSD license.
USING: kernel io sequences ascii math alien.syntax vectors quotations math.ranges accessors continuations strings generalizations arrays prettyprint combinators namespaces combinators.short-circuit combinators.smart make math.parser ;
IN: spitbol

CONSTANT: parse-null ;
CONSTANT: parse-failed ;

ERROR: parse-error ;

TUPLE: parser { in vector } { out vector } ;

TUPLE: node { sons vector } { action quotation } ;

UNION: inner-arrow quotation parser ;

TUPLE: arrow { effect inner-arrow } { to node } ;

! Arrow effect : ( quot ast vector node -- ast vector )

DEFER: (parse)

DEFER: parse-next-raw

DEFER: parse-next

DEFER: any-from

SYMBOL: parse-canceled

SYMBOL: parse-length

! Private general-use tool-words

: str2v ( string -- vector ) >vector reverse ;

: v2str ( vector -- string ) reverse >string ;

: trap ( x y z -- z y x ) swapd swap swapd ;

: 2swap ( x y z t -- z t x y ) [ trap ] dip trap ;

: quap ( x y z t -- t z y x ) 2swap [ swap ] 2bi@ ;

: 1vector? ( x -- ? ) { [ vector? ] [ length 1 = ] } 1&& ;

: 2vector ( x y -- vector ) V{ } 2sequence ;

: 4curry ( a b c d quot -- quot ) 2curry 2curry ; inline

: 2over ( x y z -- x y z x y ) pick pick swap ;

: extract ( x/seq -- x ) [ vector? ] [ first ] smart-when ;

: extract-inject ( quot -- quot ) [ extract ] swap compose ;

: newvect ( -- vect ) V{ } clone ;

: open-up ( elt -- elt ) dup 1vector? [ first ] when ;


! Private parsers-related tool-words

: 1vectrow ( inner-arrow node -- vector ) arrow boa 1vector ;

: end-node ( -- node ) node new dup [ drop rot call( ast vector -- ast vector ) ] swap 1vectrow >>sons ;

: 1arrow ( inner-arrow -- arrow ) end-node arrow boa ;

: 1son ( inner-arrow -- vector ) 1arrow 1vector ; 

: uber-seq ( seq -- seq ) dup empty? [ unclip dupd 2vector [ uber-seq ] dip prefix ] unless ;

: parse-arrow ( parser -- arrow ) arrow new swap >>effect ;

: copy-end ( parser -- parser node ) dup out>> first to>> ;

: 1parser ( inner-arrow -- parser ) 1son dup parser boa ;

: min-arrow ( -- arrow ) [ parse-next-raw ] 1arrow ;

: 1curser ( elt quot -- parser ) curry >quotation 1parser ;

: 2curser ( elt elt quot -- parser ) 2curry >quotation 1parser ;

: extract-1curser ( elt quot -- parser ) extract-inject 1curser ;


! Private core-words

: parse-check ( -- ) parse-canceled get-global [ parse-error ] when ;

: ast-combine ( ast ast -- ast ) over parse-null = [ nip ] [ dup parse-null = [ drop ] [ open-up [ dup vector? [ 1vector ] unless ] dip suffix ] if ] if ;

: parse-module ( quot ast vector node parser -- ast vector )
    swapd [ [ over [ 2swap ] 2dip parse-null = [ [ dup { [ 1vector? ] [ parse-null = ] } 1|| [ 1vector ] unless ] 2dip ] when parse-next ] 3curry ] 2dip (parse) ;

: parse-unfold ( quot ast vector seq -- ast vector ) parse-check quap [ quap [ to>> ] [ effect>> ] bi dup quotation?
    [ call( quot ast vector node -- ast vector ) ] [ parse-module ] if 2vector ] 3curry attempt-all first2 ;

: parse-next-raw ( quot ast vector node -- ast vector ) [ action>> ] [ sons>> ] bi swapd [ call( ast -- ast ) ] 2dip parse-unfold ;

: parse-next ( quot ast ast vector node -- ast vector ) [ ast-combine ] 2dip parse-next-raw ;

: parse-next-not ( quot ast vector node -- ast vector ) parse-check sons>> first [ to>> ] [ effect>> ] bi
    swapd [ ] -rot [ (parse) ] 2keep drop nip swap parse-failed = [ swap parse-next-raw ] [ parse-error ] if ;

: parse-next-fence ( quot ast vector seq -- ast vector ) parse-check sons>> first [ to>> ] [ effect>> ] bi parse-module over parse-failed = [ parse-error ] when ;

: (parse) ( quot vector parser -- ast vector ) in>> [ parse-null ] 2dip parse-unfold ;


! Private parser-specific words

: (arb) ( ranging -- parser ) [ [ swap dup length ] dip call trap [ [ swap cut* v2str swap ] dip parse-next ] 2curry attempt-all ] 1curser ; inline

: p-balanced? ( vector -- ? ) 0 swap [ dup empty? not pick 0 >= and ] [ unclip-last { { 40 [ [ 1 + ] dip ] } { 41 [ [ 1 - ] dip ] } [ drop ] } case ] while drop 0 = ;

: (cancel) ( -- ) t parse-canceled set-global parse-error ;

: (succeed) ( quot ast vector node -- ast vector ) [ parse-next-raw ] [ drop (succeed) ] recover ; 

: (arbno) ( parser -- parser ) [ min-arrow [ 1vector dup ] [ to>> ] bi [ rot ] dip [ arrow boa dup ] keep sons>> ] dip call suffix parser boa ; inline

: til-cond ( vector string cond -- string vector ) [ "" ] 3dip [ [ unclip-last ] dip 2dup ] swap compose [ [ over empty? not ] dip [ parse-null swap f ] if ] curry [ swapd [ suffix ] 2dip ] while drop [ parse-null = ] [ suffix ] [ drop ] smart-if ; inline

: (break) ( vector string -- string vector ) [ member? not ] til-cond ;

: (breakx) ( quot ast ast vector node string n -- ast vector ) [ 0 = [ parse-error ] when swap [ (break) ] dip [ append ] 2dip ] 2keep
    [ 2drop parse-next ] [ drop 1 - [ [ pop suffix ] keep ] 3dip (breakx) ] recover ;

: (len) ( quot ast vector node n -- ast vector ) swapd cut* v2str trap parse-next ;

: force-back ( quot ast vector node -- ast vector ) drop [ 2drop parse-failed ] dip ;

: 1cond ( string cond -- vector ) extract-inject [ rot [ [ unclip-last ] 2dip [ drop 1string swap ] [ parse-error ] smart-if ] dip parse-next ] 2curser ;

: (nspan) ( vector string -- string vector ) [ member? ] til-cond ;

: pos-extract ( vector node calc -- vector node y ) [ over length ] dip call( x -- y ) ;

: pos-reverse ( x y -- z ) parse-length get rot - - ;

: (rpos) ( n cond -- parser ) extract-inject curry [ pos-extract 0 = [ parse-error ] unless parse-next-raw ] 1curser ;

: (rtab) ( n cond -- parser ) extract-inject curry [ pos-extract (len) ] 1curser ;

! '-' denotes ranges. '-' at the beginning of the pattern (or after an initial ^ when passed to range-pattern), or just after a previous range counts as itself. '-' ending the pattern is an error.
: (range-pattern) ( string -- parser ) "" swap [ dup empty? not ] [ unclip swap dup empty? [ t ] [ unclip dup 45 = [ drop unclip swap [ [a,b] ] dip f ] [ prefix t ] if ] if [ [ suffix! ] dip ] [ [ append ] dip ] if ] while drop any-from ;

! Vocabulary

! Arrow effect : ( quot ast vector node -- ast vector )

: token ( string -- parser ) [ swapd str2v dup [ length cut* ] dip [ [ = ] 2all? ] keep swap [ parse-error ] unless v2str trap parse-next ] 1curser ; 

: 1token ( string -- parser ) 1string token ;

: seq ( sequence -- parser ) dup empty? f assert= unclip-last 1son [ [ over empty? not ] [ [ unclip-last node new [ 1vectrow ] keep ] dip >>sons drop ] while ] keep parser boa nip ;

: 2seq ( parser parser -- parser ) 2array seq ;

: 3seq ( parser parser parser -- parser ) 3array seq ;

: 4seq ( parser parser parser parser -- parser ) 4array seq ;

: & ( parser parser -- parser ) 2seq ;

: choice ( sequence -- parser ) >vector end-node [ arrow boa ] curry map dup parser boa ;

: seq* ( quot -- parser ) { } make seq ; inline 

: 2choice ( parser parser -- parser ) 2array choice ;

: 3choice ( parser parser parser -- parser ) 3array choice ;

: 4choice ( parser parser parser parser -- parser ) 4array choice ;

: | ( parser parser -- parser ) 2choice ;

: choice* ( quot -- parser ) { } make choice ; inline 

: arb ( -- parser ) [ [0,b) ] (arb) ;

! The same, being greedy

: arbg ( -- parser ) [ 0 (a,b] ] (arb) ;

: bal ( -- parser ) [ swap dup length [1,b] trap [ [ swap cut* dup p-balanced? [ parse-error ] unless v2str swap ] dip parse-next ] 2curry attempt-all ] 1parser ;

: cancel ( -- parser ) [ (cancel) ] 1parser ;

: fail ( -- parser ) [ parse-error ] 1parser ;

: fence ( -- parser ) [ { t f } [ [ parse-next-raw ] [ (cancel) ] if ] attempt-all ] 1parser ;

: rest ( -- parser ) [ [ v2str newvect ] dip parse-next ] 1parser ;

: succeed ( -- parser ) [ (succeed) ] 1parser ;

: any-char ( -- parser ) [ [ dup empty? [ parse-error ] when unclip-last swap ] dip parse-next ] 1parser ;

: any-from ( string -- parser ) [ member? ] 1cond ;

: arbno ( parser -- parser ) [ push ] (arbno) ;

: arbnog ( parser -- parser ) [ [ pop swap ] keep [ push ] keep push ] (arbno) ;

: repeat0 ( parser -- parser ) arbnog ;

: break ( string -- parser ) [ swap [ (break) ] dip parse-next ] extract-1curser ;

: breakx ( string -- parser ) [ [ "" ] 3dip pick over [ member? ] curry count (breakx) ] extract-1curser ;

: fenceno ( parser -- parser ) [ [ parse-next-raw ] [ drop force-back ] recover ] 1parser & 1son node new over >>sons [ parse-next-fence ] swap 1vectrow swap parser boa ; 

: len ( n -- parser ) [ (len) ] extract-1curser ;

: not-any ( string -- parser ) [ member? not ] 1cond ;

: nspan ( string -- parser ) [ swap [ (nspan) ] dip parse-next ] extract-1curser ;

: span ( string -- parser ) [ swap [ [ dup last ] dip [ member? [ parse-error ] unless ] keep (nspan) ] dip parse-next ] extract-1curser ;

: pos ( n -- parser ) [ pos-reverse ] (rpos) ;

: rpos ( n -- parser ) [ - ] (rpos) ;

: tab ( n -- parser ) [ pos-reverse ] (rtab) ;

: rtab ( n -- parser ) [ - ] (rtab) ;

: ensure ( parser -- parser ) newvect ! A recipient for parsing state backup
[ [ copy-end nip ] dip [ [ first2 2swap 2drop ] curry dip parse-next-raw ] curry >quotation 1son [ >>sons drop ] keep ] ! Get and set the parsing state back
[ [ in>> ] dip [ [ pick ] dip [ push ] keep [ over ] dip push parse-next-raw ] curry >quotation node new [ 1vectrow ] keep rot >>sons drop ] ! Get and set the parsing state back
2bi swap parser boa ;

: ensure-not ( parser -- parser ) [ force-back ] 1parser | 1son node new over >>sons [ parse-next-not ] swap 1vectrow swap parser boa ; 

: exactly-n ( parser n -- parser ) dup 0 > t assert= [ dup end-node arrow boa 1vector dup swapd ] dip [ 1 - dup 0 = not ] [ [ dupd node new swap >>sons arrow boa 1vector ] dip ] while drop nip swap parser boa ;

: at-most-n ( parser n -- parser ) dup 0 > t assert= [ dup end-node [ arrow boa ] [ [ parse-next-raw ] swap arrow boa ] bi [ 2vector dup swapd ] keep ] dip [ 1 - dup 0 = not ] [ [ dupd node new swap >>sons arrow boa ] 2dip [ [ 2vector ] keep ] dip ] while 2drop nip swap parser boa ;

: at-least-n ( parser n -- parser ) [ exactly-n ] curry [ arbnog ] bi & ;

: from-m-to-n ( parser m n -- parser ) [ over ] dip [ at-least-n ensure ] [ at-most-n ] 2bi* & ;

: range-pattern ( string -- parser ) dup first 94 = [ 1 tail (range-pattern) ensure-not any-char & ] [ (range-pattern) ] if  ;

: parse ( string parser -- ast ) [ str2v [ ] swap ] dip f parse-canceled set-global over length parse-length [ (parse) ] with-variable drop open-up ;

: parse-nostart ( string parser -- ast ) over length [0,b] trap [ rot tail swap parse ] 2curry attempt-all ;

: action ( parser quot: ( ast -- ast ) -- parser ) [ copy-end dup action>> ] dip compose >quotation >>action drop ;

: (repeat1) ( parser parser -- parser ) arbnog 2seq [ first2 swap prefix ] action ;

: repeat1 ( parser -- parser ) dup (repeat1) ; 

: ignore ( parser -- parser ) [ drop parse-null ] action ;

: list-of ( item separator -- parser ) ignore over 2seq (repeat1) ;

: pack  ( begin body end -- parser ) [ ignore ] 2dip ignore 3seq ;

: surrounded-by ( parser begin end -- parser ) [ token ] bi@ swapd pack ;

: 'digit' ( -- parser ) [ [ unclip-last dup digit? [ parse-error ] unless swap ] dip parse-next ] 1parser [ digit> ] action ;

: 'integer' ( -- parser ) 'digit' repeat1 [ 10 digits>integer ] action ;

: 'string' ( -- parser ) arb "\"" dup surrounded-by ;

