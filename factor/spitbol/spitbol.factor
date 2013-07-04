! Copyright (C) 2013 Aurélien Martin
! See http://factorcode.org/license.txt for BSD license.
USING: kernel io sequences ascii math alien.syntax vectors quotations math.ranges accessors continuations strings generalizations arrays prettyprint combinators namespaces combinators.short-circuit combinators.smart ;
IN: spitbol

CONSTANT: parse-null ;

ERROR: parse-error ;

TUPLE: parser { in vector } { out vector } ;

TUPLE: node { sons vector } { action quotation } ;

UNION: inner-arrow quotation parser ;

TUPLE: arrow { effect inner-arrow } { to node } ;

! Parser effect : ( quot ast vector node -- ast vector )

: str2v ( string -- vector ) >vector reverse ;

: v2str ( vector -- string ) reverse >string ;

: trap ( x y z -- z y x ) swapd swap swapd ;

: 2swap ( x y z t -- z t x y ) [ trap ] dip trap ;

: quap ( x y z t -- t z y x ) 2swap [ swap ] 2bi@ ;

: 1vector? ( x -- ? ) { [ vector? ] [ length 1 = ] } 1&& ;

: 2vector ( x y -- vector ) V{ } 2sequence ;

: 4curry ( a b c d quot -- quot ) 2curry 2curry ; inline

DEFER: (parse)

DEFER: parse-next

SYMBOL: parse-canceled

: end-node ( -- node ) node new dup [ drop rot call( ast vector -- ast vector ) ] swap arrow boa 1vector >>sons ;

: 1arrow ( inner-arrow -- arrow ) end-node arrow boa ;

: 1son ( inner-arrow -- vector ) 1arrow 1vector ; 

: uber-seq ( seq -- seq ) dup V{ } = [ unclip dupd 2vector [ uber-seq ] dip prefix ] unless ;

: ast-combine ( ast ast -- ast ) over parse-null = [ nip ] [ dup parse-null = [ drop ] [ [ dup vector? [ 1vector ] unless ] dip suffix ] if ] if ;

: parse-unfold ( quot ast vector seq -- ast vector ) parse-canceled get-global [ parse-error ] when quap [ quap [ to>> ] [ effect>> ] bi dup quotation?
   [ call( quot ast vector node -- ast vector ) ]
   [ swapd [ [ over [ 2swap ] 2dip parse-null = [ [ dup { [ 1vector? ] [ parse-null = ] } 1|| [ 1vector ] unless ] 2dip ] when parse-next ] 3curry ] 2dip (parse) ]
   if 2vector ] 3curry attempt-all first2 ;

: parse-next-raw ( quot ast vector node -- ast vector ) [ action>> ] [ sons>> ] bi swapd [ call( ast -- ast ) ] 2dip parse-unfold ;

: parse-next ( quot ast ast vector node -- ast vector ) [ ast-combine ] 2dip parse-next-raw ;

: 1parser ( inner-arrow -- parser ) 1son dup parser boa ;

: min-arrow ( -- arrow ) [ parse-next-raw ] 1arrow ;

: arb ( -- parser ) [ swap dup length [0,b) trap [ [ swap cut* v2str swap ] dip parse-next ] 2curry attempt-all ] 1parser ;

: p-balanced? ( vector -- ? ) 0 swap [ dup V{ } = not pick 0 >= and ] [ unclip-last { { 40 [ [ 1 + ] dip ] } { 41 [ [ 1 - ] dip ] } [ drop ] } case ] while drop 0 = ;

: bal ( -- parser ) [ swap dup length [1,b] trap [ [ swap cut* dup p-balanced? [ parse-error ] unless v2str swap ] dip parse-next ] 2curry attempt-all ] 1parser ;

: (cancel) ( -- ) t parse-canceled set-global parse-error ;

: cancel ( -- parser ) [ (cancel) ] 1parser ;

: fail ( -- parser ) [ parse-error ] 1parser ;

: fence ( -- parser ) [ { t f } [ [ parse-next-raw ] [ (cancel) ] if ] attempt-all ] 1parser ;

: rest ( -- parser ) [ [ v2str V{ } ] dip parse-next ] 1parser ;

: (succeed) ( quot ast vector node -- ast vector ) [ parse-next-raw ] [ drop (succeed) ] recover ; 

: succeed ( -- parser ) [ (succeed) ] 1parser ;

: any-char ( string -- parser ) [ -rot [ unclip-last rot [ member? ] [ drop 1string swap ] [ parse-error ] smart-if ] dip parse-next ] curry >quotation 1parser ;

: arbno ( parser -- parser ) min-arrow [ 1vector dup ] [ to>> ] bi [ rot ] dip [ arrow boa ] keep sons>> [ push ] keep suffix parser boa ;

: (break) ( vector string -- string vector ) "" -rot [ [ unclip-last ] dip 2dup member? not ] [ swapd [ suffix ] 2dip ] while drop suffix ;

: break ( string -- parser ) [ swap [ (break) ] dip parse-next ] curry >quotation 1parser ;

: 1token ( string -- parser ) [ swapd str2v dup [ length cut* ] dip [ [ = ] 2all? ] keep swap [ parse-error ] unless v2str trap parse-next ] curry >quotation 1parser ; 

: parse-arrow ( parser -- arrow ) arrow new swap >>effect ;

: copy-end ( parser -- parser node ) dup out>> first to>> ;

: | ( parser parser -- parser ) end-node [ arrow boa ] curry bi@ 2vector dup parser boa ;

: & ( parser parser -- parser ) 1son [ node new [ arrow boa 1vector ] keep ] dip [ >>sons drop ] keep parser boa ;

: ensure ( parser -- parser ) V{ } ! A recipient for parsing state backup
    [ [ copy-end nip ] dip [ [ first2 2swap 2drop ] curry dip parse-next-raw ] curry >quotation 1son [ >>sons drop ] keep ] ! Get and set the parsing state back
    [ [ in>> ] dip [ [ pick dup . ] dip [ push ] keep [ over ] dip push parse-next-raw ] curry >quotation node new [ arrow boa 1vector ] keep rot >>sons drop ] ! Get and set the parsing state back
    2bi swap parser boa ;

: (parse) ( quot vector parser -- ast vector ) in>> [ parse-null ] 2dip parse-unfold ;

: parse ( string parser -- ast ) [ str2v [ ] swap ] dip f parse-canceled set-global (parse) drop dup 1vector? [ first ] when ;

: action ( parser quot: ( ast -- ast ) -- parser ) [ copy-end dup action>> ] dip compose >quotation >>action drop ;

