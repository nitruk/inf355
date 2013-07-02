! Copyright (C) 2013 Aurélien Martin
! See http://factorcode.org/license.txt for BSD license.
USING: kernel io sequences ascii math alien.syntax vectors quotations math.ranges accessors continuations strings generalizations arrays prettyprint ;
IN: spitbol

CONSTANT: parse-null ;

ERROR: parse-error ;

TUPLE: parser { in vector } { out vector } ;

TUPLE: node { sons vector } { action quotation } ;

UNION: inner-arrow quotation parser ;

TUPLE: arrow { effect inner-arrow } { to node } ;

! Parser effect : ( ast vector node -- ast vector )

: str2v ( string -- vector ) >vector reverse ;

: v2str ( vector -- string ) reverse >string ;

: trap ( x y z -- z y x ) swapd swap swapd ;

: 2vector ( x y -- vector ) V{ } 2sequence ;

DEFER: (parse)

DEFER: parse-next

: ast-combine ( ast ast -- ast ) over parse-null = [ nip ] [ [ dup vector? [ 1vector ] unless ] dip suffix ] if ;

: parse-unfold ( ast vector seq -- ast vector ) trap [ trap [ to>> ] [ effect>> ] bi dup quotation?
   [ call( ast vector node -- ast vector ) ]
   [ nip (parse) [ ast-combine ] dip ] if 2array ] 2curry attempt-all first2 ;

: parse-next-raw ( ast vector node -- ast vector ) [ action>> ] [ sons>> ] bi swapd [ call( ast -- ast ) ] 2dip parse-unfold ;

: parse-next ( ast ast vector node -- ast vector ) [ ast-combine ] 2dip parse-next-raw ;

: end-node ( -- node ) node new dup [ drop ] swap arrow boa 1vector >>sons ;

: 1parser ( inner-arrow -- parser ) end-node arrow boa 1vector dup parser boa ;

: arb ( -- parser ) [ swap dup length [0,b) trap [ [ swap cut* v2str swap ] dip parse-next ] 2curry attempt-all ] 1parser ;

: 1token ( string -- parser ) [ swapd str2v dup [ length cut* ] dip [ [ = ] 2all? ] keep swap [ parse-error throw ] unless v2str trap parse-next ] curry >quotation 1parser ; 

: min-end ( -- node arrow ) end-node [ [ parse-next-raw ] ] keep arrow boa ;

: parse-arrow ( parser -- arrow ) arrow new swap >>effect ;

: copy-end ( parser -- parser node ) dup out>> first to>> ;

: choice ( parser parser -- parser ) end-node [ arrow boa ] curry bi@ 2vector dup parser boa ;

: & ( parser parser -- parser ) copy-end min-end swapd 1vector >>sons drop arrow boa 1vector [ [ copy-end ] dip >>sons drop parse-arrow 1vector ] keep parser boa ;

: (parse) ( vector parser -- ast vector ) in>> [ parse-null ] 2dip parse-unfold ;

: parse ( string parser -- ast ) [ str2v ] dip (parse) drop ;

: action ( parser quot: ( ast -- ast ) -- parser ) [ dup out>> first to>> ] dip >>action drop ;

: test2 ( cont val -- e ) [ continue-with ] keep ;

: test1 ( a b c -- d e f ) + * [ test2 ] curry callcc1 dup dup ;
