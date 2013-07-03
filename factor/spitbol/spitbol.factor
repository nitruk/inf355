! Copyright (C) 2013 Aurélien Martin
! See http://factorcode.org/license.txt for BSD license.
USING: kernel io sequences ascii math alien.syntax vectors quotations math.ranges accessors continuations strings generalizations arrays prettyprint backtrack ;
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

: 2vector ( x y -- vector ) V{ } 2sequence ;

: 4curry ( a b c d quot -- quot ) 2curry 2curry ; inline

DEFER: (parse)

DEFER: parse-next

: uber-seq ( seq -- seq ) dup V{ } = [ unclip dupd 2vector [ uber-seq ] dip prefix ] unless ;

: ast-combine ( ast ast -- ast ) over parse-null = [ nip ] [ [ dup vector? [ 1vector ] unless ] dip suffix ] if ;

: parse-unfold ( quot ast vector seq -- ast vector ) quap [ quap [ to>> ] [ effect>> ] bi dup quotation?
   [ call( quot ast vector node -- ast vector ) ]
   [ swapd [ [ over [ 2swap ] 2dip parse-null = [ [ 1vector ] 2dip ] when parse-next ] 3curry ] 2dip (parse) ]
   if 2vector ] 3curry attempt-all first2 ;

: parse-next-raw ( quot ast vector node -- ast vector ) [ action>> ] [ sons>> ] bi swapd [ call( ast -- ast ) ] 2dip parse-unfold ;

: parse-next ( quot ast ast vector node -- ast vector ) [ ast-combine ] 2dip parse-next-raw ;

: end-node ( -- node ) node new dup [ drop rot call( ast vector -- ast vector ) ] swap arrow boa 1vector >>sons ;

: 1parser ( inner-arrow -- parser ) end-node arrow boa 1vector dup parser boa ;

: arb ( -- parser ) [ swap dup length [0,b) amb trap [ swap cut* v2str dup print swap ] dip parse-next ] 1parser ;

: 1token ( string -- parser ) [ swapd str2v dup [ length cut* ] dip [ [ = ] 2all? ] keep swap [ fail ] unless v2str trap parse-next ] curry >quotation 1parser ; 

: min-arrow ( -- arrow ) [ parse-next-raw ] end-node arrow boa ;

: parse-arrow ( parser -- arrow ) arrow new swap >>effect ;

: copy-end ( parser -- parser node ) dup out>> first to>> ;

: | ( parser parser -- parser ) end-node [ arrow boa ] curry bi@ 2vector dup parser boa ;

: & ( parser parser -- parser ) end-node arrow boa 1vector [ node new [ arrow boa 1vector ] keep ] dip [ >>sons drop ] keep parser boa ;

: (parse) ( quot vector parser -- ast vector ) in>> [ parse-null ] 2dip parse-unfold ;

: parse ( string parser -- ast ) [ str2v [ ] swap ] dip (parse) drop ;

: action ( parser quot: ( ast -- ast ) -- parser ) [ copy-end dup action>> ] dip compose >quotation >>action drop ;
