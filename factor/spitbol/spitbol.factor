! Copyright (C) 2013 Aurélien Martin
! See http://factorcode.org/license.txt for BSD license.
USING: kernel io sequences ascii math alien.syntax vectors quotations math.ranges accessors continuations strings generalizations ;
IN: spitbol

CONSTANT: parse-end ;

ERROR: parse-error ;

TUPLE: parser { in vector } { out vector } ;

TUPLE: node { sons vector } { action quotation } ;

TUPLE: arrow { effect quotation } { to node } ;

! Bad bad bad
: (parse-int) ( vector -- int ) dup empty?
    [ drop parse-end ]
    [ dup pop dup digit? 
        [ 48 - [ (parse-int) ] dip swap dup parse-end =
            [ drop ]
            [ swap 10 * + ]
            if ]
        [ 2drop parse-end ]
        if ]
    if ;

: parse-int ( string -- int ) >vector reverse (parse-int) dup parse-end = [ drop parse-error ] when ; 

: str2v ( string -- vector ) >vector reverse ;

: v2str ( vector -- string ) reverse >string ;

: trap ( x y z -- z y x ) swapd swap swapd ;

: parse-unfold ( vector seq -- ast ) swap [ swap [ to>> ] [ effect>> ] bi call( vector node -- ast ) ] curry attempt-all ;

: parse-next-raw ( vector node -- ast ) sons>> parse-unfold ;

: parse-next ( ast vector node -- ast ) [ action>> ] [ sons>> ] bi swapd [ call( ast -- ast ) ] 2dip parse-unfold dup parse-end = [ drop ] [ dup vector? [ 1vector ] unless swap prefix ] if ;

: end-node ( -- node ) node new dup [ 2drop parse-end ] swap arrow boa 1vector >>sons ;

: 1parser ( quot: ( vector node -- ast ) -- parser ) end-node arrow boa 1vector dup parser boa ;

: arb ( -- parser ) [ swap dup length [0,b) trap [ [ swap cut* v2str swap ] dip parse-next ] 2curry attempt-all ] 1parser ;

: 1token ( string -- parser ) [ swapd str2v dup [ length cut* ] dip [ [ = ] 2all? ] keep swap [ parse-error throw ] unless v2str trap parse-next ] curry >quotation 1parser ; 

: min-arrow ( -- arrow ) [ parse-next-raw ] end-node arrow boa ;

: choice ( parser parser -- parser ) [ [ in>> ] [ out>> ] bi ] bi@ swapd min-arrow 1vector [ first to>> swap >>sons ] curry bi@ [ append ] 2bi@ parser boa ;

: & ( parser parser -- parser ) [ in>> ] [ out>> ] [ bi* ] 4keep swap bi* [ first to>> ] dip >>sons drop parser boa ;

: parse ( string parser -- ast ) [ str2v ] dip in>> parse-unfold ;

: action ( parser quot: ( ast -- ast ) -- parser ) [ dup out>> first to>> ] dip >>action drop ;
