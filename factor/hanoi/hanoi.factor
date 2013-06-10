! Copyright (C) 2013 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel math math.parser sequences io ;
IN: hanoi

: move ( a b -- str ) [ 10 >base ] bi@ " vers " glue ;

: other ( a b -- o ) + 6 - neg ;

: partial ( a b -- a o ) dupd other ;

: hanoi ( d a n -- ) dup 0 <=
[ 3drop ]
[ [ 1 - [ partial ] dip [ hanoi ] 3keep ] 3keep
drop move print [ swap partial ] dip hanoi ]
if ;
