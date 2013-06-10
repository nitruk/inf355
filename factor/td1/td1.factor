! Copyright (C) 2013 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel math ;
IN: td1

: fact ( n -- n' ) dup 0 <=
[
drop 1
] [
[ 1 - fact ] keep *
] if ;
