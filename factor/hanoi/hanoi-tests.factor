! Copyright (C) 2013 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test hanoi ;
IN: hanoi.tests

[ "5 vers 3" ] [ 5 3 move ] unit-test
[ "666 vers 42" ] [ 666 42 move ] unit-test

[ 1 ] [ 3 2 other ] unit-test
[ 2 ] [ 1 3 other ] unit-test
[ 3 ] [ 1 2 other ] unit-test

[ 3 2 ] [ 3 1 partial ] unit-test
