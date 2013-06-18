! See http://factorcode.org/license.txt for BSD license.
USING: tools.test engrammes ;
IN: engrammes.tests

[ "((01)(1)000000000000000000)" ] [ 72 >engramme ] unit-test
[ 72 ] [ 72 >engramme engramme> ] unit-test
