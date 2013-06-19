! See http://factorcode.org/license.txt for BSD license.
USING: kernel math irc.client irc.client.chats irc.messages irc.messages.parser.private sequences accessors math.factorials peg.ebnf math.parser vectors strings generalizations ;
IN: td2

: connect-bot ( -- client channel )
! Define channel
"#inf355" <irc-channel-chat>
! Configure
"irc.freenode.org" irc-port "nitruk-bot" f <irc-profile> <irc-client>
! Connect
dup connect-irc
! Join
2dup attach-chat swap ;

: operands ( seq -- x y ) [ first ] [ second ] bi ;

: parse-message ( str -- str ) [EBNF
num = ([0-9])+ => [[ string>number ]]
ping = "Ping" => [[ drop "Pong " >vector ]]
fact = "Quelle est la factorielle de "~ num " ?"~ => [[ factorial number>string ]]
name = "Comment t'appelles-tu ?" => [[ drop V{ "Je m'appelle " "me" } ]]
nick = "Quel est mon "~ ("pseudo" | "nick") " ?"~ => [[ V{ } [ "Votre " " est " surround ] dip [ push ] keep "you" swap [ push ] keep ]]
textsumleft = "la somme de "~ num " et "~ num => [[ operands + ]] 
textsubleft = "la différence de "~ num " et "~ num => [[ operands - ]] 
textprodleft = "le produit de "~ num " et "~ num => [[ operands * ]] 
textsumright = (num | calctext) (" ajouté à " | " ajoutée à ")~ (num | calctext) => [[ operands + ]]
textsubright = (num | calctext) (" auquel on soustrait " | " à laquelle on soustrait ")~ (num | calctext) => [[ operands - ]]
textprodright = (num | calctext) (" multiplié par " | " multipliée par ")~ (num | calctext) => [[ operands * ]]
calctext = textsumright | textsubright | textprodright | textsumleft | textsubleft | textprodleft
text = ("Quel est " | "Quelle est ")~ calctext " ?"~ => [[ number>string ]]
digitsum = calcdigit "+"~ calcdigit => [[ operands + ]]
digitsub = calcdigit "-"~ calcdigit => [[ operands - ]]
digitprod = calcdigit "*"~ calcdigit => [[ operands * ]]
parenthesis = "("~ calcdigit ")"~
calcdigit = digitsum | digitsub | digitprod | parenthesis | num
digit = "Calcule "~ calcdigit => [[ number>string ]]
other = . => [[ drop "" ]]
message = ping | fact | name | nick | text | digit | other
EBNF] ;

: main-loop ( client channel -- client channel )
! Get next-message
dup hear
! If it is a newcomer, welcome them
dup join? [ sender>> "Welcome " " !" surround [ swap speak ] 2keep ]
! If it is a private message, study it
[ dup privmsg? [ [ sender>> ] [ text>> ] bi
parse-message dup "" =
[ [ drop ] dip ]
[ dup vector? [ [ first ] [ second ] bi "me" = [ [ drop ] dip [ dup ] 2dip 2 1 mnswap nick>> append ] [ swap append ] if ] [ [ drop ] dip ] if
[ swap speak ] 2keep ] if ] when ] if
! Start again
drop main-loop ;
