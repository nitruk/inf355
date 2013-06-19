! See http://factorcode.org/license.txt for BSD license.
USING: kernel irc.client irc.client.chats irc.messages irc.messages.parser.private sequences accessors math.factorials peg.ebnf math.parser vectors strings ;
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

: parse-message ( str -- str ) [EBNF
ping = "Ping" => [[ drop "Pong " >vector ]]
fact = "Quelle est la factorielle de " ([0-9])+ " ?" => [[ 1 swap nth string>number factorial number>string ]]
nick = "Quel est mon " ("pseudo" | "nick") " ?" => [[ 1 swap nth "Votre " " est " surround >vector ]]
other = . => [[ drop "" ]]
message = ping | fact | nick | other
EBNF] ;

: main-loop ( channel -- channel )
! Get next-message
dup hear
! If it is a newcomer, welcome them
dup join? [ sender>> "Welcome " " !" surround [ swap speak ] 2keep ]
! If it is a private message, study it
[ dup privmsg? [ [ sender>> ] [ text>> ] bi
parse-message dup "" =
[ [ drop ] dip ]
[ dup vector? [ >string swap append ] [ [ drop ] dip ] if
[ swap speak ] 2keep ] if ] when ] if
! Start again
drop main-loop ;
