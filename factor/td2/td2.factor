! See http://factorcode.org/license.txt for BSD license.
USING: kernel irc.client irc.client.chats irc.messages irc.messages.parser.private sequences accessors ;
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

: main-loop ( channel -- channel )
! Get next-message
dup hear
! If it is a newcomer, welcome them
dup join? [ sender>> "Welcome " " !" surround [ swap speak ] 2keep ]
! If it is a private message, study it
[ dup privmsg? [ [ sender>> ] [ text>> ] bi
! Case Ping
dup "Ping" = [ drop "Pong " swap append [ swap speak ] 2keep ]
[ drop ] if ] when ] if
! Start again
drop main-loop ;
