FILES
=====

whbot.erl          The ejabberd module
whrecords.erl      An include file with definitions of the records used
whweb.erl          The Yaws appmod
mod_mnesia.erl     An ejabberd module which is used to start ejabberd (can probably be refactored out of existence)
ymnesia.erl        A Yaws appmod which allows browsing of mnesia databases (useful for debugging)
mochijson2.erl     A library for handling JSON in Erlang.  Originally part of the Mochiweb framework, also used in CouchDB
mochinum.erl       A library for handling number/string conversions.  Used by mochijson2.


INSTALLATION
============

1) Install ejabberd  (http://www.ejabberd.im/download)

2) Install yaws ("sudo apt-get install yaws" or equivalent)

3) Compile the various .erl files
   Files which are ejabberd modules should be compiled with the ejabberd/src dir in the path:
   erlc -I /path/to/ejabberd/src <filename>
   Files which are Yaws appmods should be compiled with the Yaws include dir in the path:
   erlc -I /path/to/yaws/include <filename>
   
4) Copy the .beam files to the right place
   ejabberd mods go to /ejabberd/lib/ejabberd*/ebin/ (depending on where ejabberd is installed)
   Yaws appmods go to /path/to/yaws/ebin/ (could be something like /usr/lib/erlang/lib/yaws*/ebin)
   
5) Add the ejabberd mods to ejabberd.cfg - good instructions here:
   http://anders.conbere.org/journal/building-ejabberd-modules-part-2-generic-modules/
   
6) start ejabberd


WHAT HAPPENS NEXT
=================

The bot should start up with the name trader.<host>, e.g. trader.wisdomhive.com - this depends on ejabberd config
At the moment, it doesn't seem to do presence properly, but I haven't looked into this.  This means that the bot won't necessarily show up as being 'online', but it will still respond to messages.

Yaws, as per the settings in mod_mnesia.erl, will run on port 8001.  It currently only implements one meaningful path, "/wh/markets".

BOT COMMANDS
============

Any unrecognised commands will simply be echoed back to the user.  This is an "is this thing on?" sanity check for now.

create-account
  -- creates an account for the current XMPP user

create-market <name>
  -- creates a market with the name provided.  <name> cannot contain whitespace.

create-contract <market-name> <contract-name>
  -- creates a contract with a given name on a market created using create-market.  Same rules regarding whitespace apply.

buy <market-name> <contract-name> <quantity>
  -- buys <quantity> of the contract on the identified market.  May return a generic error if the user does not have the funds available.

sell <market-name> <contract-name> <quantity>
  -- inverse of 'buy'

reset
  -- wipes all tables
  
list-contracts
  -- does not work now.  The REST interface can be used for read access.  It's not clear that an XMPP implementation is needed.
  
  
TODO
====

* Commands and replies in XML/JSON
* Error reporting sucks.  If an error occurs, a generic message is returned.  An abstract error-handling/error message system is needed.
* REST access needs to support PUT, POST and DELETE for update, creation and deletion of markets/contracts
* RESTful URL scheme needs to be defined
* The XMPP bot needs to send out notifications when changes are made, so some system for knowing which users want to know about changes to certain markets is needed.  For a first implementation, it may be simpler to just implement a 'firehose' that informs users of all trades everywhere in the system.
