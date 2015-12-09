/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(trill_on_swish_authenticate,
	  [ swish_add_user/3		% +User, +Passwd, +Fields
	  ]).
:- use_module(library(pengines), []).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(crypt)).
:- use_module(library(http/http_authenticate)).

:- use_module(config).
:- use_module(page).

:- multifile
	trill_on_swish_config:config/2,
	trill_on_swish_config:authenticate/2,
	trill_on_swish_config:verify_write_access/3.

/** <module> SWISH login management

This module provides basic login and  password management facilities for
SWISH.  You can create an authenticated SWISH server by

  1. Loading this library
  2. Add one or more users to the passwd file using swish_add_user/3

     ==
     ?- swish_add_user("Bob", "Bob's secret", []).
     ==

As a result, trying to create the  first pengine (e.g., using _|Run!|_),
the server will challenge the user.  The   logged  in  user is available
through pengine_user/1.
*/

:- dynamic
	password_file_cache/1.

password_file(File) :-
	password_file_cache(File), !.
password_file(File) :-
	absolute_file_name(trill_on_swish(passwd), File, [access(read)]),
	asserta(password_file_cache(File)).

logged_in(Request, User) :-
	password_file(File),
	http_authenticate(basic(File), Request, [User|_Fields]), !,
	debug(authenticate, 'Logged in as ~p', [User]).
logged_in(_Request, _User) :-
	throw(http_reply(authorise(basic('SWISH user')))).

%%	pengines:authentication_hook(+Request, +Application, -User)
%
%	Is called from the  /pengine/create   request  to  establish the
%	logged in user.

%pengines:authentication_hook(Request, _Application, User) :-
%	logged_in(Request, User), !.

%pengines:not_sandboxed(_User, _Application).

pengines:authentication_hook(_Request, _Application, anonymous) :- !.

pengines:not_sandboxed(_User, _Application).



%%	trill_on_swish_config:verify_write_access(+Request, +File, +Options)

trill_on_swish_config:verify_write_access(Request, _File, _Options) :-
	logged_in(Request, _User), !.

%%	trill_on_swish_config:authenticate(+Request, -User)
%
%	Called for all SWISH  actions.  May   be  used  to  restrict all
%	access. Access can only be denied by throwing an exception.

trill_on_swish_config:authenticate(Request, User) :-
	\+ swish_config(public_access, true),
	logged_in(Request, User).


%%	swish_add_user(+User, +Passwd, +Fields) is det.
%
%	Add a new user to the SWISH password file.

swish_add_user(User, Passwd, Fields) :-
	phrase("$1$", E, _),		% use Unix MD5 hashes
	crypt(Passwd, E),
	string_codes(Hash, E),

	Entry = passwd(User, Hash, Fields),

	absolute_file_name(trill_on_swish(passwd), File,
			   [access(write)]),
	(   exists_file(File)
	->  http_read_passwd_file(File, Data)
	;   Data = []
	),
	(   selectchk(passwd(User, _, _), Data, Entry, NewData)
	->  true
	;   append(Data, [Entry], NewData)
	),
	http_write_passwd_file(File, NewData).

