/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
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

:- module(swish_config,
	  [ swish_reply_config/2,	% +Request, +Options
	    swish_config/2,		% ?Type, ?Config
	    swish_config_hash/2		% -HASH, +Options
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(option)).

:- multifile
	config/2,			% ?Key, ?Value
	config/3,			% ?Key, ?Value, +Options
	source_alias/2,			% ?Alias, ?Options
	authenticate/2.			% +Request, -User

/** <module> Make HTTP locations known to JSON code
*/

		 /*******************************
		 *	       CONFIG		*
		 *******************************/

%%	swish_reply_config(+Request, +Options) is semidet.
%
%	Emit a configuration object to the client if the client requests
%	for '.../swish_config.json', regardless  of   the  path  prefix.

swish_reply_config(Request, Options) :-
	option(path(Path), Request),
	file_base_name(Path, 'swish_config.json'),
	json_config(JSON, Options),
	reply_json(JSON).

%%	swish_config_hash(-Hash, +Options) is det.
%
%	True if Hash is the SHA1 of the SWISH config.

swish_config_hash(Hash, Options) :-
	json_config(Config, Options),
	variant_sha1(Config, Hash).

json_config(json{ http: json{ locations:JSON
			    },
		  swish: SWISHConfig
		}, Options) :-
	http_locations(JSON),
	swish_config_dict(SWISHConfig, Options).

http_locations(JSON) :-
	findall(ID-Path,
		( http_current_handler(Path, _:_, Options),
		  memberchk(id(ID), Options)
		), Pairs),
	keysort(Pairs, Sorted),
	remove_duplicate_ids(Sorted, Cleaned),
	dict_pairs(JSON, json, Cleaned).

remove_duplicate_ids([], []).
remove_duplicate_ids([Id-Path1,Id-Path2|T], [Id-Path1|Cleaned]) :- !,
	same_ids(T, Id, T1, Paths0),
	sort([Path1,Path2|Paths0], Unique),
	(   Unique = [_]
	->  true
	;   print_message(warning, http(duplicate_handlers(Id, Unique)))
	),
	remove_duplicate_ids(T1, Cleaned).
remove_duplicate_ids([H|T0], [H|T]) :-
	remove_duplicate_ids(T0, T).

same_ids([], _, [], []).
same_ids([Id-Path|T0], Id, T, [Path|TP]) :- !,
	same_ids(T0, Id, T, TP).
same_ids(T, _, T, []).


%%	swish_config_dict(-Config:dict, +Options) is det.
%
%	Obtain name-value pairs from swish_config:config/2

swish_config_dict(Config, Options) :-
	findall(Key-Value, swish_config(Key, Value, Options), Pairs),
	dict_pairs(Config, json, Pairs).

%%	config(-Key, -Value) is nondet.
%%	swish_config(-Key, -Value) is nondet.
%
%	Define a name/value pair that will end   up  in the SWISH config
%	object (see =web/js/config.js=)

swish_config(Key, Value) :-
	swish_config(Key, Value, []).

swish_config(Key, Value, Options) :-
	config(Key, Value, Options).
swish_config(Key, Value, _) :-
	config(Key, Value).

%%	source_alias(?Alias, ?Options) is nondet.
%
%	Multifile hook that  defines   properties  of file_search_path/2
%	aliases wrt. storage handling.  Defined options are:
%
%	  - access(Access)
%	  One of `read` or `both`.
%	  - search(Pattern)
%	  The _New Tab_ search form searches in files that satisfy the
%	  given pattern in the matching directories.  Pattern is handed
%	  to expand_file_name/2.

		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(http(duplicate_handlers(Id, Paths))) -->
	[ 'Duplicate HTTP handler IDs: "~w"'-[Id] ],
	paths(Paths).

paths([]) --> [].
paths([H|T]) --> [ '\t~q'-[H], nl ], paths(T).
