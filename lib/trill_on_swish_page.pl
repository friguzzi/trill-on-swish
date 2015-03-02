/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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

:- module(trill_on_swish_page,
	  [ trill_on_swish_reply/2,			% +Options, +Request
	    trill_on_swish_page//1,			% +Options

	    trill_on_swish_navbar//1,			% +Options
	    trill_on_swish_content//1,			% +Options

	    trill_on_swish_resources//0,
	    trill_on_swish_js//0,
	    trill_on_swish_css//0
	  ]).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_path)).
:- if(exists_source(library(http/http_ssl_plugin))).
:- use_module(library(http/http_ssl_plugin)).
:- endif.
:- use_module(library(debug)).
:- use_module(library(time)).
:- use_module(library(option)).

:- use_module(trill_on_swish_config).
:- use_module(trill_on_swish_help).
:- use_module(trill_on_swish_form).
:- use_module(trill_on_swish_search).

/** <module> Provide the SWISH application as Prolog HTML component

This library provides the SWISH page  and   its  elements as Prolog HTML
grammer rules. This allows for server-side   generated  pages to include
swish or parts of swish easily into a page.
*/

http:location(tos_pldoc, trill_on_swish(tos_pldoc), [priority(100)]).

:- http_handler(trill_on_swish(.), trill_on_swish_reply([]), [id(trill_on_swish), prefix, priority(100)]).

:- multifile
	trill_on_swish_config:trill_on_swish_source_alias/1,
	trill_on_swish_config:trill_on_swish_reply_page/1.

%%	trill_on_swish_reply(+Options, +Request)
%
%	HTTP handler to reply the  default   SWISH  page.  Processes the
%	following parameters:
%
%	  - code(Code)
%	  Use Code as initial code. Code is either an HTTP url or
%	  concrete source code.
%	  - background(Code)
%	  Similar to Code, but not displayed in the editor.
%	  - examples(Code)
%	  Provide examples. Each example starts with ?- at the beginning
%	  of a line.
%	  - q(Query)
%	  Use Query as the initial query.

trill_on_swish_reply(_, Request) :-
	serve_resource(Request), !.
trill_on_swish_reply(_, Request) :-
	trill_on_swish_reply_config(Request), !.
trill_on_swish_reply(SwishOptions, Request) :-
	Params = [ code(_,	 [optional(true)]),
		   background(_, [optional(true)]),
		   examples(_,   [optional(true)]),
		   q(_,          [optional(true)]),
		   format(_,     [oneof([trill_on_swish,raw]), default(trill_on_swish)])
		 ],
	http_parameters(Request, Params),
	params_options(Params, Options0),
	merge_options(Options0, SwishOptions, Options1),
	source_option(Request, Options1, Options2),
	trill_on_swish_reply1(Options2).

trill_on_swish_reply1(Options) :-
	option(code(Code), Options),
	option(format(raw), Options), !,
	format('Content-type: text/x-prolog~n~n'),
	format('~s~n', [Code]).
trill_on_swish_reply1(Options) :-
	trill_on_swish_config:trill_on_swish_reply_page(Options), !.
trill_on_swish_reply1(Options) :-
	reply_html_page(
	    trill_on_swish(main),
	    [ title('TRILL on SWISH -- SWI-Prolog for SHaring'),
	      link([ rel('shortcut icon'),
		     href('/icons/favicon.ico')
		   ]),
	      link([ rel('apple-touch-icon'),
		     href('/icons/swish-touch-icon.png')
		   ])
	    ],
	    \trill_on_swish_page(Options)).

params_options([], []).
params_options([H0|T0], [H|T]) :-
	arg(1, H0, Value), nonvar(Value), !,
	functor(H0, Name, _),
	H =.. [Name,Value],
	params_options(T0, T).
params_options([_|T0], T) :-
	params_options(T0, T).


%%	source_option(+Request, +Options0, -Options)
%
%	If the data was requested  as   '/Alias/File',  reply using file
%	Alias(File).

source_option(_Request, Options, Options) :-
	option(code(_), Options),
	option(format(trill_on_swish), Options), !.
source_option(Request, Options0, Options) :-
	option(path_info(Info), Request),
	Info \== 'index.html', !,	% Backward compatibility
	(   source_data(Info, String)
	->  Options = [code(String)|Options0]
	;   http_404([], Request)
	).
source_option(_, Options, Options).

source_data(Info, Code) :-
	sub_atom(Info, B, _, A, /),
	sub_atom(Info, 0, B, _, Alias),
	sub_atom(Info, _, A, 0, File),
	catch(trill_on_swish_config:trill_on_swish_source_alias(Alias), E,
	      (print_message(warning, E), fail)),
	Spec =.. [Alias,File],
	http_safe_file(Spec, []),
	absolute_file_name(Spec, Path,
			   [ access(read),
			     file_errors(fail)
			   ]),
	setup_call_cleanup(
	    open(Path, read, In, [encoding(utf8)]),
	    read_string(In, _, Code),
	    close(In)).

%%	serve_resource(+Request) is semidet.
%
%	Serve /swish/Resource files.

serve_resource(Request) :-
	option(path_info(Info), Request),
	trill_on_swish_resource_prefix(Prefix),
	sub_atom(Info, 0, _, _, Prefix), !,
	http_reply_file(trill_on_swish_web(Info), [], Request).

trill_on_swish_resource_prefix('css/').
trill_on_swish_resource_prefix('help/').
trill_on_swish_resource_prefix('form/').
trill_on_swish_resource_prefix('icons/').
trill_on_swish_resource_prefix('js/').
trill_on_swish_resource_prefix('bower_components/').

%%	trill_on_swish_page(+Options)//
%
%	Generate the entire SWISH default page.

trill_on_swish_page(Options) -->
	trill_on_swish_navbar(Options),
	trill_on_swish_content(Options).

%%	trill_on_swish_navbar(+Options)//
%
%	Generate the swish navigation bar.

trill_on_swish_navbar(Options) -->
	trill_on_swish_resources,
	html(nav([ class([navbar, 'navbar-default']),
		   role(navigation)
		 ],
		 [ div(class('navbar-header'),
		       [ \swish_collapsed_button,
			 \swish_logos(Options)
		       ]),
		   div([ class([collapse, 'navbar-collapse']),
			 id(navbar)
		       ],
		       [ ul([class([nav, 'navbar-nav'])], []),
			 \swish_search_form(Options)
		       ])
		 ])).

swish_collapsed_button -->
	html(button([type(button),
		     class('navbar-toggle'),
		     'data-toggle'(collapse),
		     'data-target'('#navbar')
		    ],
		    [ span(class('sr-only'), 'Toggle navigation'),
		      span(class('icon-bar'), []),
		      span(class('icon-bar'), []),
		      span(class('icon-bar'), [])
		    ])).

swish_logos(Options) -->
	swish_pengine_logo(Options),
	swish_logo(Options).

swish_pengine_logo(_Options) -->
	{ http_absolute_location(root(.), HREF, [])
	},
	html(a([href(HREF), class('pengine-logo')], &(nbsp))).
swish_logo(_Options) -->
	{ http_absolute_location(trill_on_swish('index.html'), HREF, [])
	},
	html(a([href(HREF), class('swish-logo')], &(nbsp))).

%%	search_form(+Options)//
%
%	Add search box to the navigation bar

swish_search_form(Options) -->
	html(div(class(['col-sm-3', 'col-md-3', 'pull-right']),
		 \swish_search_box(Options))).


%%	trill_on_swish_content(+Options)//
%
%	Generate the SWISH editor, Prolog output  area and query editor.
%	Options processed:
%
%	  - source(HREF)
%	  Load initial source from HREF

trill_on_swish_content(Options) -->
	trill_on_swish_resources,
	trill_on_swish_config_hash,
	html(div([id(content), class([container, trill_on_swish])],
		 [ div([class([tile, horizontal]), 'data-split'('50%')],
		       [ div(class('prolog-editor'), \swish_source(Options)),
			 div([class([tile, vertical]), 'data-split'('70%')],
			     [ div(class('prolog-runners'), []),
			       div(class('prolog-query'), \swish_query(Options))
			     ])
		       ]),
		   \swish_background(Options),
		   \swish_examples(Options)
		 ])).


%%	trill_on_swish_config_hash//
%
%	Set `window.swish.config_hash` to a  hash   that  represents the
%	current configuration. This is used by   config.js  to cache the
%	configuration in the browser's local store.

trill_on_swish_config_hash -->
	{ trill_on_swish_config_hash(Hash) },
	js_script({|javascript(Hash)||
		   window.trill_on_swish = window.trill_on_swish||{};
		   window.trill_on_swish.trill_on_swish_config_hash = Hash;
		   |}).


%%	source(+Options)//
%
%	Associate the source with the SWISH   page. The source itself is
%	stored  in  the  textarea  from  which  CodeMirror  is  created.
%	Options:
%
%	  - code(+String)
%	  Initial code of the source editor
%	  - file(+File)
%	  If present and code(String) is present, also associate the
%	  editor with the given file.  See storage.pl.

swish_source(Options) -->
	{ option(code(Spec), Options), !,
	  download_source(Spec, Source, Options),
	  (   option(file(File), Options)
	  ->  Extra = ['data-file'(File)]
	  ;   Extra = []
	  )
	},
	source_meta_data(File, Options),
	html(textarea([ class([source,prolog]),
			tos_style('display:none')
		      | Extra
		      ],
		      Source)).
swish_source(_) --> [].

%%	source_meta_data(+File, +Options)//
%
%	Dump the meta-data of the provided file into swish.meta_data.

source_meta_data(File, Options) -->
	{ nonvar(File),
	  option(meta(Meta), Options)
	}, !,
	js_script({|javascript(Meta)||
		   window.trill_on_swish = window.trill_on_swish||{};
		   window.trill_on_swish.meta_data = Meta;
		   |}).
source_meta_data(_, _) --> [].

swish_background(Options) -->
	{ option(background(Spec), Options), !,
	  download_source(Spec, Source, Options)
	},
	html(textarea([ class([source,prolog,background]),
			tos_style('display:none')
		      ],
		      Source)).
swish_background(_) --> [].


swish_examples(Options) -->
	{ option(trill_on_swish_examples(Examples), Options), !
	},
	html(textarea([ class([examples,prolog]),
			tos_style('display:none')
		      ],
		      Examples)).
swish_examples(_) --> [].


swish_query(Options) -->
	{ option(q(Query), Options)
	}, !,
	html(textarea([ class([query,prolog]),
			tos_style('display:none')
		      ],
		      Query)).
swish_query(_) --> [].


%%	download_source(+HREF, -Source, Options) is det.
%
%	Download source from a URL.  Options processed:
%
%	  - timeout(+Seconds)
%	    Max time to wait for reading the source.  Default
%	    is 10 seconds.
%	  - max_length(+Chars)
%	    Maximum lenght of the content.  Default is 1 million.
%	  - encoding(+Encoding)
%	    Encoding used to interpret the text.  Default is UTF-8.
%
%	@bug: Should try to interpret the encoding from the HTTP
%	      header.

download_source(HREF, Source, Options) :-
	uri_is_global(HREF), !,
	option(timeout(TMO), Options, 10),
	option(max_length(MaxLen), Options, 1_000_000),
	catch(call_with_time_limit(
		  TMO,
		  setup_call_cleanup(
		      http_open(HREF, In,
				[ cert_verify_hook(ssl_verify)
				]),
		      read_source(In, MaxLen, Source, Options),
		      close(In))),
	      E, load_error(E, Source)).
download_source(Source0, Source, Options) :-
	option(max_length(MaxLen), Options, 1_000_000),
	string_length(Source0, Len),
	(   Len =< MaxLen
	->  Source = Source0
	;   format(string(Source),
		   '%ERROR: Content too long (max ~D)~n', [MaxLen])
	).

read_source(In, MaxLen, Source, Options) :-
	option(encoding(Enc), Options, utf8),
	set_stream(In, encoding(Enc)),
	ReadMax is MaxLen + 1,
	read_string(In, ReadMax, Source0),
	string_length(Source0, Len),
	(   Len =< MaxLen
	->  Source = Source0
	;   format(string(Source),
		   '%ERROR: Content too long (max ~D)~n', [MaxLen])
	).

load_error(E, Source) :-
	message_to_string(E, String),
	format(string(Source), '%ERROR: ~s~n', [String]).

:- public ssl_verify/5.

%%	ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%	Currently we accept  all  certificates.   We  organise  our  own
%	security using SHA1 signatures, so  we   do  not  care about the
%	source of the data.

ssl_verify(_SSL,
	   _ProblemCertificate, _AllCertificates, _FirstCertificate,
	   _Error).


		 /*******************************
		 *	     RESOURCES		*
		 *******************************/

%%	trill_on_swish_resources//
%
%	Include  SWISH  CSS  and   JavaScript.    This   does   not  use
%	html_require//1  because  we  need  to   include  the  JS  using
%	RequireJS, which requires a non-standard script element.

trill_on_swish_resources -->
	trill_on_swish_css,
	trill_on_swish_js.

trill_on_swish_js  --> html_post(head, \include_trill_on_swish_js).
trill_on_swish_css --> html_post(head, \include_trill_on_swish_css).

include_trill_on_swish_js -->
	{ trill_on_swish_resource(js, JS),
	  trill_on_swish_resource(rjs, RJS),
	  http_absolute_location(trill_on_swish(js/JS), SwishJS, []),
	  http_absolute_location(trill_on_swish(RJS),   SwishRJS, [])
	},
	rjs_timeout(JS),
	html(script([ src(SwishRJS),
		      'data-main'(SwishJS)
		    ], [])).

rjs_timeout('trill_on_swish-min') --> !,
	js_script({|javascript||
// Override RequireJS timeout, until main file is loaded.
window.require = { waitSeconds: 0 };
		  |}).
rjs_timeout(_) --> [].


include_trill_on_swish_css -->
	{ trill_on_swish_resource(css, CSS),
	  http_absolute_location(trill_on_swish(css/CSS), SwishCSS, [])
	},
	html(link([ rel(stylesheet),
		    href(SwishCSS)
		  ])).

trill_on_swish_resource(Type, ID) :-
	trill_on_swish_alt(Type, ID, File),
	(   File == (-)
	;   absolute_file_name(File, _P, [file_errors(fail), access(read)])
	), !.

trill_on_swish_alt(js,  'trill_on_swish-min',     trill_on_swish_web('js/trill_on_swish-min.js')) :-
	\+ debugging(nominified).
trill_on_swish_alt(js,  'trill_on_swish',         trill_on_swish_web('js/trill_on_swish.js')).
trill_on_swish_alt(css, 'trill_on_swish-min.css', trill_on_swish_web('css/trill_on_swish-min.css')) :-
	\+ debugging(nominified).
trill_on_swish_alt(css, 'trill_on_swish.css',     trill_on_swish_web('css/trill_on_swish.css')).
trill_on_swish_alt(rjs, 'js/tos_require.js', trill_on_swish_web('js/tos_require.js')) :-
	\+ debugging(nominified).
trill_on_swish_alt(rjs, 'bower_components/requirejs/require.js', -).

