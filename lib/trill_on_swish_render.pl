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

:- module(trill_on_swish_render,
	  [ trill_on_swish_use_rendering/1,		% +Renderer
	    trill_on_swish_use_rendering/2,		% +Renderer, +Options

	    trill_on_swish_register_renderer/2,	% Declare a rendering module
	    trill_on_swish_current_renderer/2		% Name, Comment
	  ]).
:- use_module(library(pengines_io), []).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(option)).
:- use_module(library(error)).

:- meta_predicate
	trill_on_swish_register_renderer(:, +),
	trill_on_swish_use_rendering(:),
	trill_on_swish_use_rendering(:, +).

/** <module> SWISH term-rendering support

This module manages rendering answers  using alternative vizualizations.
The idea is that a  specific  context   _uses_  zero  or  more rendering
modules.  These  rendering  modules   provide    an   alternative   HTML
representation for the target term. If  multiple possible renderings are
found,  a  =|<div  class="render-multi">|=  element  is  generated  that
contains the alternative renderings. The   jQuery  plugin =renderMulti=,
defined in =answer.js= adds the  behaviour   to  change rendering to the
generated div.

The user can import rendering schemes into the current context using the
directive below. `Spec` is either an atom   or string, making the system
look for render(Spec), or it is a   (single) file specification that can
be used for use_module/1.

  ==
  :- trill_on_swish_use_rendering(Spec).
  ==

A rendering module is a  Prolog   module  that  defines the non-terminal
term_rendering//3,  which  will  be  called  as  below.  `Term`  is  the
(non-var) term that must be rendered, `Vars` is a list of variable names
bound to this term and `Options` is a   list of write options that would
normally  be  passed  to  write_term/3.  The   grammar  is  executed  by
library(http/html_write) and must  generate   compatible  tokens  (which
means it must call html//1 to generate HTML tokens).

  ==
  phrase(Renderer:term_rendering(Term, Vars, Options), Tokens)
  ==
*/

:- multifile user:file_search_path/2.

user:file_search_path(tos_render, trill_on_swish('lib/render')).


%%	trill_on_swish_use_rendering(+FileOrID)
%
%	Register an answer  renderer.   Same  as trill_on_swish_use_rendering(FileOrID,
%	[]).
%
%	@see trill_on_swish_use_rendering/2.

:- multifile user:term_expansion/2.

trill_on_swish_use_rendering(Rendering) :-
	trill_on_swish_use_rendering(Rendering, []).

%%	trill_on_swish_use_rendering(:ID, +Options)
%
%	Register an answer renderer  with   options.  Options are merged
%	with   write-options   and   passed     to    the   non-terminal
%	term_rendering//3 defined in the rendering module.

trill_on_swish_use_rendering(Rendering, Options) :-
	Rendering = Into:Renderer,
	must_be(atom, Renderer),
	(   trill_on_swish_renderer(Renderer, _, _)
	->  true
	;   existence_error(renderer, Renderer)
	),
	retractall(Into:'trill_on_swish renderer'(Renderer, _)),
	assertz(Into:'trill_on_swish renderer'(Renderer, Options)).

user:term_expansion((:- trill_on_swish_use_rendering(Renderer)), Expanded) :-
	expand_rendering(Renderer, [], Expanded).
user:term_expansion((:- trill_on_swish_use_rendering(Renderer, Options)), Expanded) :-
	expand_rendering(Renderer, Options, Expanded).

expand_rendering(Module:Renderer, Options,
		 Module:'trill_on_swish renderer'(Renderer, Options)) :- !,
	must_be(atom, Module),
	must_be(atom, Renderer).
expand_rendering(Renderer, Options,
		 'trill_on_swish renderer'(Renderer, Options)) :-
	must_be(atom, Renderer).

%%	pengines_io:binding_term(+Term, +Vars, +Options) is semidet.
%
%	Produce alternative renderings for Term, which  is a binding for
%	Vars.

:- multifile pengines_io:binding_term//3.

pengines_io:binding_term(Term, Vars, Options) -->
	{ option(module(Module), Options),
	  findall(Tokens,
		  call_term_rendering(Module, Term, Vars, Options, Tokens),
		  NestedTokens),
	  NestedTokens \== [], !
	},
	alt_renderer(NestedTokens, Term, Options).

%%	call_term_rendering(+Module, +Term, +Vars, +Options, -Tokens) is nondet.
%
%	Call  term_rendering//3  in  all  modules    from  which  Module
%	inherits.

call_term_rendering(Module, Term, Vars, Options, Tokens) :-
	State = state([]),
	default_module(Module, Target),
	current_predicate(Target:'trill_on_swish renderer'/2),
	Target:'trill_on_swish renderer'(Name, RenderOptions),
	atom(Name),
	is_new(State, Name),
	trill_on_swish_renderer(Name, RenderModule, _Comment),
	merge_options(RenderOptions, Options, AllOptions),
	phrase(RenderModule:term_rendering(Term, Vars, AllOptions), Tokens).

%%	is_new(!State, +M) is semidet.
%
%	Only succeeds once for each new ground value M.

is_new(State, M) :-
	arg(1, State, Seen),
	(   memberchk(M, Seen)
	->  fail
	;   nb_linkarg(1, State, [M|Seen])
	).

%%	alt_renderer(+Specialised, +Term, +Options)//
%
%	Create a rendering selection object after we have found at least
%	one alternative rendering for Term.

alt_renderer(Specialised, Term, Options) -->
	html(div(class('render-multi'),
		 \specialised(Specialised, Term, Options))).

specialised([], Term, Options) -->
	html(span('data-render'('Prolog term'), \term(Term, Options))).
specialised([H|T], Term, Options) -->
	tokens(H),
	specialised(T, Term, Options).

tokens([]) --> [].
tokens([H|T]) --> [H], tokens(T).


		 /*******************************
		 *	   REGISTRATION		*
		 *******************************/

:- multifile
	trill_on_swish_renderer/3.

%%	trill_on_swish_current_renderer(Name, Comment) is nondet.
%
%	True when renderer Name is declared with Comment.

trill_on_swish_current_renderer(Name, Comment) :-
	trill_on_swish_renderer(Name, _Module, Comment).

%%	trill_on_swish_register_renderer(:Name, +Comment)
%
%	Register a module as SWISH rendering component.

trill_on_swish_register_renderer(Name, Comment) :-
	throw(error(context_error(nodirective, trill_on_swish_register_renderer(Name, Comment)),
		    _)).

user:term_expansion((:- trill_on_swish_register_renderer(Name, Comment)),
		    trill_on_swish_render:trill_on_swish_renderer(Name, Module, Comment)) :-
	prolog_load_context(module, Module).
