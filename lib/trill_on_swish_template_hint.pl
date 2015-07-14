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

:- module(trill_on_swish_template_hint,
	  [ visible_predicate/3,	% ?PI, +Module, +Options
	    predicate_template/2,	% +PI, -TemplateDict
	    visible_predicate_templates/3 %  +Module, +Options, -Templates
	  ]).
:- use_module(library(apply)).
:- use_module(library(pldoc), []).
:- use_module(library(pldoc/doc_man)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_modes)).
:- use_module(library(http/html_write)).
:- use_module(library(memfile)).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(xpath)).
:- use_module(library(sandbox)).
:- use_module(library(option)).
:- use_module(library(filesex)).
:- use_module(library(error)).

:- use_module(trill_on_swish_render).

/** <module> Generate template hints for CondeMirror

Provide templates for the Prolog   template-hinting functionality of the
SWISH editor.

@tbd	For which predicates should we generate templates?  Should we
	provide templates on demand?
@tbd	What about safely?
@tbd	Dedicated template for the rendering support?
*/

%%	visible_predicate_templates(+Module, +Options, -Templates) is det.
%
%	True when Templates is a JSON dict holding autocompletion
%	templates for Module.

:- dynamic
	cached_templates/3.		% Module, Options, Templates

visible_predicate_templates(Module, Templates, Options) :-
	cached_templates(Module, Options, Templates), !.
visible_predicate_templates(Module, Templates, Options) :-
	with_mutex(trill_on_swish_template_hint,
		   visible_predicate_templates_sync(Module, Templates, Options)).

visible_predicate_templates_sync(Module, Templates, Options) :-
	cached_templates(Module, Options, Templates), !.
visible_predicate_templates_sync(Module, Templates, Options) :-
	findall(Templ,
		(   visible_predicate(PI, Module, Options),
		    predicate_template(PI, Templ)
		),
		Templates0),
	assertz(cached_templates(Module, Options, Templates0)),
	Templates0 = Templates.

clean_template_cache :-
	retractall(cached_templates(_,_,_)).

:- initialization clean_template_cache.

%%	visible_predicate(?PI, +Module, +Options) is nondet.
%
%	True when PI is a plain predicate indicator for a predicate that
%	can be called in Module.  Additional options:
%
%	  - safe(+Boolean)
%	  If =true=, filter out unsafe predicates.
%	  - autoload(+Boolean)
%	  Load autoloadable predicates.

visible_predicate(PI, Module, Options) :-
	option(from(FromList), Options), !,
	must_be(list, FromList),
	member(From, FromList),
	must_be(ground, From),
	visible_from(From, PI, Module, Options),
	\+ no_template(PI).
visible_predicate(PI, Module, Options) :-
	PI = Name/Arity,
	predicate_property(Module:Head, visible),
	autoload(Module:Head, Options),
	safe(Module:Head, Options),
	functor(Head, Name, Arity),
	\+ no_template(PI).

no_template(use_module/1).
no_template(use_module/2).
no_template(trill_on_swish_use_rendering/1).
no_template(trill_on_swish_use_rendering/2).

%%	visible_from(+Spec, -PI, +Module, +Options) is nondet.
%
%	Provide  complates  from  a    selected  specification.  Defined
%	specifications are =built_in= or the   specification  of a file,
%	e.g., library(lists).

visible_from(built_in, Name/Arity, _Module, Options) :- !,
	predicate_property(system:Head, built_in),
	functor(Head, Name, Arity),
	\+ sub_atom(Name, 0, _, _, $),
	safe(system:Head, Options).
visible_from(Spec, Name/Arity, _Module, _Options) :-
	compound(Spec),
	functor(Spec, _, 1),
	exists_source(Spec),
	xref_public_list(Spec, -,
			 [ exports(Exports)
			 ]),
	member(Name/Arity, Exports).


autoload(Pred, Options) :-
	option(autoload(false), Options, false), !,
	Pred = M:Head,
	functor(Head, Name, Arity),
	(   current_predicate(M:Name/Arity)
	->  \+ ( predicate_property(M:Head, imported_from(LoadModule)),
		 no_autocomplete_module(LoadModule)
	       )
	;   '$find_library'(M, Name, Arity, LoadModule, _Library),
	    \+ no_autocomplete_module(LoadModule),
	    current_predicate(LoadModule:Name/Arity)
	).
autoload(_, _).

no_autocomplete_module(pce).
no_autocomplete_module(pce_principal).
no_autocomplete_module(pce_class_template).
no_autocomplete_module(pce_dispatch).
no_autocomplete_module(pce_expansion).
no_autocomplete_module(pce_error).
no_autocomplete_module(pce_compatibility_layer).
no_autocomplete_module(backward_compatibility).
no_autocomplete_module(settings).
no_autocomplete_module(quintus).
no_autocomplete_module(toplevel_variables).
no_autocomplete_module('$qlf').
no_autocomplete_module(pldoc).
no_autocomplete_module(quasi_quotations).
no_autocomplete_module(ssl).
no_autocomplete_module(oset).
no_autocomplete_module(prolog_colour).
no_autocomplete_module(pengines_io).
no_autocomplete_module(broadcast).
no_autocomplete_module(sgml).
no_autocomplete_module(swi_system_utilities).
no_autocomplete_module(prolog_metainference).
no_autocomplete_module(thread_pool).

%%	safe(+Goal, +Options) is semidet.
%
%	True if Goal is sometimes safe.   Note  that meta-predicates are
%	never immediately safe.

safe(Goal, Options) :-
	option(safe(true), Options, true), !,
	(   predicate_property(Goal, meta_predicate(_))
	->  true
	;   catch(safe_goal(Goal), _, fail)
	).
safe(_, _).

%%	predicate_template(:PI, -Template:json) is semidet.
%
%	@arg Template is a dict holding the   keys below. Only =mode= is
%	guaranteed to be present.
%
%		- mode
%		String holding the mode-line.  Always present.
%		- summary
%		Summary description.
%		- iso
%		=true= if the predicate is an ISO predicate.
%		- determinism
%		Determinism indicator (if known)
%	@tbd	Deal with locally redefined predicates, etc.

predicate_template(PI, Dict) :-
	findall(Pair, predicate_info(PI, Pair), Pairs),
	Pairs \== [],
	dict_pairs(Dict, json, Pairs).

predicate_info(PI, Pair) :-
	(   man_predicate_info(PI, Pair)
	*-> true
	;   pldoc_predicate_info(PI, Pair)
	).

%%	man_predicate_info(+PI, -Pair) is nondet.
%
%	Extract the mode line from the SWI-Prolog manual.

man_predicate_info(PI, Name-Value) :-
	pi_head(PI, Head),
	strip_module(Head, _, PHead),
	functor(PHead, PName, Arity),
	phrase(man_page(PName/Arity,
			[ no_manual(fail),
			  links(false),
			  navtree(false)
			]), HTML),
	setup_call_cleanup(
	    new_memory_file(MF),
	    ( setup_call_cleanup(
		  open_memory_file(MF, write, Out),
		  print_html(Out, HTML),
		  close(Out)),
	      setup_call_cleanup(
		  open_memory_file(MF, read, In),
		  load_html(stream(In), DOM, [syntax_errors(quiet)]),
		  close(In))
	    ),
	    free_memory_file(MF)),
	xpath_chk(DOM, //dt(@class=pubdef), DT),
	xpath_chk(DT, a(text), ModeLine0),
	normalize_space(string(ModeLine), ModeLine0),
	(   atom_string(PName, PString),
	    Name-Value = name-PString
	;   Name-Value = arity-Arity
	;   Name-Value = mode-ModeLine
	;   once(catch(predicate(PName, Arity, Summary, _, _), _, fail)),
	    Name-Value = summary-Summary
	;   predicate_property(system:PHead, iso),
	    Name-Value = iso-true
	;   predicate_property(system:PHead, built_in),
	    Name-Value = type-built_in
	).

%%	pldoc_predicate_info(+PI, -ModeLine) is semidet.

pldoc_predicate_info(PI, Name-Value) :-
	pi_head(PI, Head),
	strip_module(Head, _, PHead),
	functor(PHead, PName, Arity),
	implementation_module(Head, Module),
	doc_comment(PI, Pos, Summary, Comment), !,
	is_structured_comment(Comment, Prefixes),
	string_codes(Comment, CommentCodes),
	indented_lines(CommentCodes, Prefixes, Lines),
	process_modes(Lines, Module, Pos, Modes, _VarNames, _RestLines),
	member(mode(Mode,Vars), Modes),
	mode_head_det(Mode, ModeHead, Det),
	m_same_name_arity(ModeHead, Head),
	maplist(bind_var, Vars),
	term_string(ModeHead, ModeLine,
		    [ quoted(true),
		      module(pldoc_modes),
		      numbervars(true),
		      spacing(next_argument)
		    ]),
	(   atom_string(PName, PString),
	    Name-Value = name-PString
	;   Name-Value = arity-Arity
	;   Name-Value = mode-ModeLine
	;   Name-Value = summary-Summary
	;   Det \== unknown,
	    Name-Value = determinism-Det
	).


bind_var(Name=Var) :- Var = '$VAR'(Name).

mode_head_det(Head is Det, Head, Det) :- !.
mode_head_det(Head, Head, unknown).

pi_head(Var, _) :-
	var(Var), !, instantiation_error(Var).
pi_head(M0:T0, M:T) :- !,
	strip_module(M0:T0, M, T1),
	pi_head(T1, T).
pi_head(Name/Arity, Head) :- !,
	functor(Head, Name, Arity).
pi_head(Name//DCGArity, Head) :-
	Arity is DCGArity+2,
	functor(Head, Name, Arity).

implementation_module(Head, M) :-
	predicate_property(Head, imported_from(M0)), !,
	M = M0.
implementation_module(Head, M) :-
	strip_module(user:Head, M, _).

m_same_name_arity(H1, H2) :-
	strip_module(H1, _, P1),
	strip_module(H2, _, P2),
	functor(P1, Name, Arity),
	functor(P2, Name, Arity).


		 /*******************************
		 *	     RENDERING		*
		 *******************************/

%%	rendering_template(-Template)
%
%	Create a template for the SWISH rendering modules.

rendering_template([ /*json{displayText:  "trill_on_swish_use_rendering(+Renderer).",
			  type:         "directive",
			  template:     "trill_on_swish_use_rendering(${Renderer}).",
			  varTemplates: json{'Renderer': Template}},
		     json{displayText:  "trill_on_swish_use_rendering(+Renderer, +Options).",
			  type:         "directive",
			  template:     "trill_on_swish_use_rendering(${Renderer}).",
			  varTemplates: json{'Renderer': Template}},*/
		     json{displayText:  "prob_instanceOf(+Class, +Individual, -Prob).",
 			  type:         "directive",
			  template:     "prob_instanceOf(${Class},${Individual},${Prob}).",
 			  varTemplates: json{'Renderer': Template}},
			  
		     json{displayText:  "prob_sub_class(+Class1, +Class2, -Prob).",
 			  type:         "directive",
			  template:     "prob_sub_class(${Class1},${Class2},${Prob}).",
			  varTemplates: json{'Renderer': Template}},

		     json{displayText:  "prob_unsat(+ClassExpression, -Prob).",
			  type:         "directive",
			  template:     "prob_unsat(${ClassExpression},${Prob}).",
			  varTemplates: json{'Renderer': Template}},
		 
		     json{displayText:  "prob_inconsistent_theory(-Prob).",
			  type:         "directive",
			  template:     "prob_inconsistent_theory(${Prob}).",
			  varTemplates: json{'Renderer': Template}},
		
		     json{displayText:  "instanceOf(+Class, +Individual, -Expl).",
 			  type:         "directive",
			  template:     "instanceOf(${Class},${Individual},${Expl}).",
 			  varTemplates: json{'Renderer': Template}},
			  
		     json{displayText:  "sub_class(+Class1, +Class2, -Expl).",
 			  type:         "directive",
			  template:     "sub_class(${Class1},${Class2},${Expl}).",
			  varTemplates: json{'Renderer': Template}},

		     json{displayText:  "unsat(+ClassExpression, -Expl).",
			  type:         "directive",
			  template:     "unsat(${ClassExpression},${Expl}).",
			  varTemplates: json{'Renderer': Template}},
			  
		     json{displayText:  "inconsistent_theory(-Expl).",
			  type:         "directive",
			  template:     "inconsistent_theory(${Expl}).",
			  varTemplates: json{'Renderer': Template}},
		     
		     json{displayText:  "instanceOf(+Class, +Individual).",
 			  type:         "directive",
			  template:     "instanceOf(${Class},${Individual}).",
 			  varTemplates: json{'Renderer': Template}},
			  
		     json{displayText:  "sub_class(+Class1, +Class2).",
 			  type:         "directive",
			  template:     "sub_class(${Class1},${Class2}).",
			  varTemplates: json{'Renderer': Template}},

		     json{displayText:  "unsat(+ClassExpression).",
			  type:         "directive",
			  template:     "unsat(${ClassExpression}).",
			  varTemplates: json{'Renderer': Template}},
			  
		     json{displayText:  "inconsistent_theory.",
			  type:         "directive",
			  template:     "inconsistent_theory.",
			  varTemplates: json{'Renderer': Template}}	
		   ]) :-
	findall(json{displayText: Comment,
		     text: Name},
		trill_on_swish_current_renderer(Name, Comment),
		Template).


		 /*******************************
		 *	      LIBRARY		*
		 *******************************/

%%	library_template(-Template, +Options) is det.
%
%	Produce a template for  selecting   libraries.  By default, this
%	enumerates all Prolog files under the   file alias =library=. If
%	Options includes from(FromList), this is  interpreted similar to
%	visible_predicate/3.

library_template(json{displayText:  "use_module(library(...))",
		      type:         "directive",
		      template:     "use_module(library(${Library})).",
		      varTemplates: json{'Library': Template}}, Options) :-
	(   option(from(From), Options)
	->  library_template_from(From, Template)
	;   library_template(library, '.', Template)
	).


:- dynamic
	library_template_cache/3.

library_template(Alias, SubDir, Values) :-
	library_template_cache(Alias, SubDir, Values), !.
library_template(Alias, SubDir, Values) :-
	with_mutex(trill_on_swish_template_hint,
		   (   library_template_cache(Alias, SubDir, Values)
		   ->  true
		   ;   library_template_no_cache(Alias, SubDir, Values),
		       asserta(library_template_cache(Alias, SubDir, Values))
		   )).

library_template_no_cache(Alias, SubDir, Values) :-
	library_files(Alias, SubDir, Files, Dirs),
	maplist(library_sub_template(Alias, SubDir), Dirs, DirTemplates),
	maplist(plain_file, Files, PlainFiles),
	flatten([DirTemplates, PlainFiles], Values).

library_sub_template(Alias, Dir0, Dir,
		     json{displayText: DirSlash,
			  template: DirTemplate,
			  varTemplates: VarTemplates
			 }) :-
	directory_file_path(Dir0, Dir, Dir1),
	library_template(Alias, Dir1, Templates),
	Templates \== [], !,
	string_concat(Dir, "/", DirSlash),
	string_upper(Dir, UDir),
	atom_concat(UDir, lib, TemplateVar),
	format(string(DirTemplate), "~w/${~w}", [Dir, TemplateVar]),
	VarTemplates = json{}.put(TemplateVar, Templates).
library_sub_template(_,_,_,[]).

plain_file(File, Plain) :-
	file_name_extension(Plain, _Ext, File).

%%	library_files(+Alias, +SubDir, -Files, -Dirs)
%
%	True when Files is a  list  of   files  that  can be loaded from
%	Alias(SubDir) and Dirs is a list of sub directories of Files.

library_files(Alias, SubDir, Files, Dirs) :-
	findall(Type-Name, directory_entry(Alias, SubDir, Type, Name), Pairs),
	keysort(Pairs, Sorted),
	group_pairs_by_key(Sorted, Grouped),
	group(directory, Grouped, Dirs),
	group(prolog, Grouped, Files).

group(Key, Grouped, List) :-
	(   memberchk(Key-List0, Grouped)
	->  sort(List0, List)
	;   List = []
	).

directory_entry(Alias, SubDir, Type, Name) :-
	Spec =.. [Alias, SubDir],
	absolute_file_name(Spec, Dir,
			   [ file_type(directory),
			     file_errors(fail),
			     solutions(all),
			     access(exist)
			   ]),
	directory_files(Dir, All),
	member(Name, All),
	\+ sub_atom(Name, 0, _, _, '.'),
	directory_file_path(Dir, Name, Path),
	file_type(Path, Name, Type).

file_type(_, 'INDEX.pl', _) :- !,
	fail.
file_type(Path, _, Type) :-
	exists_directory(Path), !,
	Type = directory.
file_type(_, Name, Type) :-
	file_name_extension(_, Ext, Name),
	prolog_file_type(Ext, prolog),
	\+ prolog_file_type(Ext, qlf),
	Type = prolog.

%%	library_template_from(+From:list, -Template) is det.
%
%	As library_template/1, but build  the   completion  list  from a
%	given set of libraries.

library_template_from(From, Template) :-
	libs_from(From, Libs),
	lib_template_from(Libs, Template).

lib_template_from(Libs, Template) :-
	dirs_plain(Libs, Dirs, Plain),
	keysort(Dirs, Sorted),
	group_pairs_by_key(Sorted, Grouped),
	maplist(library_sub_template_from, Grouped, DirTemplates),
	flatten([DirTemplates, Plain], Template).

dirs_plain([], [], []).
dirs_plain([[Plain]|T0], Dirs, [Plain|T]) :- !,
	dirs_plain(T0, Dirs, T).
dirs_plain([[Dir|Sub]|T0], [Dir-Sub|T], Plain) :-
	dirs_plain(T0, T, Plain).

libs_from([], []).
libs_from([library(Lib)|T0], [Segments|T]) :- !,
	phrase(segments(Lib), Segments),
	libs_from(T0, T).
libs_from([_|T0], T) :-
	libs_from(T0, T).

segments(A/B) --> !, segments(A), segments(B).
segments(A)   --> [A].

segments_to_slash([One], One).
segments_to_slash(List, Term/Last) :-
	append(Prefix, [Last], List), !,
	segments_to_slash(Prefix, Term).


library_sub_template_from(Dir-Members,
			  json{displayText: DirSlash,
			       template: DirTemplate,
			       varTemplates: VarTemplates
			      }) :-
	lib_template_from(Members, Templates),
	string_concat(Dir, "/", DirSlash),
	string_upper(Dir, UDir),
	atom_concat(UDir, lib, TemplateVar),
	format(string(DirTemplate), "~w/${~w}", [Dir, TemplateVar]),
	VarTemplates = json{}.put(TemplateVar, Templates).


%%	imported_library(+Module, -Library) is nondet.
%
%	True when Library is imported into Module.

imported_library(Module, Library) :-
	setof(FromModule, imported_from(Module, FromModule), FromModules),
	member(FromModule, FromModules),
	module_property(FromModule, file(File)),
	file_name_on_path(File, Library),
	Library = library(_).

imported_from(Module, FromModule) :-
	current_predicate(Module:Name/Arity),
	functor(Head, Name, Arity),
	predicate_property(Module:Head, imported_from(FromModule)).


		 /*******************************
		 *       COLLECT TEMPLATES	*
		 *******************************/

swish_templates(Template) :-
	setof(From, visible_lib(trill_on_swish, From), FromList),
	swish_templates(Template, [from(FromList)]).

%swish_templates(Template, Options) :-
%	library_template(Template, Options).
swish_templates(Template, _Options) :-
	rendering_template(Template).
%swish_templates(Templates, Options) :-
%	visible_predicate_templates(trill_on_swish, Templates, Options).

%%	visible_lib(+Module, -Lib) is nondet.
%
%	Enumerate modules imported into  Module   and  generally  useful
%	modules.

visible_lib(Module, library(Path)) :-
	imported_library(Module, Lib),
	Lib = library(Name),
	\+ no_autocomplete_module(Name),
	atomic_list_concat(Segments, /, Name),
	segments_to_slash(Segments, Path).
visible_lib(_, Lib) :-
	visible_lib(Lib).

visible_lib(built_in).
visible_lib(library(apply)).
visible_lib(library(aggregate)).
visible_lib(library(assoc)).
visible_lib(library(base32)).
visible_lib(library(base64)).
visible_lib(library(charsio)).
visible_lib(library(clpb)).
visible_lib(library(clpfd)).
visible_lib(library(codesio)).
visible_lib(library(coinduction)).
visible_lib(library(date)).
visible_lib(library(debug)).
visible_lib(library(error)).
visible_lib(library(dif)).
visible_lib(library(gensym)).
visible_lib(library(heaps)).
visible_lib(library(lists)).
visible_lib(library(occurs)).
visible_lib(library(option)).
visible_lib(library(ordsets)).
visible_lib(library(pairs)).
visible_lib(library(random)).
visible_lib(library(rbtrees)).
visible_lib(library(statistics)).
visible_lib(library(sort)).
visible_lib(library(terms)).
visible_lib(library(ugraph)).
visible_lib(library(utf8)).
visible_lib(library(varnumbers)).
visible_lib(library(when)).

%visible_lib(library(semweb/rdf_db)).
%visible_lib(library(semweb/rdfs)).


		 /*******************************
		 *	    SWISH CONFIG	*
		 *******************************/

%%	trill_on_swish_config:tos_config(-Name, -Styles) is det.
%
%	Provides the object `config.swish.templates`, a JSON object that
%	provides the templates for hinting in CodeMirror.

trill_on_swish_config:tos_config(templates, Templates) :-
	findall(Templ, swish_templates(Templ), Templates0),
	flatten(Templates0, Templates).
