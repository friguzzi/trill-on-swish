/**
 * @fileOverview
 * Run an manage Prolog queries and their output
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 * @requires laconic
 * @requires editor
 */

define([ "jquery", "config", "tos_cm/lib/codemirror", "answer", "laconic" ],
       function($, config, CodeMirror) {

		 /*******************************
		 *	  THE COLLECTION	*
		 *******************************/

(function($) {
  var pluginName = 'prologRunners';

  /** @lends $.fn.prologRunners */
  var methods = {
    /**
     * Initialize the container for Prolog queries.
     * @example $(".prolog-runners").prologRunners();
     * @param {Object} [options] currently ignored
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};

	function runnerMenu() {
	  var icon = $.el.span();
	  $(icon).html("&#9776");
	  var menu = dropdownButton(
	    icon,
	    { divClass:"runners-menu",
	      ulClass:"pull-right",
	      client:elem,
	      actions:
	      { "Collapse all": function() {
		  this.find(".prolog-runner").prologRunner('toggleIconic', true);
	        },
		"Expand all": function() {
		  this.find(".prolog-runner").prologRunner('toggleIconic', false);
		},
		"Stop all": function() {
		  this.find(".prolog-runner").prologRunner('stop');
		},
		"Clear": function() { this.prologRunners('clear'); }
	      }
	    });

	  return menu;
	}

	data.stretch = $($.el.div({class:"stretch"}));
	data.inner   = $($.el.div({class:"inner"}));

	elem.append(runnerMenu());
	elem.append(data.stretch);
	elem.append(data.inner);

	elem.on("pane.resize", function() {
	  elem.prologRunners('scrollToBottom', true);
	});

	elem.data(pluginName, data);
      });
    },

    /**
     * Run a Prolog query.  The methods appends a `<div>` and runs the
     * plugin `prologRunner` on the new div.
     * @param {Object} query
     * @param {String} query.query the Prolog query to prove
     * @param {String} [query.source] the Prolog program
     * @param {Boolean} [query.iconifyLast=true] define whether or not
     * to iconify the previous runner.
     * @param {Boolean} [query.tabled=false] if `true`, make a table with
     * the results.
     */
    run: function(query) {
      var data = this.data('prologRunners');

      if ( query.iconifyLast )
	this.prologRunners('iconifyLast');

      var runner = $.el.div({class: "prolog-runner"});

      data.inner.append(runner);
      $(runner).prologRunner(query);
      this.prologRunners('scrollToBottom');

      return this;
    },

    /**
     * Destroy all runners and, if applicable, their associated
     * pengines.
     */
    clear: function() {
      this.find(".prolog-runner").prologRunner('close');
    },

    /**
     * Iconify the last runner if it is not associated to an open
     * query.
     */
    iconifyLast: function() {
      var jrunner = $(this.inner).children().last();

      if ( jrunner.length == 1 )
      { var runner = jrunner.prologRunner();

	if ( !runner.alive() )
	  runner.toggleIconic(true);
      }

      return this;
    },

    /**
     * Keep the content at the bottom of the window, such that the
     * buttons remain in the same position.  The only way to achieve
     * this is by putting something on top of the content as long as
     * the content is lower than the window.
     *
     * @param {Boolean} [onlydown=false] only scroll down if we are
     * not at the bottom.
     */
    // the "- 4" compensates for the prolog-runner top&bottom margin.
    scrollToBottom: function(onlydown) {
      this.each(function() {
	var elem = $(this);
	var data   = elem.data('prologRunners');
	var height = data.inner.height();
	var room   = elem.height() - height - 4 - 2;

	if ( room > 0 || onlydown !== true ) {
	  data.stretch.height(room > 0 ? room : 0);
	  elem.scrollTop(height);
	}
      });

      return this;
    }
  }; // methods

  /**
   * Manage a subwindow (`<div>`) that acts as a collection of runner
   * items.  Each runner represents a Prolog query, either active or
   * terminated.  The collection keeps the runners properly stacked and
   * provides a menu to control the collection, such as _clear_,
   * _iconify all_, etc.
   *
   * @class prologRunners
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */
  $.fn.prologRunners = function(method) {
    if ( methods[method] ) {
      return methods[method]
	.apply(this, Array.prototype.slice.call(arguments, 1));
    } else if ( typeof method === 'object' || !method ) {
      return methods._init.apply(this, arguments);
    } else {
      $.error('Method ' + method + ' does not exist on jQuery.' + pluginName);
    }
  };
}(jQuery));


		 /*******************************
		 *	INDIVIDUAL RUNNER	*
		 *******************************/

(function($) {
  var pluginName = 'prologRunner';

  // keyBindings rely on the jQuery normalized `which` field
  var keyBindings = { 59:      'next',		/* ; (FF) */
		      186:     'next',		/* ; (Chromium) */
		      32:      'next',		/* space */
		      190:     'stop',		/* . */
		      13:      'stop',		/* Enter */
		      65:      'stopOrAbort',	/* a */
		      27:      'stopOrAbort',	/* Esc */
		      46:      'close',		/* Del */
		      112:     'trill_on_swish_help'		/* F1 */
                    };

  /** @lends $.fn.prologRunner */
  var methods = {
    /**
     * Initialize a runner for a Prolog query
     * @param {Object} query
     * @param {String} query.query the Prolog query to prove
     * @param {String} [query.source] the Prolog program
     * @param {Boolean} [query.tabled=false]  If `true`, represent the
     * results as a table.
     */
    _init: function(query) {
      return this.each(function() {
	var elem = $(this);
	var data = {};

	function closeButton() {
	  var btn = $.el.button();
	  $(btn).html('&times');

	  $(btn).on("click", function() { elem.prologRunner('close'); });
	  return btn;
	}

	function iconizeButton() {
	  var btn = $.el.button("_");
	  $(btn).on("click", function() { elem.prologRunner('toggleIconic'); });
	  return btn;
	}

	function stateButton() {
	  var icon = $.el.span({class:"runner-state show-state idle"});

	  return dropdownButton(icon);
	}

	function controllerDiv() {
	  function next()     { elem.prologRunner('next',    1); }
	  function next10()   { elem.prologRunner('next',   10); }
	  function next100()  { elem.prologRunner('next',  100); }
	  function next1000() { elem.prologRunner('next', 1000); }
	  function stop()     { data.prolog.stop(); }
	  function abort()    { data.prolog.abort(); }

	  function button(action, label) {
	    var btn = $.el.button(label);
	    $(btn).on("click", action);
	    return btn;
	  }

	  function input() {
	    var inp = $.el.input({class:"prolog-input"});
	    var btn = $.el.button("Send");

	    $(inp).keypress(function(ev) {
			      var s;
			      if ( ev.which == 13 &&
				   (s=termNoFullStop($(inp).val())) != "" ) {
				$(inp).val("");
				ev.preventDefault();
				elem.prologRunner('respond', s);
				return false;		/* prevent bubbling */
			      } else if ( ev.key != "Esc" ) {
				ev.stopPropagation();   /* prevent bubbling */
			      }
			    });
	    $(btn).on("click", function() {
				 var s;
				 if ( (s=termNoFullStop($(inp).val())) != "" ) {
				   elem.prologRunner('respond', s);
				 }
			       });

	    return {input:inp, button:btn};
	  }

	  var inp = input();
	  var div = $.el.div({class:"controller show-state"},
			     $.el.div({class:"running"},
				      button(abort, "Abort")),
			     $.el.div({class:"wait-next"},
				      button(next, "Next"),
				      button(next10, "10"),
				      button(next100, "100"),
				      button(next1000, "1,000"), " ",
				      button(stop, "Stop")),
			     $.el.div({class:"wait-input"},
				      button(abort, "Abort"), inp.button,
				      $.el.span(inp.input)));

	  return div;
	}

	elem.addClass("prolog-runner");
	var qspan = $.el.span({class:"query tos_cm-s-prolog"});
	CodeMirror.runMode(query.query, "prolog", qspan);
	elem.append($.el.div(
	  {class:"runner-title ui-widget-header"},
	  closeButton(),
	  iconizeButton(),
	  stateButton(),
          qspan));
	elem.append($.el.div(
	  {class:"runner-results"}));
	elem.append(controllerDiv());

	elem.data('prologRunner', data);

	elem.prologRunner('populateActionMenu');
	elem.keydown(function(ev) {
	  if ( elem.prologRunner('getState') != "wait-input" ) {
	    if ( keyBindings[ev.which] ) {
	      ev.preventDefault();
	      elem.prologRunner(keyBindings[ev.which]);
	    }
	  }
	});

	data.savedFocus = document.activeElement;
	elem.attr('tabindex', -1);
	elem.focus();

	data.query   = query;
	data.answers = 0;

	/* Load pengines.js incrementally because we wish to ask the
	   one from the pengine server rather than a packaged one.
	*/

	require([config.http.locations.pengines+"/pengines.js"],
		function() {

	  data.prolog = new Pengine({
	    server: config.http.locations.pengines,
	    runner: elem,
	    application: "trill_on_swish",
	    src: "\n\
\n\
\n\
:- dynamic\n\
        classAssertion/2,\n\
        propertyAssertion/3,\n\
        subPropertyOf/2,\n\
        subClassOf/2,\n\
        equivalentClasses/1,\n\
        differentIndividuals/1,\n\
        sameIndividual/1,\n\
        intersectionOf/1,\n\
        unionOf/1,\n\
        propertyRange/2,\n\
        propertyDomain/2,\n\
        annotationAssertion/3,\n\
        exactCardinality/2,\n\
        exactCardinality/3,\n\
        maxCardinality/2,\n\
        maxCardinality/3,\n\
        minCardinality/2,\n\
        minCardinality/3.\n\
\n\
:- use_module(library(lists),[member/2]).\n\
:- use_module(library(pengines)).\n\
:- use_module(library(trill)).\n\
\n\
\n\
:- discontiguous(valid_axiom/1).\n\
:- discontiguous(axiompred/1).\n\
:- discontiguous(axiom_arguments/2).\n\
\n\
\n\
builtin_class('http://www.w3.org/2002/07/owl#Thing').\n\
builtin_class('http://www.w3.org/2002/07/owl#Nothing').\n\
is_class(C) :- class(C).\n\
is_class(C) :- builtin_class(C).\n\
\n\
\n\
% TODO: hasKey\n\
\n\
/****************************************\n\
  AXIOMS\n\
  ****************************************/\n\
\n\
%% entity(?IRI)\n\
% the fundamental building blocks of owl 2 ontologies, and they define the vocabulary (the named terms) of an ontology\n\
%\n\
% @see individual/1, property/1, class/1, datatype/1\n\
entity(A) :- individual(A).\n\
entity(A) :- property(A).\n\
entity(A) :- class(A).\n\
entity(A) :- datatype(A).\n\
axiom_arguments(entity,[iri]).\n\
valid_axiom(entity(A)) :- subsumed_by([A],[iri]).\n\
\n\
%declarationAxiom(individual(A)) :- individual(A). % TODO - check this\n\
declarationAxiom(namedIndividual(A)) :- namedIndividual(A).\n\
declarationAxiom(objectProperty(A)) :- objectProperty(A).\n\
declarationAxiom(dataProperty(A)) :- dataProperty(A).\n\
declarationAxiom(annotationProperty(A)) :- annotationProperty(A).  % VV added 9/3/2010\n\
declarationAxiom(class(A)) :- class(A).\n\
declarationAxiom(datatype(A)) :- datatype(A).\n\
% TODO: check. here we treat the ontology declaration as an axiom;\n\
% this liberal definition of axiom allows us to iterate over axiom/1\n\
% to find every piece of information in the ontology.\n\
declarationAxiom(ontology(A)) :- ontology(A).\n\
\n\
%% class(?IRI)\n\
% Classes can be understood as sets of individuals\n\
:- dynamic(class/1).\n\
\n\
axiompred(class/1).\n\
axiom_arguments(class,[iri]).\n\
valid_axiom(class(A)) :- subsumed_by([A],[iri]).\n\
\n\
%% datatype(?IRI)\n\
% Datatypes are entities that refer to sets of values described by a datatype map\n\
:- dynamic(datatype/1).\n\
\n\
axiompred(datatype/1).\n\
axiom_arguments(datatype,[iri]).\n\
valid_axiom(datatype(A)) :- subsumed_by([A],[iri]).\n\
\n\
%% property(?IRI)\n\
% Properties connect individuals with either other individuals or with literals\n\
%\n\
% @see dataProperty/1, objectProperty/1, annotationProperty/1\n\
property(A) :- dataProperty(A).\n\
property(A) :- objectProperty(A).\n\
property(A) :- annotationProperty(A).\n\
axiom_arguments(property,[iri]).\n\
valid_axiom(property(A)) :- subsumed_by([A],[iri]).\n\
\n\
%% objectProperty(?IRI)\n\
% Object properties connect pairs of individuals\n\
:- dynamic(objectProperty/1).\n\
\n\
axiompred(objectProperty/1).\n\
axiom_arguments(objectProperty,[iri]).\n\
valid_axiom(objectProperty(A)) :- subsumed_by([A],[iri]).\n\
\n\
%% dataProperty(?IRI)\n\
% Data properties connect individuals with literals. In some knowledge representation systems, functional data properties are called attributes.\n\
:- dynamic(dataProperty/1).\n\
\n\
axiompred(dataProperty/1).\n\
axiom_arguments(dataProperty,[iri]).\n\
valid_axiom(dataProperty(A)) :- subsumed_by([A],[iri]).\n\
\n\
%% annotationProperty(?IRI)\n\
% Annotation properties can be used to provide an annotation for an ontology, axiom, or an IRI\n\
:- dynamic(annotationProperty/1).\n\
\n\
axiompred(annotationProperty/1).\n\
axiom_arguments(annotationProperty,[iri]).\n\
valid_axiom(annotationProperty(A)) :- subsumed_by([A],[iri]).\n\
\n\
\n\
%% individual(?IRI)\n\
% Individuals represent actual objects from the domain being modeled\n\
% @see anonymousIndividual/1, namedIndividual/1\n\
individual(A) :- anonymousIndividual(A).\n\
individual(A) :- namedIndividual(A).\n\
%individual(A) :- nonvar(A),iri(A),\\+property(A),\\+class(A),\\+ontology(A). % TODO: check: make individuals the default\n\
axiom_arguments(individual,[iri]).\n\
valid_axiom(individual(A)) :- subsumed_by([A],[iri]).\n\
\n\
%% namedIndividual(?IRI)\n\
% Named individuals are given an explicit name that can be used in any ontology in the import closure to refer to the same individual\n\
:- dynamic(namedIndividual/1).\n\
\n\
axiompred(namedIndividual/1).\n\
axiom_arguments(namedIndividual,[iri]).\n\
valid_axiom(namedIndividual(A)) :- subsumed_by([A],[iri]).\n\
\n\
%% anonymousIndividual(?IRI)\n\
% Anonymous individuals are local to the ontology they are contained in. Analagous to bnodes\n\
% @see construct/1\n\
:- dynamic(anonymousIndividual/1).\n\
\n\
axiompred(anonymousIndividual/1).\n\
axiom_arguments(anonymousIndividual,[iri]).\n\
valid_axiom(anonymousIndividual(A)) :- subsumed_by([A],[iri]).\n\
\n\
%% construct(?IRI)\n\
% @see axiom/1, annotation/1, ontology/1\n\
construct(A) :- axiom(A).\n\
construct(A) :- annotation(A).\n\
construct(A) :- ontology(A).\n\
axiom_arguments(construct,[iri]).\n\
valid_axiom(construct(A)) :- subsumed_by([A],[iri]).\n\
\n\
%% axiom(?Axiom)\n\
% The main component of an OWL 2 ontology is a set of axioms - statements that say what is true in the domain being modeled.\n\
% @see classAxiom/1, propertyAxiom/1, fact/1\n\
axiom(A) :- classAxiom(A).\n\
axiom(A) :- propertyAxiom(A).\n\
axiom(hasKey(A,B)) :- hasKey(A,B).\n\
axiom(A) :- fact(A).\n\
axiom(A) :- declarationAxiom(A).\n\
%axiom(annotation(A,B,C)) :-\n\
%	annotation(A,B,C). % CJM-treat annotations as axioms\n\
axiom_arguments(axiom,[axiom]).\n\
valid_axiom(axiom(A)) :- subsumed_by([A],[axiom]).\n\
\n\
%% classAxiom(?Axiom)\n\
% OWL 2 provides axioms that allow relationships to be established between class expressions. This predicate reifies the actual axiom\n\
% @see equivalentClasses/1, disjointClasses/1, subClassOf/2, disjointUnion/2\n\
classAxiom(equivalentClasses(A)) :- equivalentClasses(A).\n\
classAxiom(disjointClasses(A)) :- disjointClasses(A).\n\
classAxiom(subClassOf(A, B)) :- subClassOf(A, B).\n\
classAxiom(disjointUnion(A, B)) :- disjointUnion(A, B).\n\
axiom_arguments(classAxiom,[axiom]).\n\
valid_axiom(classAxiom(A)) :- subsumed_by([A],[axiom]).\n\
\n\
%% subClassOf(?SubClass:ClassExpression, ?SuperClass:ClassExpression)\n\
% A subclass axiom SubClassOf( CE1 CE2 ) states that the class expression CE1 is a subclass of the class expression CE2\n\
%\n\
%   @param SubClass a classExpression/1 representing the more specific class\n\
%   @param SuperClass a classExpression/1 representing the more general class\n\
:- dynamic(subClassOf/2).\n\
\n\
axiompred(subClassOf/2).\n\
axiom_arguments(subClassOf,[classExpression, classExpression]).\n\
valid_axiom(subClassOf(A, B)) :- subsumed_by([A, B],[classExpression, classExpression]).\n\
\n\
\n\
%% equivalentClasses(?ClassExpressions:set(ClassExpression))\n\
% An equivalent classes axiom EquivalentClasses( CE1 ... CEn ) states that all of the class expressions CEi, 1 <= i <= n, are semantically equivalent to each other.\n\
:- dynamic(equivalentClasses/1).\n\
\n\
axiompred(equivalentClasses/1).\n\
axiom_arguments(equivalentClasses,[set(classExpression)]).\n\
valid_axiom(equivalentClasses(A)) :- subsumed_by([A],[set(classExpression)]).\n\
\n\
%% disjointClasses(?ClassExpressions:set(ClassExpression))\n\
% A disjoint classes axiom DisjointClasses( CE1 ... CEn ) states that all of the class expressions CEi, 1 <= i <= n, are pairwise disjoint; that is, no individual can be at the same time an instance of both CEi and CEj for i != j\n\
:- dynamic(disjointClasses/1).\n\
\n\
axiompred(disjointClasses/1).\n\
axiom_arguments(disjointClasses,[set(classExpression)]).\n\
valid_axiom(disjointClasses(A)) :- subsumed_by([A],[set(classExpression)]).\n\
\n\
%% disjointUnion(?ClassExpression, ?ClassExpressions:set(ClassExpression))\n\
% A disjoint union axiom DisjointUnion( C CE1 ... CEn ) states that a class C is a disjoint union of the class expressions CEi, 1 <= i <= n, all of which are pairwise disjoint.\n\
:- dynamic(disjointUnion/2).\n\
\n\
axiompred(disjointUnion/2).\n\
axiom_arguments(disjointUnion,[classExpression,set(classExpression)]).\n\
valid_axiom(disjointUnion(A,B)) :- subsumed_by([A,B],[classExpression,set(classExpression)]).\n\
\n\
%% propertyAxiom(?Axiom)\n\
% OWL 2 provides axioms that can be used to characterize and establish relationships between object property expressions. This predicate reifies the actual axiom\n\
%\n\
% @see symmetricProperty/1, inverseFunctionalProperty/1, transitiveProperty/1, asymmetricProperty/1, subPropertyOf/2, functionalProperty/1, irreflexiveProperty/1, disjointProperties/1, propertyDomain/2, reflexiveProperty/1, propertyRange/2, equivalentProperties/1, inverseProperties/2\n\
propertyAxiom(symmetricProperty(A)) :- symmetricProperty(A).\n\
propertyAxiom(inverseFunctionalProperty(A)) :- inverseFunctionalProperty(A).\n\
propertyAxiom(transitiveProperty(A)) :- transitiveProperty(A).\n\
propertyAxiom(asymmetricProperty(A)) :- asymmetricProperty(A).\n\
propertyAxiom(subPropertyOf(A, B)) :- subPropertyOf(A, B).\n\
propertyAxiom(functionalProperty(A)) :- functionalProperty(A).\n\
propertyAxiom(irreflexiveProperty(A)) :- irreflexiveProperty(A).\n\
propertyAxiom(disjointProperties(A)) :- disjointProperties(A).\n\
propertyAxiom(propertyDomain(A, B)) :- propertyDomain(A, B).\n\
propertyAxiom(reflexiveProperty(A)) :- reflexiveProperty(A).\n\
propertyAxiom(propertyRange(A, B)) :- propertyRange(A, B).\n\
propertyAxiom(equivalentProperties(A)) :- equivalentProperties(A).\n\
propertyAxiom(inverseProperties(A, B)) :- inverseProperties(A, B).\n\
axiom_arguments(propertyAxiom,[axiom]).\n\
valid_axiom(propertyAxiom(A)) :- subsumed_by([A],[axiom]).\n\
\n\
\n\
%% subPropertyOf(?Sub:PropertyExpression, ?Super:ObjectPropertyExpression)\n\
% subproperty axioms are analogous to subclass axioms\n\
% (extensional predicate - can be asserted)\n\
:- dynamic(subPropertyOf/2).\n\
\n\
axiompred(subPropertyOf/2).\n\
axiom_arguments(subPropertyOf,[propertyExpression, objectPropertyExpression]).\n\
valid_axiom(subPropertyOf(A, B)) :- subsumed_by([A, B],[propertyExpression, objectPropertyExpression]).\n\
\n\
%% subObjectPropertyOf(?Sub:ObjectPropertyExpressionOrChain, ?Super:ObjectPropertyExpression)\n\
% The basic form is SubPropertyOf( OPE1 OPE2 ). This axiom states that the object property expression OPE1 is a subproperty of the object property expression OPE2 - that is, if an individual x is connected by OPE1 to an individual y, then x is also connected by OPE2 to y. The more complex form is SubPropertyOf( PropertyChain( OPE1 ... OPEn ) OPE ). This axiom states that, if an individual x is connected by a sequence of object property expressions OPE1, ..., OPEn with an individual y, then x is also connected with y by the object property expression OPE\n\
subObjectPropertyOf(A, B) :- subPropertyOf(A, B),subsumed_by([A, B],[objectPropertyExpressionOrChain, objectPropertyExpression]).\n\
axiom_arguments(subObjectPropertyOf,[objectPropertyExpressionOrChain, objectPropertyExpression]).\n\
valid_axiom(subObjectPropertyOf(A, B)) :- subsumed_by([A, B],[objectPropertyExpressionOrChain, objectPropertyExpression]).\n\
\n\
%% subDataPropertyOf(?Sub:DataPropertyExpression, ?Super:DataPropertyExpression)\n\
% A data subproperty axiom SubPropertyOf( DPE1 DPE2 ) states that the data property expression DPE1 is a subproperty of the data property expression DPE2 - that is, if an individual x is connected by OPE1 to a literal y, then x is connected by OPE2 to y as well.\n\
subDataPropertyOf(A, B) :- subPropertyOf(A, B),subsumed_by([A, B],[dataPropertyExpression, dataPropertyExpression]).\n\
axiom_arguments(subDataPropertyOf,[dataPropertyExpression, dataPropertyExpression]).\n\
valid_axiom(subDataPropertyOf(A, B)) :- subsumed_by([A, B],[dataPropertyExpression, dataPropertyExpression]).\n\
\n\
%% subAnnotationPropertyOf(?Sub:AnnotationProperty, ?Super:AnnotationProperty)\n\
% An annotation subproperty axiom SubPropertyOf( AP1 AP2 ) states that the annotation property AP1 is a subproperty of the annotation property AP2\n\
subAnnotationPropertyOf(A, B) :- subPropertyOf(A, B),subsumed_by([A, B],[annotationProperty, annotationProperty]).\n\
axiom_arguments(subAnnotationPropertyOf,[annotationProperty, annotationProperty]).\n\
valid_axiom(subAnnotationPropertyOf(A, B)) :- subsumed_by([A, B],[annotationProperty, annotationProperty]).\n\
\n\
%% equivalentProperties(?PropertyExpressions:set(PropertyExpression))\n\
% An equivalent object properties axiom EquivalentProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are semantically equivalent to each other\n\
% (extensional predicate - can be asserted)\n\
:- dynamic(equivalentProperties/1).\n\
\n\
axiompred(equivalentProperties/1).\n\
axiom_arguments(equivalentProperties,[set(propertyExpression)]).\n\
valid_axiom(equivalentProperties(A)) :- subsumed_by([A],[set(propertyExpression)]).\n\
\n\
%% equivalentObjectProperties(?PropertyExpressions:set(ObjectPropertyExpression))\n\
% An equivalent object properties axiom EquivalentObjectProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are semantically equivalent to each other\n\
equivalentObjectProperties(A) :- equivalentProperties(A),subsumed_by([A],[set(objectPropertyExpression)]).\n\
axiom_arguments(equivalentObjectProperties,[set(objectPropertyExpression)]).\n\
valid_axiom(equivalentObjectProperties(A)) :- subsumed_by([A],[set(objectPropertyExpression)]).\n\
\n\
%% equivalentDataProperties(?PropertyExpressions:set(DataPropertyExpression))\n\
% An equivalent data properties axiom EquivalentProperties( DPE1 ... DPEn ) states that all the data property expressions DPEi, 1 <= i <= n, are semantically equivalent to each other. This axiom allows one to use each DPEi as a synonym for each DPEj - that is, in any expression in the ontology containing such an axiom, DPEi can be replaced with DPEj without affecting the meaning of the ontology\n\
equivalentDataProperties(A) :- equivalentProperties(A),subsumed_by([A],[set(dataPropertyExpression)]).\n\
axiom_arguments(equivalentDataProperties,[set(dataPropertyExpression)]).\n\
valid_axiom(equivalentDataProperties(A)) :- subsumed_by([A],[set(dataPropertyExpression)]).\n\
\n\
%% disjointProperties(?PropertyExpressions:set(PropertyExpression))\n\
% A disjoint properties axiom DisjointProperties( PE1 ... PEn ) states that all of the property expressions PEi, 1 <= i <= n, are pairwise disjoint\n\
% (extensional predicate - can be asserted)\n\
:- dynamic(disjointProperties/1).\n\
\n\
axiompred(disjointProperties/1).\n\
axiom_arguments(disjointProperties,[set(propertyExpression)]).\n\
valid_axiom(disjointProperties(A)) :- subsumed_by([A],[set(propertyExpression)]).\n\
\n\
%% disjointObjectProperties(?PropertyExpressions:set(ObjectPropertyExpression))\n\
% A disjoint object properties axiom DisjointProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are pairwise disjoint; that is, no individual x can be connected to an individual y by both OPEi and OPEj for i != j.\n\
disjointObjectProperties(A) :- disjointProperties(A),subsumed_by([A],[set(objectPropertyExpression)]).\n\
axiom_arguments(disjointObjectProperties,[set(objectPropertyExpression)]).\n\
valid_axiom(disjointObjectProperties(A)) :- subsumed_by([A],[set(objectPropertyExpression)]).\n\
\n\
%% disjointDataProperties(?PropertyExpressions:set(DataPropertyExpression))\n\
% A disjoint data properties axiom DisjointProperties( DPE1 ... DPEn ) states that all of the data property expressions DPEi, 1 <= i <= n, are pairwise disjoint; that is, no individual x can be connected to a literal y by both DPEi and DPEj for i !- j.\n\
disjointDataProperties(A) :- disjointProperties(A),subsumed_by([A],[set(dataPropertyExpression)]).\n\
axiom_arguments(disjointDataProperties,[set(dataPropertyExpression)]).\n\
valid_axiom(disjointDataProperties(A)) :- subsumed_by([A],[set(dataPropertyExpression)]).\n\
\n\
%% inverseProperties(?ObjectPropertyExpression1:ObjectPropertyExpression, ?ObjectPropertyExpression2:ObjectPropertyExpression)\n\
% An inverse object properties axiom InverseProperties( OPE1 OPE2 ) states that the object property expression OPE1 is an inverse of the object property expression OPE2\n\
% (note there are no inverse data properties, as literals are not connected to individuals)\n\
% Example:\n\
% =|inverseProperties(partOf,hasPart)|=\n\
% (extensional predicate - can be asserted)\n\
:- dynamic(inverseProperties/2).\n\
\n\
axiompred(inverseProperties/2).\n\
axiom_arguments(inverseProperties,[objectPropertyExpression, objectPropertyExpression]).\n\
valid_axiom(inverseProperties(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, objectPropertyExpression]).\n\
\n\
%% propertyDomain(?PropertyExpression, ?CE)\n\
%  A property domain axiom PropertyDomain( PE CE ) states that the\n\
%  domain of the property expression PE is CE\n\
% (extensional predicate - can be asserted)\n\
\n\
:- dynamic(propertyDomain/2).\n\
\n\
axiompred(propertyDomain/2).\n\
axiom_arguments(propertyDomain,[propertyExpression, classExpression]).\n\
valid_axiom(propertyDomain(A, B)) :- subsumed_by([A, B],[propertyExpression, classExpression]).\n\
\n\
%% objectPropertyDomain(?ObjectPropertyExpression, ?ClassExpression)\n\
% An object property domain axiom PropertyDomain( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE\n\
objectPropertyDomain(A, B) :- propertyDomain(A, B),subsumed_by([A, B],[objectPropertyExpression, classExpression]).\n\
axiom_arguments(objectPropertyDomain,[objectPropertyExpression, classExpression]).\n\
valid_axiom(objectPropertyDomain(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, classExpression]).\n\
\n\
%% dataPropertyDomain(?DataPropertyExpression, ?ClassExpression)\n\
% A data property domain axiom PropertyDomain( DPE CE ) states that the domain of the data property expression DPE is the class expression CE - that is, if an individual x is connected by DPE with some literal, then x is an instance of CE\n\
dataPropertyDomain(A, B) :- propertyDomain(A, B),subsumed_by([A, B],[dataPropertyExpression, classExpression]).\n\
axiom_arguments(dataPropertyDomain,[dataPropertyExpression, classExpression]).\n\
valid_axiom(dataPropertyDomain(A, B)) :- subsumed_by([A, B],[dataPropertyExpression, classExpression]).\n\
\n\
%% annotationPropertyDomain(?AnnotationProperty, ?IRI)\n\
% An annotation property domain axiom PropertyDomain( AP U ) states that the domain of the annotation property AP is the IRI U. Such axioms have no effect on the Direct Semantics of OWL 2\n\
annotationPropertyDomain(A, B) :- propertyDomain(A, B),subsumed_by([A, B],[annotationProperty, iri]).\n\
axiom_arguments(annotationPropertyDomain,[annotationProperty, iri]).\n\
valid_axiom(annotationPropertyDomain(A, B)) :- subsumed_by([A, B],[annotationProperty, iri]).\n\
\n\
%% propertyRange(?PropertyExpression, ?ClassExpression)\n\
% An object property domain axiom PropertyRange( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE\n\
% (extensional predicate - can be asserted)\n\
:- dynamic(propertyRange/2).\n\
\n\
axiompred(propertyRange/2).\n\
axiom_arguments(propertyRange,[propertyExpression, classExpression]).\n\
valid_axiom(propertyRange(A, B)) :- subsumed_by([A, B],[propertyExpression, classExpression]).\n\
\n\
%% objectPropertyRange(?ObjectPropertyExpression, ?ClassExpression)\n\
% An object property domain axiom PropertyRange( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE\n\
objectPropertyRange(A, B) :- propertyRange(A, B),subsumed_by([A, B],[objectPropertyExpression, classExpression]).\n\
axiom_arguments(objectPropertyRange,[objectPropertyExpression, classExpression]).\n\
valid_axiom(objectPropertyRange(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, classExpression]).\n\
\n\
%% dataPropertyRange(?ObjectPropertyExpression, ?DataRange)\n\
% A data property range axiom PropertyRange( DPE DR ) states that the range of the data property expression DPE is the data range DR - that is, if some individual is connected by DPE with a literal x, then x is in DR. The arity of DR MUST be one\n\
dataPropertyRange(A, B) :- propertyRange(A, B),subsumed_by([A, B],[dataPropertyExpression, dataRange]).\n\
axiom_arguments(dataPropertyRange,[objectPropertyExpression, dataRange]).\n\
valid_axiom(dataPropertyRange(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, dataRange]).\n\
\n\
%% annotationPropertyRange(?AnnotationProperty, ?IRI)\n\
% An annotation property range axiom PropertyRange( AP U ) states that the range of the annotation property AP is the IRI U. Such axioms have no effect on the Direct Semantics of OWL 2\n\
annotationPropertyRange(A, B) :- propertyRange(A, B),subsumed_by([A, B],[annotationProperty, iri]).\n\
axiom_arguments(annotationPropertyRange,[annotationProperty, iri]).\n\
valid_axiom(annotationPropertyRange(A, B)) :- subsumed_by([A, B],[annotationProperty, iri]).\n\
\n\
%% functionalProperty(?PropertyExpression)\n\
% An object property functionality axiom FunctionalProperty( OPE ) states that the object property expression OPE is functional - that is, for each individual x, there can be at most one distinct individual y such that x is connected by OPE to y\n\
% (extensional predicate - can be asserted)\n\
:- dynamic(functionalProperty/1).\n\
\n\
axiompred(functionalProperty/1).\n\
axiom_arguments(functionalProperty,[propertyExpression]).\n\
valid_axiom(functionalProperty(A)) :- subsumed_by([A],[propertyExpression]).\n\
\n\
%% functionalObjectProperty(?ObjectPropertyExpression)\n\
% An object property functionality axiom FunctionalProperty( OPE ) states that the object property expression OPE is functional - that is, for each individual x, there can be at most one distinct individual y such that x is connected by OPE to y\n\
functionalObjectProperty(A) :- functionalProperty(A),subsumed_by([A],[objectPropertyExpression]).\n\
axiom_arguments(functionalObjectProperty,[objectPropertyExpression]).\n\
valid_axiom(functionalObjectProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).\n\
\n\
%% functionalDataProperty(?DataPropertyExpression)\n\
% A data property functionality axiom FunctionalProperty( DPE ) states that the data property expression DPE is functional - that is, for each individual x, there can be at most one distinct literal y such that x is connected by DPE with y\n\
functionalDataProperty(A) :- functionalProperty(A),subsumed_by([A],[dataPropertyExpression]).\n\
axiom_arguments(functionalDataProperty,[dataPropertyExpression]).\n\
valid_axiom(functionalDataProperty(A)) :- subsumed_by([A],[dataPropertyExpression]).\n\
\n\
%% inverseFunctionalProperty(?ObjectPropertyExpression)\n\
% An object property inverse functionality axiom InverseFunctionalProperty( OPE ) states that the object property expression OPE is inverse-functional - that is, for each individual x, there can be at most one individual y such that y is connected by OPE with x. Note there are no InverseFunctional DataProperties\n\
:- dynamic(inverseFunctionalProperty/1).\n\
\n\
axiompred(inverseFunctionalProperty/1).\n\
axiom_arguments(inverseFunctionalProperty,[objectPropertyExpression]).\n\
valid_axiom(inverseFunctionalProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).\n\
\n\
%% reflexiveProperty(?ObjectPropertyExpression)\n\
% An object property reflexivity axiom ReflexiveProperty( OPE ) states that the object property expression OPE is reflexive - that is, each individual is connected by OPE to itself\n\
:- dynamic(reflexiveProperty/1).\n\
\n\
axiompred(reflexiveProperty/1).\n\
axiom_arguments(reflexiveProperty,[objectPropertyExpression]).\n\
valid_axiom(reflexiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).\n\
\n\
%% irreflexiveProperty(?ObjectPropertyExpression)\n\
% An object property reflexivity axiom ReflexiveProperty( OPE ) states that the object property expression OPE is reflexive - that is, no individual is connected by OPE to itsel\n\
:- dynamic(irreflexiveProperty/1).\n\
\n\
axiompred(irreflexiveProperty/1).\n\
axiom_arguments(irreflexiveProperty,[objectPropertyExpression]).\n\
valid_axiom(irreflexiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).\n\
\n\
%% symmetricProperty(?ObjectPropertyExpression)\n\
% An object property symmetry axiom SymmetricProperty( OPE ) states that the object property expression OPE is symmetric - that is, if an individual x is connected by OPE to an individual y, then y is also connected by OPE to x\n\
:- dynamic(symmetricProperty/1).\n\
\n\
axiompred(symmetricProperty/1).\n\
axiom_arguments(symmetricProperty,[objectPropertyExpression]).\n\
valid_axiom(symmetricProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).\n\
\n\
%% asymmetricProperty(?ObjectPropertyExpression)\n\
% An object property asymmetry axiom AsymmetricProperty( OPE ) states that the object property expression OPE is asymmetric - that is, if an individual x is connected by OPE to an individual y, then y cannot be connected by OPE to x\n\
:- dynamic(asymmetricProperty/1).\n\
\n\
axiompred(asymmetricProperty/1).\n\
axiom_arguments(asymmetricProperty,[objectPropertyExpression]).\n\
valid_axiom(asymmetricProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).\n\
\n\
%% transitiveProperty(?ObjectPropertyExpression)\n\
% An object property transitivity axiom TransitiveProperty( OPE ) states that the object property expression OPE is transitive - that is, if an individual x is connected by OPE to an individual y that is connected by OPE to an individual z, then x is also connected by OPE to z\n\
:- dynamic(transitiveProperty/1).\n\
\n\
axiompred(transitiveProperty/1).\n\
axiom_arguments(transitiveProperty,[objectPropertyExpression]).\n\
valid_axiom(transitiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).\n\
\n\
%% hasKey(?ClassExpression,?PropertyExpression)\n\
% A key axiom HasKey( CE PE1 ... PEn ) states that each (named) instance of the class expression CE is uniquely identified by the (data or object) property expressions PEi - that is, no two distinct (named) instances of CE can coincide on the values of all property expressions PEi\n\
:- dynamic(hasKey/2).\n\
\n\
axiompred(hasKey/2).\n\
axiom_arguments(hasKey,[classExpression,propertyExpression]).\n\
valid_axiom(hasKey(CE,PE)) :- subsumed_by([CE,PE],[classExpression,propertyExpression]).\n\
\n\
\n\
%% fact(?Axiom)\n\
% OWL 2 supports a rich set of axioms for stating assertions - axioms about individuals that are often also called facts. The fact/1 predicate reifies the fact predicate\n\
%\n\
% @see annotationAssertion/3, differentIndividuals/1, negativePropertyAssertion/3, propertyAssertion/3, sameIndividual/1, classAssertion/2\n\
fact(annotationAssertion(A, B, C)) :- annotationAssertion(A, B, C).\n\
fact(differentIndividuals(A)) :- differentIndividuals(A).\n\
fact(negativePropertyAssertion(A, B, C)) :- negativePropertyAssertion(A, B, C).\n\
fact(propertyAssertion(A, B, C)) :- propertyAssertion(A, B, C).\n\
fact(sameIndividual(A)) :- sameIndividual(A).\n\
fact(classAssertion(A, B)) :- classAssertion(A, B).\n\
axiom_arguments(fact,[axiom]).\n\
valid_axiom(fact(A)) :- subsumed_by([A],[axiom]).\n\
\n\
%% sameIndividual(?Individuals:set(Individual))\n\
% An individual equality axiom SameIndividual( a1 ... an ) states that all of the individuals ai, 1 <= i <= n, are equal to each other.\n\
% note that despite the name of this predicate, it accepts a list of individuals as argument\n\
:- dynamic(sameIndividual/1).\n\
\n\
axiompred(sameIndividual/1).\n\
axiom_arguments(sameIndividual,[set(individual)]).\n\
valid_axiom(sameIndividual(A)) :- subsumed_by([A],[set(individual)]).\n\
\n\
%% differentIndividuals(?Individuals:set(Individual))\n\
% An individual inequality axiom DifferentIndividuals( a1 ... an ) states that all of the individuals ai, 1 <= i <= n, are different from each other\n\
:- dynamic(differentIndividuals/1).\n\
\n\
axiompred(differentIndividuals/1).\n\
axiom_arguments(differentIndividuals,[set(individual)]).\n\
valid_axiom(differentIndividuals(A)) :- subsumed_by([A],[set(individual)]).\n\
\n\
%% classAssertion(?ClassExpression, ?Individual)\n\
% A class assertion ClassAssertion( CE a ) states that the individual a is an instance of the class expression CE\n\
:- dynamic(classAssertion/2).\n\
\n\
axiompred(classAssertion/2).\n\
axiom_arguments(classAssertion,[classExpression, individual]).\n\
valid_axiom(classAssertion(A, B)) :- subsumed_by([A, B],[classExpression, individual]).\n\
\n\
%% propertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)\n\
% A positive object property assertion PropertyAssertion( OPE a1 a2 ) states that the individual a1 is connected by the object property expression OPE to the individual a2\n\
% (extensional predicate - can be asserted)\n\
:- dynamic(propertyAssertion/3).\n\
\n\
axiompred(propertyAssertion/3).\n\
axiom_arguments(propertyAssertion,[propertyExpression, individual, individual]).\n\
valid_axiom(propertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[propertyExpression, individual, individual]).\n\
\n\
%% objectPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)\n\
% A positive object property assertion PropertyAssertion( OPE a1 a2 ) states that the individual a1 is connected by the object property expression OPE to the individual a2\n\
objectPropertyAssertion(A, B, C) :- propertyAssertion(A, B, C),subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).\n\
axiom_arguments(objectPropertyAssertion,[objectPropertyExpression, individual, individual]).\n\
valid_axiom(objectPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).\n\
\n\
%% dataPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetValue:Literal)\n\
% A positive data property assertion PropertyAssertion( DPE a lt ) states that the individual a is connected by the data property expression DPE to the literal lt\n\
dataPropertyAssertion(A, B, C) :- propertyAssertion(A, B, C),subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).\n\
axiom_arguments(dataPropertyAssertion,[objectPropertyExpression, individual, literal]).\n\
valid_axiom(dataPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).\n\
\n\
%% negativePropertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)\n\
% A negative object property assertion NegativePropertyAssertion( OPE a1 a2 ) states that the individual a1 is not connected by the object property expression OPE to the individual a2\n\
% (extensional predicate - can be asserted)\n\
:- dynamic(negativePropertyAssertion/3).\n\
\n\
axiompred(negativePropertyAssertion/3).\n\
axiom_arguments(negativePropertyAssertion,[propertyExpression, individual, individual]).\n\
valid_axiom(negativePropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[propertyExpression, individual, individual]).\n\
\n\
%% negativeObjectPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)\n\
% A negative object property assertion NegativePropertyAssertion( OPE a1 a2 ) states that the individual a1 is not connected by the object property expression OPE to the individual a2\n\
negativeObjectPropertyAssertion(A, B, C) :- negativePropertyAssertion(A, B, C),subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).\n\
axiom_arguments(negativeObjectPropertyAssertion,[objectPropertyExpression, individual, individual]).\n\
valid_axiom(negativeObjectPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).\n\
\n\
%% negativeDataPropertyAssertion(?DataPropertyExpression, ?SourceIndividual:Individual, ?TargetValue:Literal)\n\
% A negative data property assertion NegativePropertyAssertion( DPE a lt ) states that the individual a is not connected by the data property expression DPE to the literal lt\n\
negativeDataPropertyAssertion(A, B, C) :- negativePropertyAssertion(A, B, C),subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).\n\
axiom_arguments(negativeDataPropertyAssertion,[dataPropertyExpression, individual, literal]).\n\
valid_axiom(negativeDataPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).\n\
\n\
%% annotationAssertion(?AnnotationProperty, ?AnnotationSubject, ?AnnotationValue)\n\
% An annotation assertion AnnotationAssertion( AP as av ) states that the annotation subject as - an IRI or an anonymous individual - is annotated with the annotation property AP and the annotation value av\n\
:- dynamic(annotationAssertion/3).\n\
\n\
axiompred(annotationAssertion/3).\n\
axiom_arguments(annotationAssertion,[annotationProperty, annotationSubject, annotationValue]).\n\
valid_axiom(annotationAssertion(A, B, C)) :- subsumed_by([A, B, C],[annotationProperty, annotationSubject, annotationValue]).\n\
annotationSubject(_).\n\
annotationValue(_).\n\
\n\
%% annotation(?IRI,?AnnotationProperty,?AnnotationValue)\n\
%\n\
% @see annotationAnnotation/3, ontologyAnnotation/3, axiomAnnotation/3\n\
:- dynamic(annotation/3).\n\
\n\
axiompred(annotation/3).\n\
\n\
annotation(annotationAnnotation(A, B, C)) :- annotationAnnotation(A, B, C).\n\
annotation(axiomAnnotation(A, B, C)) :- axiomAnnotation(A, B, C).\n\
axiom_arguments(annotation,[iri,annotationProperty,annotationValue]).\n\
valid_axiom(annotation(A,B,C)) :- subsumed_by([A,B,C],[iri,annotationProperty,annotationValue]).\n\
\n\
%% ontologyAnnotation(?Ontology, ?AnnotationProperty, ?AnnotationValue)\n\
ontologyAnnotation(Ontology,AP,AV) :-\n\
	annotation(Ontology,AP,AV),\n\
	ontology(Ontology).\n\
axiom_arguments(ontologyAnnotation,[ontology, annotationProperty, annotationValue]).\n\
valid_axiom(ontologyAnnotation(A, B, C)) :- subsumed_by([A, B, C],[ontology, annotationProperty, annotationValue]).\n\
\n\
%% axiomAnnotation(?Axiom, ?AnnotationProperty, ?AnnotationValue)\n\
axiomAnnotation(Axiom,AP,AV) :-\n\
	annotation(Axiom,AP,AV),\n\
	axiom(Axiom).\n\
axiom_arguments(axiomAnnotation,[axiom, annotationProperty, annotationValue]).\n\
valid_axiom(axiomAnnotation(A, B, C)) :- subsumed_by([A, B, C],[axiom, annotationProperty, annotationValue]).\n\
\n\
%% annotationAnnotation(?Annotation, ?AnnotationProperty, ?AnnotationValue)\n\
annotationAnnotation(Annotation,AP,AV) :-\n\
	annotation(Annotation,AP,AV),\n\
	annotation(Annotation).\n\
axiom_arguments(annotationAnnotation,[annotation, annotationProperty, annotationValue]).\n\
valid_axiom(annotationAnnotation(A, B, C)) :- subsumed_by([A, B, C],[annotation, annotationProperty, annotationValue]).\n\
\n\
%% ontology(?IRI)\n\
% An ontology in OWL2 is a collection of OWL Axioms\n\
:- dynamic(ontology/1).\n\
\n\
axiompred(ontology/1).\n\
axiom_arguments(ontology,[iri]).\n\
valid_axiom(ontology(A)) :- subsumed_by([A],[iri]).\n\
\n\
%% ontologyDirective(?OntologyIRI,?IRI)\n\
% @see ontologyImport/2, ontologyAxiom/2\n\
ontologyDirective(A, B) :- ontologyImport(A, B).\n\
ontologyDirective(A, B) :- ontologyAxiom(A, B).\n\
ontologyDirective(A, B) :- ontologyVersionInfo(A, B).\n\
axiom_arguments(ontologyDirective,[ontology, iri]).\n\
valid_axiom(ontologyDirective(A, B)) :- subsumed_by([A, B],[ontology, iri]).\n\
\n\
%% ontologyAxiom(?Ontology, ?Axiom)\n\
% True if Ontology contains Axiom.\n\
% Axiom is a prolog term that is typically asserted and separately and can thus can be executed as a goal.\n\
% For example, an ontology http://example.org# will contain redundant assertions:\n\
% ==\n\
% subClassOf('http://example.org#a', 'http://example.org#b').\n\
% ontologyAxiom('http://example.org#', subClassOf('http://example.org#a','http://example.org#b')).\n\
% ==\n\
:- dynamic(ontologyAxiom/2).\n\
\n\
axiompred(ontologyAxiom/2).\n\
axiom_arguments(ontologyAxiom,[ontology, axiom]).\n\
valid_axiom(ontologyAxiom(A, B)) :- subsumed_by([A, B],[ontology, axiom]).\n\
\n\
%% ontologyImport(?Ontology, ?IRI)\n\
% True of Ontology imports document IRI\n\
:- dynamic(ontologyImport/2).\n\
\n\
axiompred(ontologyImport/2).\n\
axiom_arguments(ontologyImport,[ontology, iri]).\n\
valid_axiom(ontologyImport(A, B)) :- subsumed_by([A, B],[ontology, iri]).\n\
\n\
%% ontologyVersionInfo(?Ontology, ?IRI)\n\
:- dynamic(ontologyVersionInfo/2).\n\
\n\
axiompred(ontologyVersionInfo/2).\n\
axiom_arguments(ontologyVersionInfo,[ontology, iri]).\n\
valid_axiom(ontologyVersionInfo(A, B)) :- subsumed_by([A, B],[ontology, iri]).\n\
\n\
/****************************************\n\
  RESTRICTIONS ON AXIOMS\n\
  ****************************************/\n\
\n\
% 11.1\n\
% An object property expression OPE is simple in Ax if, for each object property expression OPE' such that OPE' ->* OPE holds, OPE' is not composite.\n\
% (The property hierarchy relation ->* is the reflexive-transitive closure of ->)\n\
%simpleObjectPropertyExpresion(OPE) :-\n\
%        objectPropertyExpression(OPE),\n\
\n\
\n\
/****************************************\n\
  EXPRESSIONS\n\
  ****************************************/\n\
\n\
subsumed_by(X,_) :- var(X),!.\n\
subsumed_by([],[]) :- !.\n\
subsumed_by([I|IL],[T|TL]) :-\n\
	!,\n\
	subsumed_by(I,T),\n\
	subsumed_by(IL,TL).\n\
subsumed_by(L,set(T)):-\n\
        !,\n\
        forall(member(I,L),\n\
               subsumed_by(I,T)).\n\
subsumed_by(I,T):-\n\
        !,\n\
	G=..[T,I],\n\
	G.\n\
\n\
\n\
%% iri(?IRI)\n\
% true if IRI is an IRI. TODO: currently underconstrained, any atomic term can be an IRI\n\
iri(IRI) :- atomic(IRI).	%\n\
\n\
%% literal(?Lit)\n\
% true if Lit is an rdf literal\n\
%literal(_).			% TODO\n\
literal(literal(_)).			% TODO\n\
\n\
propertyExpression(E) :- objectPropertyExpression(E) ; dataPropertyExpression(E).\n\
\n\
%% objectPropertyExpression(?OPE)\n\
% true if OPE is an ObjectPropertyExpression\n\
% ObjectPropertyExpression := ObjectProperty | InverseObjectProperty\n\
objectPropertyExpression(E) :- objectProperty(E) ; inverseObjectProperty(E).\n\
\n\
% give benefit of doubt; e.g. rdfs:label\n\
% in the OWL2 spec we have DataProperty := IRI\n\
% here dataProperty/1 is an asserted fact\n\
objectPropertyExpression(E) :- nonvar(E),iri(E).\n\
\n\
objectPropertyExpressionOrChain(propertyChain(PL)) :- forall(member(P,PL),objectPropertyExpression(P)).\n\
objectPropertyExpressionOrChain(PE) :- objectPropertyExpression(PE).\n\
\n\
\n\
inverseObjectProperty(inverseOf(OP)) :- objectProperty(OP).\n\
\n\
dataPropertyExpression(E) :- dataProperty(E).\n\
\n\
dataPropertyExpression(DPEs) :-\n\
	(   is_list(DPEs)\n\
	->  forall(member(DPE,DPEs),\n\
		   dataPropertyExpression(DPE))\n\
	;   dataPropertyExpression(DPEs)).\n\
\n\
% give benefit of doubt; e.g. rdfs:label\n\
% in the OWL2 spec we have DataProperty := IRI\n\
% here dataProperty/1 is an asserted fact\n\
dataPropertyExpression(E) :- nonvar(E),iri(E).\n\
\n\
%already declared as entity\n\
%datatype(IRI) :- iri(IRI).\n\
\n\
%% dataRange(+DR) is semidet\n\
dataRange(DR) :-\n\
    datatype(DR) ;\n\
    dataIntersectionOf(DR );\n\
    dataUnionOf(DR) ;\n\
    dataComplementOf(DR) ;\n\
    dataOneOf(DR) ;\n\
    datatypeRestriction(DR).\n\
\n\
%% classExpression(+CE) is semidet\n\
%\n\
% true if CE is a class expression term, as defined in OWL2\n\
%\n\
% Example: =|classExpression(intersectionOf([car,someValuesFrom(hasColor,blue)])))|=\n\
%\n\
% Union of:\n\
%\n\
%    class/1 | objectIntersectionOf/1 | objectUnionOf/1 |\n\
%    objectComplementOf/1 | objectOneOf/1 | objectSomeValuesFrom/1 |\n\
%    objectAllValuesFrom/1 | objectHasValue/1 | objectHasSelf/1 |\n\
%    objectMinCardinality/1 | objectMaxCardinality/1 |\n\
%    objectExactCardinality/1 | dataSomeValuesFrom/1 |\n\
%    dataAllValuesFrom/1 | dataHasValue/1 | dataMinCardinality/1 |\n\
%    dataMaxCardinality/1 | dataExactCardinality/1\n\
classExpression(CE):-\n\
        iri(CE) ;               % NOTE: added to allow cases where class is not imported\n\
    class(CE) ;\n\
    objectIntersectionOf(CE) ; objectUnionOf(CE) ; objectComplementOf(CE) ; objectOneOf(CE) ;\n\
    objectSomeValuesFrom(CE) ; objectAllValuesFrom(CE) ; objectHasValue(CE) ; objectHasSelf(CE) ;\n\
    objectMinCardinality(CE) ; objectMaxCardinality(CE) ; objectExactCardinality(CE) ;\n\
    dataSomeValuesFrom(CE) ; dataAllValuesFrom(CE) ; dataHasValue(CE) ;\n\
    dataMinCardinality(CE) ; dataMaxCardinality(CE) ; dataExactCardinality(CE).\n\
\n\
%% objectIntersectionOf(+CE) is semidet\n\
% true if CE is a term intersectionOf(ClassExpression:list)\n\
%\n\
% An intersection class expression IntersectionOf( CE1 ... CEn ) contains all individuals that are instances of all class expressions CEi for 1 <= i <= n.\n\
objectIntersectionOf(intersectionOf(CEs)) :-\n\
	forall(member(CE,CEs),\n\
	       classExpression(CE)).\n\
\n\
%% objectUnionOf(+CE) is semidet\n\
% A union class expression UnionOf( CE1 ... CEn ) contains all individuals that are instances of at least one class expression CEi for 1 <= i <= n\n\
objectUnionOf(unionOf(CEs)) :-\n\
	forall(member(CE,CEs),\n\
	       classExpression(CE)).\n\
\n\
%% objectComplementOf(+CE) is semidet\n\
%\n\
objectComplementOf(complementOf(CE)) :-\n\
	classExpression(CE).\n\
\n\
%% objectOneOf(+CE) is semidet\n\
% An enumeration of individuals OneOf( a1 ... an ) contains exactly the individuals ai with 1 <= i <= n.\n\
objectOneOf(oneOf(Is)) :-\n\
        is_list(Is). % TODO: check if we need to strengthen this check\n\
%objectOneOf(oneOf(Is)) :-\n\
%	forall(member(I,Is),\n\
%	       individual(I)).\n\
\n\
%% objectSomeValuesFrom(+R) is semidet\n\
% An existential class expression SomeValuesFrom( OPE CE ) consists of an object property expression OPE and a class expression CE, and it contains all those individuals that are connected by OPE to an individual that is an instance of CE\n\
objectSomeValuesFrom(someValuesFrom(OPE,CE)) :-\n\
	objectPropertyExpression(OPE),\n\
	classExpression(CE).\n\
\n\
%% objectAllValuesFrom(+R) is semidet\n\
% A universal class expression AllValuesFrom( OPE CE ) consists of an object property expression OPE and a class expression CE, and it contains all those individuals that are connected by OPE only to individuals that are instances of CE\n\
objectAllValuesFrom(allValuesFrom(OPE,CE)) :-\n\
	objectPropertyExpression(OPE),\n\
	classExpression(CE).\n\
\n\
%% objectHasValue(+R) is semidet\n\
% A has-value class expression HasValue( OPE a ) consists of an object property expression OPE and an individual a, and it contains all those individuals that are connected by OPE to a\n\
objectHasValue(hasValue(OPE,I)) :-\n\
	objectPropertyExpression(OPE),\n\
	individual(I).\n\
\n\
%% objectHasSelf(+R) is semidet\n\
% A self-restriction HasSelf( OPE ) consists of an object property expression OPE, and it contains all those individuals that are connected by OPE to themselves\n\
objectHasSelf(hasSelf(OPE)) :-\n\
	objectPropertyExpression(OPE).\n\
\n\
%% objectMinCardinality(+CR) is semidet\n\
% A minimum cardinality expression MinCardinality( n OPE CE ) consists of a nonnegative integer n, an object property expression OPE, and a class expression CE, and it contains all those individuals that are connected by OPE to at least n different individuals that are instances of CE. If CE is missing, it is taken to be owl:Thing\n\
objectMinCardinality(minCardinality(C,OPE,CE)):-\n\
	number(C),\n\
	C>=0,\n\
	objectPropertyExpression(OPE),\n\
	classExpression(CE).\n\
objectMinCardinality(minCardinality(C,OPE)):-\n\
	number(C),\n\
	C>=0,\n\
	objectPropertyExpression(OPE).\n\
\n\
\n\
%% objectMaxCardinality(+CR) is semidet\n\
% A maximum cardinality expression MaxCardinality( n OPE CE ) consists of a nonnegative integer n, an object property expression OPE, and a class expression CE, and it contains all those individuals that are connected by OPE to at most n different individuals that are instances of CE. If CE is missing, it is taken to be owl:Thing\n\
objectMaxCardinality(maxCardinality(C,OPE,CE)):-\n\
	number(C),\n\
	C>=0,\n\
	objectPropertyExpression(OPE),\n\
	classExpression(CE).\n\
objectMaxCardinality(maxCardinality(C,OPE)):-\n\
	number(C),\n\
	C>=0,\n\
	objectPropertyExpression(OPE).\n\
\n\
%% objectExactCardinality(+CR) is semidet\n\
% An exact cardinality expression ExactCardinality( n OPE CE ) consists of a nonnegative integer n, an object property expression OPE, and a class expression CE, and it contains all those individuals that are connected by OPE to exactly n different individuals that are instances of CE. If CE is missing, it is taken to be owl:Thing\n\
objectExactCardinality(exactCardinality(C,OPE,CE)):-\n\
	number(C),\n\
	C>=0,\n\
	objectPropertyExpression(OPE),\n\
	classExpression(CE).\n\
objectExactCardinality(exactCardinality(C,OPE)):-\n\
	number(C),\n\
	C>=0,\n\
	objectPropertyExpression(OPE).\n\
% NON-NORMATIVE: we accept this in order to maximize compatibility with Thea1\n\
objectExactCardinality(cardinality(C,OPE)):-\n\
	number(C),\n\
	C>=0,\n\
	objectPropertyExpression(OPE).\n\
\n\
\n\
%% dataIntersectionOf(+DR:dataIntersectionOf) is semidet\n\
% An intersection data range IntersectionOf( DR1 ... DRn ) contains all data values that are contained in the value space of every data range DRi for 1 <= i <= n. All data ranges DRi must be of the same arity\n\
dataIntersectionOf(intersectionOf(DRs)) :-\n\
	forall(member(DR,DRs),\n\
	       dataRange(DR)).\n\
\n\
%% dataUnionOf(+DR:dataUnionOf) is semidet\n\
% A union data range UnionOf( DR1 ... DRn ) contains all data values that are contained in the value space of at least one data range DRi for 1 <= i <= n. All data ranges DRi must be of the same arity\n\
dataUnionOf(unionOf(DRs)) :-\n\
	forall(member(DR,DRs),\n\
	       dataRange(DR)).\n\
\n\
%% dataComplementOf(+DR:dataComplementOf) is semidet\n\
% A complement data range ComplementOf( DR ) contains all literals that are not contained in the data range DR\n\
dataComplementOf(complementOf(DR)) :-\n\
	dataRange(DR).\n\
\n\
%% dataOneOf(+DR:dataOneOf) is semidet\n\
% An enumeration of literals OneOf( lt1 ... ltn ) contains exactly the explicitly specified literals lti with 1 <= i <= n\n\
dataOneOf(oneOf(DRs)) :-\n\
	forall(member(DR,DRs),\n\
	       dataRange(DR)).\n\
\n\
%% datatypeRestriction(+DR) is semidet\n\
%\n\
% TODO: multiple args\n\
datatypeRestriction(datatypeRestriction(DR,FacetValues)):-\n\
	datatype(DR),\n\
	FacetValues=[_|_].\n\
\n\
%% dataSomeValuesFrom(+DR) is semidet\n\
dataSomeValuesFrom(someValuesFrom(DPE,DR)):-\n\
	dataPropertyExpression(DPE),\n\
	dataRange(DR).\n\
\n\
%% dataAllValuesFrom(+DR) is semidet\n\
dataAllValuesFrom(allValuesFrom(DPE,DR)):-\n\
	dataPropertyExpression(DPE),\n\
	dataRange(DR).\n\
\n\
%% dataHasValue(+DR) is semidet\n\
% A has-value class expression HasValue( DPE lt ) consists of a data property expression DPE and a literal lt, and it contains all those individuals that are connected by DPE to lt. Each such class expression can be seen as a syntactic shortcut for the class expression SomeValuesFrom( DPE OneOf( lt ) )\n\
dataHasValue(hasValue(DPE,L)):-\n\
	dataPropertyExpression(DPE),\n\
	literal(L).\n\
\n\
%% dataMinCardinality(+DR) is semidet\n\
% A minimum cardinality expression MinCardinality( n DPE DR ) consists of a nonnegative integer n, a data property expression DPE, and a unary data range DR, and it contains all those individuals that are connected by DPE to at least n different literals in DR. If DR is not present, it is taken to be rdfs:Literal\n\
dataMinCardinality(minCardinality(C,DPE,DR)):-\n\
	number(C),\n\
	C>=0,\n\
	dataPropertyExpression(DPE),\n\
	dataRange(DR).\n\
dataMinCardinality(minCardinality(C,DPE)):-\n\
	number(C),\n\
	C>=0,\n\
	dataPropertyExpression(DPE).\n\
\n\
\n\
\n\
%% dataMaxCardinality(+DR) is semidet\n\
% A maximum cardinality expression MaxCardinality( n DPE DR ) consists of a nonnegative integer n, a data property expression DPE, and a unary data range DR, and it contains all those individuals that are connected by DPE to at most n different literals in DR. If DR is not present, it is taken to be rdfs:Literal.\n\
dataMaxCardinality(maxCardinality(C,DPE,DR)):-\n\
	number(C),\n\
	C>=0,\n\
	dataPropertyExpression(DPE),\n\
	dataRange(DR).\n\
dataMaxCardinality(maxCardinality(C,DPE)):-\n\
	number(C),\n\
	C>=0,\n\
	dataPropertyExpression(DPE).\n\
\n\
\n\
%% dataExactCardinality(+DR) is semidet\n\
% An exact cardinality expression ExactCardinality( n DPE DR ) consists of a nonnegative integer n, a data property expression DPE, and a unary data range DR, and it contains all those individuals that are connected by DPE to exactly n different literals in DR. If DR is not present, it is taken to be rdfs:Literal\n\
dataExactCardinality(exactCardinality(C,DPE,DR)):-\n\
	number(C),\n\
	C>=0,\n\
	dataPropertyExpression(DPE),\n\
	dataRange(DR).\n\
dataExactCardinality(exactCardinality(C,DPE)):-\n\
	number(C),\n\
	C>=0,\n\
	dataPropertyExpression(DPE).\n\
% NON-NORMATIVE: we accept this in order to maximize compatibility with Thea1\n\
dataExactCardinality(cardinality(C,OPE)):-\n\
	number(C),\n\
	C>=0,\n\
	objectPropertyExpression(OPE).\n\
\n\
\n\
%% valid_axiom(?Axiom) is nondet\n\
% true if Axiom passes typechecking\n\
\n\
\n\
%% is_valid_axiom(?Axiom) is semidet\n\
% true if Axiom passes typechecking\n\
is_valid_axiom(Axiom) :- \\+ \\+ valid_axiom(Axiom).\n\
\n\
\n\
/****************************************\n\
  VIEW PREDICATES\n\
  ****************************************/\n\
\n\
%% equivalent_to(?X,?Y)\n\
% note: this is currently slow for bound values of X and Y\n\
equivalent_to(X,Y) :- equivalentClasses(L),member(X,L),member(Y,L),X\\=Y.\n\
equivalent_to(X,Y) :- equivalentProperties(L),member(X,L),member(Y,L),X\\=Y.\n\
\n\
disjoint_with(X,Y) :- disjointClasses(L),member(X,L),member(Y,L),X\\=Y.\n\
\n\
%% anyPropertyAssertion(?Property,?Entity,?Value)\n\
% subsumes propertyAssertion/3 and annotationAssertion/3\n\
anyPropertyAssertion(P,E,V) :- propertyAssertion(P,E,V).\n\
anyPropertyAssertion(P,E,V) :- annotationAssertion(P,E,V).\n\
\n\
\n\
%% labelAnnotation_value(?X,?Val)\n\
labelAnnotation_value(X,Val) :-\n\
        anyPropertyAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(type(_,Val))),atom(Val).\n\
labelAnnotation_value(X,Val) :-\n\
        anyPropertyAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(lang(_,Val))),atom(Val).\n\
labelAnnotation_value(X,Val) :-\n\
        anyPropertyAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(Val)),atom(Val).\n\
\n\
/****************************************\n\
  META-PREDICATES\n\
  ****************************************/\n\
\n\
\n\
%% axiom_directly_about(?Ax,?About)\n\
% true if Ax is an axiom whose first argument is equal to About.\n\
%\n\
% e.g. axiom_directly_about( subClassOf(X,_), X).\n\
%\n\
% also include property assertions whose second argument is equal to About.\n\
%\n\
% e.g. axiom_directly_about( propertyAssertion(P,X,_), X).\n\
%\n\
axiom_directly_about(Ax,About) :-\n\
        axiom(Ax),\n\
        Ax =.. [_,Arg1|_],\n\
        (   is_list(Arg1)\n\
        ->  member(About,Arg1)\n\
        ;   About=Arg1).\n\
axiom_directly_about(Ax,About) :-\n\
	Ax=propertyAssertion(_,About,_),\n\
        axiom(Ax).\n\
axiom_directly_about(Ax,About) :-\n\
	Ax=annotationAssertion(_,About,_),\n\
        axiom(Ax).\n\
axiom_directly_about(Ax,About) :-\n\
	Ax=classAssertion(_,About),\n\
        axiom(Ax).\n\
\n\
\n\
%% axiom_directly_references(?Ax:axiom,?Ref)\n\
%\n\
% Ref may be\n\
%  - an axiom\n\
%  - a named entity\n\
%  - an expression\n\
axiom_directly_references(Ax,Ref) :-\n\
        axiom(Ax),\n\
        axiom_or_expression_references(Ax,Ref).\n\
\n\
axiom_or_expression_references(X,Ref) :-\n\
        X =.. [P|Args],\n\
        P\\=literal,\n\
        member(Arg,Args),\n\
        (   is_list(Arg)\n\
        ->  member(Ref,Arg)\n\
        ;   Ref=Arg).\n\
\n\
axiom_about(Ax,About) :-\n\
        axiom_directly_about(Ax,About).\n\
axiom_about(Ax,About) :-\n\
        axiom_directly_about(Ax,X),\n\
        axiom_about(X,About).\n\
\n\
axiom_references(Ax,Ref) :-\n\
        axiom_directly_references(Ax,Ref).\n\
axiom_references(Ax,Ref) :-\n\
        axiom_directly_references(Ax,X),\n\
        axiom_or_expression_references(X,Ref).\n\
\n\
axiom_contains_expression(Ax,Ex) :-\n\
        axiom_contains_expression(Ax,Ex,_).\n\
axiom_contains_expression(Ax,Ex,D) :-\n\
        axiom(Ax),\n\
        expression_has_subexpression(Ax,Ex,[],Chain),\n\
        length(Chain,D).\n\
\n\
expression_has_subexpression(Ex,Ex,Accum,Accum).\n\
expression_has_subexpression(Ex,SubEx,Accum,Results) :-\n\
        Ex =.. [F|Args],\n\
        member(A,Args),\n\
        expression_has_subexpression(A,SubEx,[F|Accum],Results).\n\
\n\
\n\
\n\
%% referenced_description(?Desc) is nondet\n\
% true if Desc is either a class or a class expression using the set of ontologies loaded.\n\
% Example: if the ontology contains\n\
% ==\n\
% subClassOf(a,intersectionOf([b,someValuesFrom(p,c)]))\n\
% ==\n\
% then Desc will be a member of [a, b, c, b and p some c, p some c]\n\
referenced_description(C) :-\n\
        setof(C,referenced_description_1(C),Cs),\n\
        member(C,Cs).\n\
\n\
referenced_description_1(C) :- class(C).\n\
referenced_description_1(C) :-\n\
        subClassOf(A,B),\n\
        (   referenced_description(A,C)\n\
        ;   referenced_description(B,C)).\n\
referenced_description_1(C) :-\n\
        equivalentClasses(L),\n\
        member(A,L),\n\
        referenced_description(A,C).\n\
referenced_description_1(C) :-\n\
        classAssertion(A,_),\n\
        referenced_description(A,C).\n\
\n\
% TODO - this is incomplete\n\
referenced_description(X,X) :- ground(X).\n\
referenced_description(someValuesFrom(_,X),Y) :- referenced_description(X,Y).\n\
referenced_description(allValuesFrom(_,X),Y) :- referenced_description(X,Y).\n\
referenced_description(intersectionOf(L),Y) :- member(X,L),referenced_description(X,Y).\n\
referenced_description(unionOf(L),Y) :- member(X,L),referenced_description(X,Y).\n\
\n\
\n\
/****************************************\n\
  UTILITY\n\
  ****************************************/\n\
\n\
\n\
:- dynamic assert_axiom_hook/1.\n\
\n\
%% assert_axiom(+Axiom:axiom)\n\
%\n\
% writes an axiom to the prolog database.\n\
% typically this will just be a matter of calling assert/1. However, in future we\n\
% will have different backing stores (rdf_db, sql), and in these cases calls to\n\
% this predicate will perform the appropriate actions.\n\
%\n\
% this also asserts ontologyAxiom/2, using nb_getval with current_ontology\n\
assert_axiom(Axiom) :-\n\
        assert_axiom_hook(Axiom),\n\
        !.\n\
assert_axiom(Axiom) :-\n\
        assert(Axiom),\n\
	(   nb_current(current_ontology,O)\n\
        ->  assert(ontologyAxiom(O,Axiom))\n\
        ;   true),\n\
        !.\n\
  \n\
%% assert_axiom(+Axiom:axiom,+Ontology:ontology) is det\n\
%\n\
% as assert_axiom/1, but also asserts to ontologyAxiom/2\n\
assert_axiom(Axiom,O) :-\n\
        assert(Axiom),\n\
	assert(ontologyAxiom(O,Axiom)),\n\
        !.\n\
\n\
\n\
:- dynamic retract_axiom_hook/1.\n\
\n\
%% retract_axiom(+Axiom:axiom)\n\
%\n\
% removes an axiom from the prolog database.\n\
% typically this will just be a matter of calling retract/1. However, in future we\n\
% will have different backing stores (rdf_db, sql), and in these cases calls to\n\
% this predicate will perform the appropriate actions.\n\
%\n\
% also removes ontologyAxiom/2 from ALL ontologies\n\
retract_axiom(Axiom) :-\n\
        retract_axiom_hook(Axiom),\n\
        !.\n\
retract_axiom(Axiom) :-\n\
        retractall(Axiom),\n\
	retractall(ontologyAxiom(_,Axiom)),\n\
        !.\n\
\n\
%% retract_axiom(+Axiom:axiom,+Ontology)\n\
% retracts axioms from a specified ontology\n\
retract_axiom(Axiom,Ontology) :-\n\
        \\+ var(Ontology),\n\
	retractall(ontologyAxiom(Ontology,Axiom)),\n\
        (   \\+ ontologyAxiom(_,Axiom)\n\
        ->  retractall(Axiom)\n\
        ;   true),              % still exists in other ontology..\n\
        !.\n\
\n\
\n\
retract_all_axioms :-\n\
        findall(A,axiom(A),Axioms),\n\
        maplist(retract,Axioms),\n\
        findall(ontologyAxiom(O,A),ontologyAxiom(O,A),OAxioms),\n\
        maplist(retract,OAxioms),\n\
	!.\n\
\n\
\n\
owl2_model_init :-\n\
	assert(annotationProperty('http://www.w3.org/2000/01/rdf-schema#label')),\n\
	assert(annotationProperty('http://www.w3.org/2000/01/rdf-schema#comment')).\n\
\n\
consult_axioms(File) :-\n\
        consult(File).\n\
\n\
axiom_type(A,T) :- functor(A,T,_).\n\
\n\
/** <module> Ontology language axioms and expressions\n\
\n\
---+ Synopsis\n\
\n\
    Example OWL2 ontology as a prolog database:\n\
==\n\
class(organism).\n\
class(animal).\n\
class(carnivore).\n\
class(herbivore).\n\
objectProperty(eats).\n\
subClassOf(animal,organism).\n\
equivalentClasses([carnivore,intersectionOf([animal,someValuesFrom(eats,animal)])]).\n\
disjointClasses([herbivore,carnivore]).\n\
==\n\
\n\
Example of use:\n\
\n\
==\n\
:- use_module(library(thea2/owl2_io)).\n\
:- use_module(library(thea2/owl2_model)).\n\
\n\
show_superclasses(OntFile,Class) :-\n\
        load_axioms(OntFile),\n\
        forall(subClassOf(Class,Super),\n\
               writeln(Super)).\n\
==\n\
\n\
---+ Details\n\
\n\
This module is a prolog model of the OWL2 language. It consists of predicates for both OWL2 axioms and expressions.\n\
\n\
This model is intended to closely parallel Structural Specification and Functional-Style Syntax for OWL2 (http://www.w3.org/TR/owl2-syntax).\n\
\n\
* Axioms and Declarations are modeled as prolog predicates (e.g. SubClassOf --> subClassOf/2)\n\
* Class and Property Expressions are modeled as prolog terms. These can be checked via semi-deterministic predicates (e.g. objectIntersectionOf/1)\n\
* Axiom Annotations are modeled as prolog predicates taking a reified axiom clause head as an argument (axiomAnnotation/3)\n\
* The names should correspond exactly, with the leading uppercase character substituted for a lowercase (to avoid quoting in prolog) - the one exception is Import, which maps to the prolog predicate ontologyImport/2 (to avoid confusion with prolog import/1)\n\
* Axioms with variable arguments are modeled as prolog predicates that take prolog lists as arguments (e.g. equivalentClasses/1)\n\
* For programmatic convenience we provide additional abstract predicates that do not necessarily correspond to the OWL syntax (e.g. property/1,fact/1)\n\
\n\
\n\
---++ Axioms\n\
\n\
Extensional predicates are declared for all terminal axiom symbols in the functional syntax;  i.e. subPropertyOf/2, subClassOf/2.\n\
These can be directly asserted, or compiled from a prolog file.\n\
\n\
The terms from the OWL2 structural syntax are taken as primitive,\n\
i.e. they are extensional / unit-clauses, and designed to be asserted\n\
or compiled.\n\
\n\
Some predicates such as property/1 are intensional - these generalize\n\
over the OWL2 axioms are defined by prolog rules, and should not be\n\
asserted.  In this case property/1 is defined as annotationProperty/1\n\
or dataProperty/1 or objectProperty/1.\n\
\n\
For the strong typed model, we also provide intensional predicates\n\
such as subObjectPropertyOf/2 - satisfaction of this predicate is\n\
determined at runtime based on type-checking, if subPropertyOf/2 holds.\n\
\n\
\n\
---++ Expressions and Type checking\n\
\n\
OWL Axioms can take either entities or expressions as arguments.\n\
 Entities are simply prolog atoms corresponding to the IRI.\n\
 Expressions are prolog terms;\n\
 e.g. =|intersectionOf(a,someValuesFrom(inverseOf(p),b))|=\n\
\n\
 (Class expressions are also known as Descriptions)\n\
\n\
 Optional run-time checking of predicates using valid_axiom/1.\n\
\n\
 For example =|subClassOf(intersectionOf(c1,c2),unionOf(c3,c4))|= is\n\
 valid if c1,c2,c3,c4 are all class expressions, but\n\
 =|subClassOf(p1,c1)|= is not valid if p1 is a property\n\
\n\
 We can also make checks for specific types: e.g objectIntersectionOf/1\n\
\n\
---++ Annotations\n\
\n\
  In OWL Syntax, axiom annotations are written using an optional annotation list argument.\n\
  We opt not to do this here; instead we use axiomAnnotation/3 where the first argument is the reified predicate head.\n\
  E.g.\n\
\n\
==\n\
subClassOf(cat,mammal).\n\
axiomAnnotation(SubClassOf(cat,mammal),author,linnaeus).\n\
==\n\
\n\
\n\
  ---++ Ontologies\n\
\n\
  We use a similar scheme for annotations:\n\
\n\
==\n\
subClassOf(cat,mammal).\n\
ontologyAxiom(linnaenTaxonomy,SubClassOf(cat,mammal)).\n\
ontology(linnaenTaxonomy).\n\
==\n\
\n\
TODO: check edge cases, eg two ontologies have the same axioms but different annotations\n\
\n\
---++ IRIs\n\
\n\
By default there is no type checking of IRIs, so =|class(polarBear)|=\n\
is allowed, even though polarBear is not an IRI - this makes for\n\
convenience in working with example ontologies.\n\
\n\
See prefix_IRIs/1 in owl2_util.pl for converting between short names\n\
and valid IRIs.\n\
\n\
---+ Open Issues\n\
\n\
---++ Enumeration of expressions\n\
\n\
We provide semi-deterministic predicates of the form\n\
  ?type(+Expression).  Should the mode be extended to allow\n\
  enumeration of all descriptions/expressions? This would probably\n\
  require forcing all expressions to be bnodes OR possibly recursively\n\
  analyzing the term Axiom in a call axiom(Axiom)\n\
\n\
---++ Type checking\n\
\n\
  Is Tom Schrijvers type checking system going to be integrated into SWI and Yap? Should we use that?\n\
\n\
  I am attempting to put as much typing info in the pldoc comments,\n\
  but unsure of the conventions for complex terms.\n\
\n\
  LATEST: see owl2_metamodel.pl\n\
\n\
---++ Ontologies\n\
\n\
  continue using ontologyAxiom/2? Alternatively use builtin prolog module mechanism..?\n\
\n\
---+ See Also\n\
\n\
* owl2_from_rdf.pl\n\
* swrl.pl\n\
\n\
---+ Additional Information\n\
\n\
@see     README\n\
@license License\n\
\n\
*/\n\
\n\
:- use_module(library(debug)).\n\
:- use_module(library('semweb/rdf_db')).\n\
:- use_module(library('semweb/rdf_edit')).\n\
:- use_module(library('semweb/rdfs')).\n\
:- use_module(library('url')).\n\
:- use_module(library('http/http_open')).\n\
:- use_module(library(charsio)).\n\
\n\
:- dynamic(owl/4).\n\
%% blanknode(Node,Description,Used)\n\
% see owl_get_bnode/2\n\
% Node - bNodeId\n\
% Description - prolog term corresponding to owl Description\n\
% Used - used | shared\n\
:- dynamic(blanknode/3).\n\
:- dynamic(outstream/1).\n\
\n\
:- dynamic(aNN/3). % implements the ANN(X) function.\n\
:- dynamic(annotation_r_node/4).  % annotation_r_node(S,P,O,Node)\n\
:- dynamic(axiom_r_node/4).       % axiom_r_node(S,P,O,Node)\n\
:- dynamic(owl_repository/2). % implements a simple OWL repository: if URL not found, Ontology is read from a repository (local) RURL\n\
\n\
\n\
% we make this discontiguous so that the code can follow the structure of the document as much as possible\n\
\n\
:- discontiguous owl_parse_axiom/3.\n\
:- discontiguous dothislater/1.\n\
\n\
% hookable\n\
\n\
\n\
% -----------------------------------------------------------------------\n\
%                                UTILITY Predicates\n\
% -----------------------------------------------------------------------\n\
\n\
\n\
%%       owl_clear_as\n\
%\n\
%       Clears the prolog terms that store the Abstract Syntax\n\
%       implementation of the OWL ontology.\n\
\n\
owl_clear_as :-\n\
        debug(owl_parser,'Clearing abstract syntax',[]),\n\
        forall((axiompred(PredSpec),predspec_head(PredSpec,Head)),\n\
               retractall(Head)).\n\
\n\
predspec_head(Pred/A,Head) :- functor(Head,Pred,A).\n\
\n\
u_assert(Term) :-\n\
	call(Term), !; assert(Term).\n\
\n\
\n\
convert(T,V,typed_value(T,V)).\n\
\n\
\n\
%%	rdf_2_owl(+Base, +Ont) is det\n\
%\n\
%       Converts RDF triples to OWL/4 triples so that\n\
%	their use can tracked by the OWL parser.\n\
\n\
\n\
rdf_2_owl(_Base,Ont) :-\n\
%	debug(owl_parser, 'Removing existing owl triples',[]),\n\
%	retractall(owl(_,_,_,Ont)),\n\
%  pengine_self(Self),\n\
%  pengine_property(Self,module(M)),\n\
	debug(owl_parser,'Copying RDF triples to OWL triples for Ontology ~w',[Ont]),\n\
	myrdf(X,Y,Z),\n\
%	owl_fix_no(X,X1), owl_fix_no(Y,Y1), owl_fix_no(Z,Z1),\n\
	assert(owl(X,Y,Z,Ont)), fail.\n\
\n\
rdf_2_owl(_,Ont) :-\n\
	owl_count(Ont,Z),\n\
	debug(owl_parser,'Number of owl triples copied: ~w',[Z]).\n\
\n\
\n\
%%	owl_count(?U).\n\
%       Returns/Checks the number of unused OWL triples.\n\
\n\
owl_count(O,U) :-\n\
	findall(1,owl(_,_,_,O),X), length(X,U).\n\
\n\
%% expand_and_assert(S,P,O) is det\n\
%\n\
% adds a owl(S,P,O,not_used) after expanding namespaces.\n\
% this is required for the triple replacement rules,\n\
% which use shortened rdfs/owl namespaces.\n\
% (or we could just use the expanded forms here which\n\
%  may be faster..)\n\
expand_and_assert(X1,Y1,Z1) :-\n\
	expand_ns(X1,X),\n\
	expand_ns(Y1,Y),\n\
	expand_ns(Z1,Z),!,\n\
	retractall(owl(X,Y,Z, used1)),\n\
	assert(owl(X,Y,Z, not_used)).\n\
\n\
\n\
%%       test_use_owl(+Triples:list) is nondet\n\
%\n\
%       As use_owl/1, but does not consume the triple.  If owl(S,P,O)\n\
%       in Triples has a non-ground variable then this will succeed\n\
%       non-deterministically.  If all variables are ground, then this\n\
%       will succeed semi-deterministically.\n\
test_use_owl([]).\n\
test_use_owl([owl(S,P,O)|Rest]) :-\n\
	test_use_owl(S,P,O),\n\
	test_use_owl(Rest).\n\
\n\
\n\
%%       test_use_owl(?S,?P,?O)\n\
%	As use_owl/3, but does not consume the triple. Expands the S,P,O.\n\
%\n\
%       If any of S, P or O is non-ground then this will succeed\n\
%       non-deterministically.  If all variables are ground, then this\n\
%       will succeed semi-deterministically.\n\
test_use_owl(X1,Y1,Z1) :-\n\
	expand_ns(X1,X),\n\
	expand_ns(Y1,Y),\n\
	expand_ns(Z1,Z),!,\n\
	owl(X,Y,Z, not_used).\n\
\n\
test_use_owl(X1,Y1,Z1,named) :-\n\
	expand_ns(X1,X),\n\
	expand_ns(Y1,Y),\n\
	expand_ns(Z1,Z),\n\
	owl(X,Y,Z, not_used),\n\
	\\+ sub_string(X,0,2,_,'__').\n\
\n\
\n\
%%       use_owl(+Triples:list)\n\
%	Marks a list of OWL triples as used, but only if all match. Expands the S,P,O.\n\
\n\
use_owl(Triples) :-\n\
        test_use_owl(Triples),\n\
        use_owl_2(Triples).\n\
\n\
% consume all triples; we have already tested the list and know that all match\n\
use_owl_2([]).\n\
use_owl_2([owl(S,P,O)|Triples]) :-\n\
        use_owl(S,P,O),\n\
        use_owl_2(Triples).\n\
\n\
\n\
use_owl(X1,Y1,Z1) :-\n\
	expand_ns(X1,X),\n\
	expand_ns(Y1,Y),\n\
	expand_ns(Z1,Z),\n\
	owl(X,Y,Z, not_used),\n\
	debug(owl_parser_detail,'using ~w ~w ~w',[X,Y,Z]),\n\
	retract(owl(X,Y,Z, not_used)),\n\
	assert(owl(X,Y,Z,used1)).\n\
\n\
use_owl(X1,Y1,Z1,named) :-\n\
	expand_ns(X1,X),\n\
	expand_ns(Y1,Y),\n\
	expand_ns(Z1,Z),\n\
	owl(X,Y,Z, not_used),\n\
	\\+ sub_string(X,0,2,_,'__'),\n\
	retract(owl(X,Y,Z, not_used)),\n\
	assert(owl(X,Y,Z,used2)).\n\
\n\
use_owl(X1,Y1,Z1,Term) :-\n\
	expand_ns(X1,X),\n\
	expand_ns(Y1,Y),\n\
	expand_ns(Z1,Z),\n\
	owl(X,Y,Z, not_used),\n\
	debug(owl_parser_detail,'using ~w ~w ~w',[X,Y,Z]),\n\
	retract(owl(X,Y,Z, not_used)),\n\
	assert(owl(X,Y,Z,used(Term))).\n\
\n\
\n\
%%	use_owl(?S,?P,?O,+Named,Term)\n\
%\n\
%       Named = named: Same as use_owl/3, but marks only if S 	is Named URI (i.e. non blank node).\n\
\n\
use_owl(X1,Y1,Z1,named,Term) :-\n\
	expand_ns(X1,X),\n\
	expand_ns(Y1,Y),\n\
	expand_ns(Z1,Z),\n\
	owl(X,Y,Z, not_used),\n\
	\\+ sub_string(X,0,2,_,'__'),\n\
	retract(owl(X,Y,Z, not_used)),\n\
	assert(owl(X,Y,Z,used(Term))).\n\
\n\
\n\
%%       expand_ns(+NS_URL, ?Full_URL)\n\
%\n\
%       Expands a 'namespaced' URI of the form ns:fragment to a full URI\n\
%       substituting the full expansion for ns from the ns/2 facts\n\
expand_ns(NS_URL, Full_URL) :-\n\
	nonvar(NS_URL),\n\
	NS_URL \\= literal(_),\n\
	uri_split(NS_URL,Short_NS,Term, ':'),\n\
	rdf_db:ns(Short_NS,Long_NS),!,\n\
	concat_atom([Long_NS,Term],Full_URL).\n\
\n\
expand_ns(URL, URL).\n\
\n\
\n\
%%       collapse_ns(+FullURL, ?NSURL, +Char, +Options)\n\
%\n\
%	Collapses a full URI of the form Path#fragment to a Namespaced\n\
%	URI NS:fragment substituting the full expansion for ns from\n\
%	the ns/2 facts\n\
%	Char is either ':' for normal ns notation or '_' for builing\n\
%	prolog terms.\n\
%	Options supported: no_base(ShortNs): Use only term!\n\
\n\
\n\
collapse_ns(FullURL, NSURL,Char,Options) :-\n\
	nonvar(FullURL),\n\
	FullURL \\= literal(_),\n\
	uri_split(FullURL,LongNS, Term, '#'),\n\
	concat(LongNS,'#',LongNS1),\n\
	rdf_db:ns(ShortNS,LongNS1),\n\
	(   member(no_base(ShortNS),Options), ! , NSURL = Term\n\
	;\n\
	concat_atom([ShortNS,Char,Term],NSURL)\n\
	),!.\n\
% CJM\n\
collapse_ns(FullURL, NSURL,_Char,Options) :-\n\
	nonvar(FullURL),\n\
	\\+ FullURL = literal(_),\n\
	uri_split(FullURL,LongNS, Term, '#'),\n\
	member(no_base(LongNS),Options),\n\
        !,\n\
        NSURL = Term.\n\
\n\
\n\
collapse_ns(URL, URL,_,_).\n\
\n\
\n\
\n\
%%       uri_split(+URI,-Namespace,-Term,+Split_Char) is det\n\
%\n\
%       Splits a URI into the Namespace and the Term parts\n\
%       separated by the Split_Char character.\n\
%       It supposes URI = concat(Namespace,Split_Char,Term)\n\
\n\
uri_split(URI,Namespace,Term,Split_Char) :-\n\
	sub_atom(URI,Start,_,After,Split_Char),\n\
	sub_atom(URI,0,Start,_,Namespace),\n\
	Start1 is Start + 1,\n\
	sub_atom(URI,Start1,After,_,Term).\n\
\n\
\n\
%%       owl_collect_linked_nodes(+Node,+Predicate, +InList,-OutList)\n\
\n\
%	Appends Node to the InList, and recursively, all other\n\
%	Nodes that are linked with the Predicate to the Node. The\n\
%	result is returned to OutList.\n\
\n\
owl_collect_linked_nodes(Node,Predicate,InList,OutList) :-\n\
	use_owl(Node,Predicate,A),!,\n\
	owl_collect_linked_nodes(Node,Predicate,InList,List1),\n\
	owl_collect_linked_nodes(A,Predicate,List1,OutList).\n\
\n\
owl_collect_linked_nodes(Node,Predicate,InList,OutList) :-\n\
	use_owl(A,Predicate,Node),!,\n\
	owl_collect_linked_nodes(Node,Predicate,InList,List1),\n\
	owl_collect_linked_nodes(A,Predicate,List1,OutList).\n\
\n\
owl_collect_linked_nodes(Node,_,List, [Node|List]) :-\n\
	\\+ memberchk(Node, List),!.\n\
\n\
owl_collect_linked_nodes(_,_,List, List) :- !.\n\
\n\
\n\
% ----------------------------------------------------------------\n\
%                OWL Parser implementation predicates\n\
% ----------------------------------------------------------------\n\
\n\
\n\
%%       owl_get_bnode(+Node,+Description)\n\
%\n\
%	if Node is a blank (not named) node, then it is asserted in\n\
%	the database as a blanknode(Node,Description,used) term.\n\
%	The purpose is to record when a blank node has been used, so\n\
%	subsequent uses of it will result in structure sharing.\n\
\n\
owl_get_bnode(Node,Description) :-\n\
	sub_string(Node,0,2,_,'__'),!,\n\
	\\+ blanknode(Node,_,_),\n\
	assert(blanknode(Node,Description, used)).\n\
\n\
owl_get_bnode(_,_).\n\
\n\
\n\
\n\
% -----------------------------------------------------------------------\n\
%                                Top Level  Predicates\n\
% -----------------------------------------------------------------------\n\
\n\
/*:- multifile owl2_io:load_axioms_hook/3.\n\
\n\
owl2_io:load_axioms_hook(File,owl,Opts) :-\n\
        owl_parse_rdf(File,Opts).\n\
\n\
owl2_io:load_axioms_hook(File,ttl,Opts) :-\n\
        ensure_loaded(library('semweb/rdf_turtle')),\n\
        owl_parse_rdf(File,Opts).\n\
*/\n\
%% owl_parse_rdf(+File)\n\
% as owl_parse_rdf/1 with empty Opts\n\
owl_parse_rdf(F):-\n\
	owl_parse_rdf(F,[]).\n\
\n\
%% owl_parse_rdf(+File,+Opts:list)\n\
% @param Opts\n\
%  * imports(ImportFlag:Boolean) if true, follow imports\n\
%  * clear(Clear) if Clear=complete, clears all axioms in owl2_model\n\
owl_parse_rdf(F,Opts):-\n\
	(   member(imports(Imports),Opts)\n\
	->  true\n\
	;   Imports=false),\n\
	(   member(clear(Clear),Opts)\n\
	->  true\n\
	;   Clear=false),\n\
	owl_parse(F,Clear,Clear,Imports),\n\
	debug(owl_parser,'parsed ~w',[F]).\n\
\n\
\n\
\n\
\n\
%% owl_parse(+URL, +RDF_Load_Mode, +OWL_Parse_Mode, +ImportFlag:boolean)\n\
%\n\
%  Top level: parse a set of RDF triples and produce an\n\
%  AS representation of an OWL ontology.\n\
%\n\
%	Calls the rdf_load_stream predicate to parse RDF stream in URL.\n\
%       If RDF_Load_Mode = complete it first retacts all rdf triples.\n\
%       If ImportFlag = true it handles owl:import clause at RDF level.\n\
%\n\
% This implements the mapping defined here:\n\
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/\n\
owl_parse(URL, RDF_Load_Mode, OWL_Parse_Mode,ImportFlag) :-\n\
	(   RDF_Load_Mode=complete\n\
	->  rdf_retractall(_,_,_), retractall(rdf_db:rdf_source(_,_,_,_))\n\
        ;   true),\n\
	(   OWL_Parse_Mode=complete\n\
        ->  owl_clear_as,retractall(blanknode(_,_,_)), retractall(owl(_,_,_,_))\n\
        ;   true),\n\
        !,\n\
        debug(owl_parser,'Loading stream ~w',[URL]),\n\
	owl_canonical_parse_2([URL],URL,ImportFlag,[],ProcessedIRIs),\n\
        debug(owl_parser,'rdf_db populated, the following IRIs were processed: ~w',[ProcessedIRIs]),\n\
	owl2_model_init,\n\
	owl_canonical_parse_3(ProcessedIRIs).\n\
\n\
%% owl_canonical_parse_2(+IRIs:list,+ParentIRI,+ImportFlag:boolean,+ProcessedURIsIn:list,?ProcessedURIsOut:list) is det\n\
% recursively parses all ontologies in IRIs into rdf_db, ensuring none are processed twice.\n\
owl_canonical_parse_2([],_,_,Processed,Processed) :- !.\n\
\n\
owl_canonical_parse_2([IRI|ToProcessRest],Parent,ImportFlag,ProcessedIn,ProcessedOut) :-\n\
	member(IRI,ProcessedIn),\n\
        !,\n\
	owl_canonical_parse_2(ToProcessRest,Parent,ImportFlag,ProcessedIn,ProcessedOut).\n\
\n\
owl_canonical_parse_2([IRI|ToProcessRest],Parent,ImportFlag,ProcessedIn,ProcessedOut) :-\n\
	% Get rdf triples, *Ontology* and Imports\n\
	rdf_load_stream(IRI,O,BaseURI,Imports),\n\
	(   nonvar(O)\n\
        ->  Ont = O\n\
        ;   Ont = Parent), % in the include case we may need to remove the import...\n\
        debug(owl_parser,'Commencing rdf_2_owl. Generating owl/4',[]),\n\
	rdf_2_owl(BaseURI,Ont),  	% move the RDF triples into the owl-Ont/4 facts\n\
	(   ImportFlag = true\n\
        ->  owl_canonical_parse_2(Imports,Ont,ImportFlag,[Ont|ProcessedIn],ProcessedIn1)\n\
        ;   ProcessedIn1=[Ont|ProcessedIn]),\n\
	owl_canonical_parse_2(ToProcessRest,Parent,ImportFlag,ProcessedIn1,ProcessedOut).\n\
\n\
\n\
%% owl_canonical_parse_3(+IRIs:list) is det\n\
% translate the current rdf_db into owl2_model axioms.\n\
% First owl/4 facts are populated, and then these are translated\n\
% according to:\n\
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/\n\
% (table references refer to this document).\n\
% we use an intermediate owl/4 database because the mapping\n\
% is non-monotonic, and triples are 'consumed'\n\
owl_canonical_parse_3([]).\n\
\n\
owl_canonical_parse_3([IRI|Rest]) :-\n\
	% Remove any existing not used owl fact\n\
	retractall(owl(_,_,_,not_used)),\n\
	% Copy the owl facts of the IRI document to the 'not_used'\n\
	forall(owl(S,P,O,IRI),assert(owl(S,P,O,not_used))),\n\
\n\
        debug(owl_parser,'Anon individuals in reification [see table 8]',[]),\n\
\n\
\n\
	collect_r_nodes,\n\
\n\
	% First parse the Ontology axiom\n\
        owl_parse_annotated_axioms(ontology/1),\n\
\n\
        debug(owl_parser,'Replacing patterns [see table 5]',[]),\n\
	% remove triples based on pattern match (Table 5)\n\
	(   forall((triple_remove(Pattern,Remove), test_use_owl(Pattern)),\n\
	        forall(member(owl(S,P,O),Remove),use_owl(S,P,O,removed))) -> true ; true),\n\
\n\
\n\
        % temporary fix to make up for bug in rdf parsing\n\
        % see email to JanW July-1-2009\n\
        forall((test_use_owl(S,P,BNode),\n\
                atom(BNode),\n\
                sub_atom(BNode,0,2,_,'__'),\n\
                test_use_owl(BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype',literal(_))),\n\
               (   use_owl(S,P,BNode,datatype_fix),\n\
                   use_owl(BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype',literal(_)),\n\
                   expand_and_assert(S,P,literal('')))),\n\
\n\
	% replace matched patterns (Table 6)\n\
        debug(owl_parser,'Replacing patterns [see table 6]',[]),\n\
	(   setof(ReplaceWith,\n\
                  Pattern^(   triple_replace(Pattern,ReplaceWith), % +Triples:list, ?Triples:list\n\
                              use_owl(Pattern),\n\
                              debug(owl_parser,'Replacing ~w ==> ~w [see table 6]',[Pattern,ReplaceWith])),\n\
                  ReplacementSetList)\n\
        ->  forall((member(ReplacementSet,ReplacementSetList),member(owl(S,P,O),ReplacementSet)),\n\
                   expand_and_assert(S,P,O))\n\
        ;   debug(owl_parser,'No replacements required',[])),\n\
\n\
        /*\n\
	forall(triple_replace(Pattern,ReplaceWith),\n\
               forall(use_owl(Pattern),\n\
                      forall(member(owl(S,P,O),ReplaceWith),\n\
                             (   expand_and_assert(S,P,O),\n\
                                 debug(owl_parser,'Replacing ~w ==> ~w [see table 6]',[Pattern,owl(S,P,O)]))))),\n\
        */\n\
\n\
	% continue with parsing using the rules...\n\
	% Table 8, get the set of RIND - anonymous individuals in reification\n\
	findall(X, (member(Y,['owl:Axiom','owl:Annotation',\n\
			      'owl:AllDisjointClasses','owl:AllDisljointProperties',\n\
			      'owl:AllDifferent','owl:NegativePropertyAssertion']),\n\
                    test_use_owl(X,'rdf:type',Y)\n\
                   ),\n\
                RIND),\n\
	nb_setval(rind,RIND),\n\
\n\
        % Table 9, row 5\n\
	% VV 10/3/2010 get the annotation properties before collecting the annotations.\n\
        debug(owl_parser,'asserting annotationProperty/1 for all APs',[]),\n\
	forall( test_use_owl(D,'rdf:type','owl:AnnotationProperty'),\n\
		assert_axiom(annotationProperty(D))),\n\
\n\
        % TODO - make this faster\n\
        debug(owl_parser,'Implements function ANN(x) 3.2.2 Table 10.',[]),\n\
	findall(_,ann(_,_),_), % find all annotations, assert annotation(X,AP,AV) axioms.\n\
\n\
        debug(owl_parser,'Commencing parse of annotated axioms',[]),\n\
        forall((axiompred(PredSpec),\\+dothislater(PredSpec),\\+omitthis(PredSpec)),\n\
               owl_parse_annotated_axioms(PredSpec)),\n\
        forall((axiompred(PredSpec),dothislater(PredSpec),\\+omitthis(PredSpec)),\n\
               owl_parse_annotated_axioms(PredSpec)),\n\
\n\
        debug(owl_parser_detail,'Commencing parse of unannotated axioms',[]),\n\
        forall((axiompred(PredSpec),\\+dothislater(PredSpec),\\+omitthis(PredSpec)),\n\
               owl_parse_nonannotated_axioms(PredSpec)),\n\
        forall((axiompred(PredSpec),dothislater(PredSpec),\\+omitthis(PredSpec)),\n\
               owl_parse_nonannotated_axioms(PredSpec)),!,\n\
	% annotation Assertion\n\
	parse_annotation_assertions,\n\
	forall(owl_parse_compatibility_DL(Axiom),assert_axiom(Axiom)),\n\
	owl_canonical_parse_3(Rest).\n\
\n\
rdf_db_to_owl :-\n\
	owl2_model_init,\n\
        findall(BaseURI,\n\
                (   rdf(Ont,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#Ontology',BaseURI:_),\n\
                    rdf_2_owl(BaseURI,Ont),\n\
                    owl_canonical_parse_3(IRIs)),\n\
                IRIs).\n\
\n\
%% translate_rdf_db(+IRI)\n\
% translates a graph in current rdf_db instance into an owl2_model.pl set of facts.\n\
% assumes that IRI has already been loaded using the semweb package\n\
translate_rdf_db(BaseURI) :-\n\
        rdf(Ont,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#Ontology',BaseURI:_),\n\
        !,\n\
        rdf_2_owl(BaseURI,Ont),\n\
        owl2_model_init,\n\
        owl_canonical_parse_3(BaseURI).\n\
\n\
\n\
omitthis(ontology/1).\n\
\n\
\n\
owl_parse_annotated_axioms(Pred/Arity) :-\n\
        debug(owl_parser_detail,'[ann] Parsing all of type: ~w',[Pred]),\n\
        functor(Head,Pred,Arity),\n\
%        forall(owl_parse_axiom(Mod:Head),\n\
%               (   debug(owl_parser_detail,' parsed: [~w] ~w',[Mod,Head]),\n\
%                   assert(Mod:Head))).\n\
	forall(owl_parse_axiom(Head,true,Annotations),\n\
	       (   assert_axiom(Head),\n\
	           debug(owl_parser_detail_anns,' parsed: ~w : anns: ~w',[Head,Annotations]),\n\
		   forall(member(X,Annotations),\n\
			  forall(aNN(X,AP,AV),\n\
				 assert_axiom(annotation(Head,AP,AV))\n\
		          )\n\
			 )\n\
	       )\n\
	      ),\n\
        debug(owl_parser_detail,'[ann] Done parsing all of type: ~w',[Pred]).\n\
\n\
owl_parse_nonannotated_axioms(Pred/Arity) :-\n\
        debug(owl_parser_detail,'[unann] Parsing all of type: ~w',[Pred]),\n\
        functor(Head,Pred,Arity),\n\
	forall(owl_parse_axiom(Head,false,_),\n\
	       assert_axiom(Head)\n\
	      ).\n\
\n\
\n\
\n\
%%       rdf_load_stream(+URL, -Ontology, -BaseURI, -Imports:list) is det\n\
%\n\
%	This predicate calls the rdf parser to parse the RDF/XML URL\n\
%	into RDF triples. URL can be a local file or a URL.\n\
%	The predicate returns all Imports based on the 	owl:imports predicate.\n\
%	Also the Ontology of the URL if an owl:Ontology exists, var\n\
%	otherise.\n\
%\n\
%       If owl_repository/2 is defined, then this is used to map URLs\n\
%       prior to loading.\n\
\n\
\n\
rdf_load_stream(URL,Ontology,BaseURI,Imports) :-\n\
        owl_repository(URL,RURL),\n\
        !,\n\
        % note: users responsibility to avoid infinite loops by avoid cycles in repository mappings!\n\
        rdf_load_stream(RURL,Ontology,BaseURI,Imports).\n\
\n\
rdf_load_stream(URL,Ontology,BaseURI,Imports) :-\n\
	BaseURI = URL,\n\
  	(   sub_atom(URL,0,4,_,'http')\n\
        ->  catch((http_open(URL,RDF_Stream,[]),\n\
	      rdf_load(RDF_Stream,[if(true),base_uri(BaseURI),blank_nodes(noshare),\n\
				   result(Action, Triples, MD5),register_namespaces(true)]),\n\
		   debug(owl_parser,' Loaded ~w stream: ~w Action: ~w Triples:~w MD5: ~w',[URL,RDF_Stream,Action,Triples,MD5]),\n\
                   close(RDF_Stream)),\n\
                  Message,\n\
                  throw(io_error(URL,'rdf_load/2 failed',Message))) % re-throw with more information\n\
        ;  RDF_Stream = URL, rdf_load(RDF_Stream,[blank_nodes(noshare),if(true),base_uri(BaseURI),register_namespaces(true)])\n\
	),\n\
        % collect all imports directives\n\
	(   rdf(Ontology,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#Ontology',BaseURI:_)\n\
        ->  findall(I,rdf(Ontology,'http://www.w3.org/2002/07/owl#imports',I,BaseURI:_),Imports)\n\
	;   Imports = []\n\
	).\n\
\n\
\n\
\n\
% ----------------------------------------------------------------\n\
% 3 Mapping from RDF Graphs to the Structural Specification\n\
% ----------------------------------------------------------------\n\
\n\
/*\n\
\n\
  This section specifies the results of steps CP-2.2 and CP-3.3 of the\n\
  canonical parsing process from Section 3.6 of the OWL 2\n\
  Specification [OWL 2 Specification] on an ontology document D that\n\
  can be parsed into an RDF graph G. ...\n\
\n\
  */\n\
\n\
%       owl_description_list(+Node, -List)\n\
%\n\
%       If +Node is defined as rdf:type rdf:List, then List returns\n\
%       a prolog list of descriptions for this Node.\n\
\n\
owl_description_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.\n\
\n\
owl_description_list(X,[F|R]) :-\n\
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph\n\
	use_owl(X,'rdf:first',Element,first),\n\
	owl_description(Element,F),\n\
	use_owl(X,'rdf:rest',Y,rest),\n\
	!,owl_description_list(Y,R).\n\
\n\
\n\
%       owl_individual_list(+Node, -List)\n\
%\n\
%       If +Node is defined as rdf:type rdf:List, then List returns\n\
%       a prolog list of individuals for this Node.\n\
\n\
owl_individual_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.\n\
\n\
owl_individual_list(X,[F|R]) :-\n\
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph\n\
	use_owl(X,'rdf:first',F,first),\n\
	use_owl(X,'rdf:rest',Y,rest),\n\
	!,owl_individual_list(Y,R).\n\
\n\
%       owl_property_list(+Node, -List)\n\
%\n\
%       If +Node is defined as rdf:type rdf:List, then List returns\n\
%       a prolog list of properties for this Node.\n\
\n\
owl_property_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.\n\
\n\
owl_property_list(X,[F|R]) :-\n\
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph\n\
	use_owl(X,'rdf:first',Element,first),\n\
	owl_property_expression(Element,F),\n\
	use_owl(X,'rdf:rest',Y,rest),\n\
	!,owl_property_list(Y,R).\n\
\n\
%       owl_datarange_list(+Node, -List)\n\
%\n\
%       If +Node is defined as rdf:type rdf:List, then List returns\n\
%       a prolog list of dataranges for this Node.\n\
\n\
owl_datarange_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.\n\
\n\
owl_datarange_list(X,[F|R]) :-\n\
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph\n\
	use_owl(X,'rdf:first',Element,first),\n\
	owl_datarange(Element,F),\n\
	use_owl(X,'rdf:rest',Y,rest),\n\
	!,owl_datarange_list(Y,R).\n\
\n\
%       owl_datatype_restriction_list(+Node, -List)\n\
%\n\
%       If +Node is defined as rdf:type rdf:List, then List returns\n\
%       a prolog list of datatype restrictions for this Node.\n\
\n\
owl_datatype_restriction_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.\n\
\n\
owl_datatype_restriction_list(X,[facetRestriction(W2,L)|R]) :-\n\
	% use_owl(X,'rdf:type','rdf:List'), % this is now removed from graph\n\
	use_owl(X,'rdf:first',Element,first_datatype_restr),\n\
	use_owl(Element,W,L,datatype_restr),\n\
	(   concat_atom([_,W2],'#',W)\n\
	->  true\n\
	;   W2=W),\n\
	use_owl(X,'rdf:rest',Y,rest_datatype_restr),\n\
	!,owl_datatype_restriction_list(Y,R).\n\
\n\
\n\
% 3.1 Extracting Declarations and the IRIs of the Directly Imported Ontology Documents\n\
% This section specifies the result of step CP-2.2 of the canonical parsing process on an RDF graph G\n\
\n\
\n\
% 3.1.2 Parsing of the Ontology Header and Declarations\n\
\n\
%  Table 4.\n\
owl_parse_axiom(ontology(O),AnnMode,List) :-\n\
        test_use_owl(O,'rdf:type','owl:Ontology'),\n\
	\\+ test_use_owl([owl(U,_W,O),owl(U,'rdf:type','owl:Ontology')]),\n\
	valid_axiom_annotation_mode(AnnMode,O,'rdf:type','owl:Ontology',List),\n\
        use_owl(O,'rdf:type','owl:Ontology',ontology),\n\
        nb_setval(current_ontology,O),\n\
	forall(use_owl(O,'owl:imports',IRI,ontology_import), assert_axiom(ontologyImport(O,IRI))),\n\
	forall(use_owl(O,'owl:versionInfo',IRI2,ontology_version_info), assert_axiom(ontologyVersionInfo(O,IRI2))),!. % Do Once\n\
\n\
\n\
% See table 5.\n\
% triple_remove(Pattern:list,Remove:list)\n\
% if Pattern is present, remove triples in Remove\n\
triple_remove([owl(X,'rdf:type','owl:Ontology')],[owl(X,'rdf:type','owl:Ontology')]).\n\
triple_remove([owl(X,'rdf:type','owl:Class'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).\n\
triple_remove([owl(X,'rdf:type','rdfs:Datatype'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).\n\
triple_remove([owl(X,'rdf:type','owl:DataRange'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).\n\
triple_remove([owl(X,'rdf:type','owl:Restriction'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).\n\
triple_remove([owl(X,'rdf:type','owl:Restriction'),owl(X,'rdf:type','owl:Class')],[owl(X,'rdf:type','owl:Class')]).\n\
triple_remove([owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).\n\
triple_remove([owl(X,'rdf:type','owl:FunctionalProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).\n\
triple_remove([owl(X,'rdf:type','owl:InverseFunctionalProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).\n\
triple_remove([owl(X,'rdf:type','owl:TransitiveProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).\n\
triple_remove([owl(X,'rdf:type','owl:DatatypeProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).\n\
triple_remove([owl(X,'rdf:type','owl:AnnotationProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).\n\
triple_remove([owl(X,'rdf:type','owl:OntologyProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).\n\
triple_remove([owl(X,'rdf:type','rdf:List'),owl(X,'rdf:first',_Y),owl(X,'rdf:rest',_Z)],[owl(X,'rdf:type','rdf:List')]).\n\
/*\n\
   triple_remove([owl(X,'rdf:type','owl:Thing')],[owl(X,'rdf:type','owl:Thing')]).\n\
*/\n\
% See table 6.\n\
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/\n\
triple_replace([owl(X,'rdf:type','owl:OntologyProperty')],[owl(X,'rdf:type','owl:AnnotationProperty')]).\n\
triple_replace([owl(X,'rdf:type','owl:InverseFunctionalProperty')],[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:InverseFunctionalProperty')]).\n\
triple_replace([owl(X,'rdf:type','owl:TransitiveProperty')],[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:TransitiveProperty')]).\n\
triple_replace([owl(X,'rdf:type','owl:SymmetricProperty')],[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:SymmetricProperty')]).\n\
\n\
% NOTE: this is not specified in table 6. However, we treat rdfs:Classes as equivalent to owl:Classes\n\
triple_replace([owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','owl:Class')]).\n\
\n\
% DECLARATIONS\n\
%\n\
% See table 7.\n\
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/\n\
\n\
%% owl_parse_axiom(+AxiomSpec,+AnnMode:boolean,?AnnList:list) is det\n\
%\n\
% None\n\
%\n\
owl_parse_axiom(class(C),AnnMode,List) :-\n\
	test_use_owl(C,'rdf:type','owl:Class'),\n\
	valid_axiom_annotation_mode(AnnMode,C,'rdf:type','owl:Class',List),\n\
        (   use_owl(C,'rdf:type','owl:Class',named,class(C)) -> true ; use_owl(C,'rdf:type','rdfs:Class',named,class(C))),\n\
	\\+ class(C).\n\
\n\
\n\
owl_parse_axiom(datatype(D), AnnMode, List) :-\n\
        test_use_owl(D,'rdf:type','rdf:Datatype'),\n\
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:Datatype',List),\n\
        use_owl(D,'rdf:type','rdf:Datatype',datatype(D)).\n\
\n\
\n\
owl_parse_axiom(objectProperty(D), AnnMode, List) :-\n\
        test_use_owl(D,'rdf:type','owl:ObjectProperty'),\n\
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','owl:ObjectProperty',List),\n\
        use_owl(D,'rdf:type','owl:ObjectProperty',objectProperty(D)),\n\
	\\+ objectProperty(D).\n\
\n\
\n\
% note the difference in names between syntax and rdf\n\
owl_parse_axiom(dataProperty(D), AnnMode, List) :-\n\
        test_use_owl(D,'rdf:type','owl:DatatypeProperty'),\n\
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:DatatypeProperty',List),\n\
        use_owl(D,'rdf:type','owl:DatatypeProperty',dataProperty(D)),\n\
	\\+ dataProperty(D).\n\
\n\
owl_parse_axiom(annotationProperty(D), AnnMode, List) :-\n\
        test_use_owl(D,'rdf:type','owl:AnnotationProperty'),\n\
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:AnnotationProperty',List),\n\
        use_owl(D,'rdf:type','owl:AnnotationProperty',annotationProperty(D)),\n\
	\\+ annotationProperty(D).\n\
\n\
\n\
% TODO: check this. do we need to assert individual axioms if all we have is an rdf:type?\n\
owl_parse_axiom(namedIndividual(D), AnnMode, List) :-\n\
        test_use_owl(D,'rdf:type','owl:NamedIndividual'),\n\
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:NamedIndividual',List),\n\
        use_owl(D,'rdf:type','owl:NamedIndividual',namedIndividual(D)).\n\
\n\
\n\
% Table 8. Identifying Anonymous Individuals in Reification\n\
% TODO\n\
\n\
\n\
% 3.2 Populating an Ontology\n\
\n\
\n\
% 3.2.1 Analyzing Declarations\n\
\n\
% 3.2.2 Parsing of Annotations\n\
\n\
%\n\
%       ann(?X, -Extension List)\n\
%\n\
%       Implements function ANN(x) 3.2.2 Table 10\n\
%\n\
%     The annotations in G are parsed next. The function ANN assigns a\n\
%     set of annotations ANN(x) to each IRI or blank node x. This\n\
%     function is initialized by setting ANN(x) = â.. for each each IRI\n\
%     or blank node x. Next, the triple patterns from Table 10 are\n\
%     matched in G and, for each matched pattern, ANN(x) is extended\n\
%     with an annotation from the right column. Each time one of these\n\
%     triple patterns is matched, the matched triples are removed from\n\
%     G. This process is repeated until no further matches are\n\
%     possible\n\
\n\
ann(X,Y) :-\n\
	ann(X,X,Y).\n\
\n\
\n\
\n\
ann(X,X1, annotation(X1,Y,Z)) :-\n\
	annotationProperty(Y),\n\
        debug(owl_parser_detail,'annotation property: ~w',[Y]),\n\
        owl(X,Y,Z,not_used),\n\
        use_owl(X,Y,Z,annotationProperty(Y)),\n\
	u_assert(aNN(X1,Y,Z)),\n\
	ann2(X,Y,Z,X1).\n\
\n\
\n\
ann2(X,Y,Z,X1) :-\n\
	annotation_r_node(X,Y,Z,W),\n\
	ann(W,annotation(X1,Y,Z),Term),\n\
        u_assert(Term).\n\
\n\
ann2(X,Y,Z,X1) :-\n\
	axiom_r_node(X,Y,Z,W),\n\
	ann(W,annotation(X1,Y,Z),Term),\n\
        u_assert(Term).\n\
\n\
\n\
ann2(_,_,_,_).\n\
\n\
\n\
% 3.2.4 Parsing of Expressions\n\
\n\
is_bnode(C) :-\n\
	atom(C),\n\
	sub_atom(C,0,2,_,'__').\n\
\n\
\n\
	% Table 11. Parsing Object Property Expressions\n\
owl_property_expression(C,C) :-\n\
	\\+ is_bnode(C), % better: IRI(C).\n\
	% VV added 10/3/2011\n\
	C\\='http://www.w3.org/1999/02/22-rdf-syntax-ns#first',\n\
	C\\='http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',\n\
        !.\n\
\n\
owl_property_expression(C,D) :-\n\
	blanknode(C,D,Use),\n\
	(   Use = used,\n\
	    retractall(blanknode(C,D,used)),\n\
	    assert(blanknode(C,D,shared))\n\
	;\n\
	    true).\n\
\n\
owl_property_expression(P,inverseOf(Q)) :-\n\
        use_owl(P,'owl:inverseOf',Q,inverseof(P,Q)),\n\
        owl_get_bnode(P,inverseOf(Q)).\n\
\n\
\n\
% Table 12. Parsing of Data Ranges\n\
\n\
owl_datarange(D,D) :-\n\
	\\+ is_bnode(D),!.  % better: IRI(C).\n\
\n\
owl_datarange(C,D) :-\n\
	blanknode(C,D,Use),\n\
	(   Use = used,\n\
	    retractall(blanknode(C,D,used)),\n\
	    assert(blanknode(C,D,shared))\n\
	;\n\
	true).\n\
\n\
owl_datarange(D,intersectionOf(L)) :-\n\
	use_owl(D,'rdf:type','rdfs:Datatype',datarange(D)),\n\
	use_owl(D,'owl:intersectionOf',Y,datarange(D)),\n\
	%print(D-inter-Y),nl,\n\
        owl_datarange_list(Y,L),\n\
	owl_get_bnode(D,intersectionOf(L)).\n\
\n\
owl_datarange(D,unionOf(L)) :-\n\
	use_owl(D,'rdf:type','rdfs:Datatype',datarange(D)),\n\
	use_owl(D,'owl:unionOf',Y,datarange(D)),\n\
        owl_datarange_list(Y,L),\n\
	owl_get_bnode(D,unionOf(L)).\n\
\n\
\n\
owl_datarange(D,complementOf(DY)) :-\n\
	use_owl(D,'rdf:type','rdfs:Datatype',dataRange(D)),\n\
	use_owl(D,'owl:datatypeComplementOf',Y,datacomplement(D)),\n\
        owl_datarange(Y,DY),\n\
	owl_get_bnode(D,complementOf(DY)).\n\
\n\
% Table 14, case 2\n\
 owl_datarange(D,complementOf('rdfs:Literal')) :-\n\
	use_owl(D,'rdf:type','rdfs:DataRange',dataRange(D)),\n\
	use_owl(D,'owl:oneOf',[],oneOf(D)),\n\
	owl_get_bnode(D,complementOf('rdfs:Literal')).\n\
\n\
owl_datarange(D,oneOf(L)) :-\n\
	use_owl(D,'rdf:type','rdfs:Datatype',dataType(D)),\n\
	use_owl(D,'owl:oneOf',L1,oneOf(D)),\n\
	owl_individual_list(L1,L),\n\
	owl_get_bnode(D,oneOf(L)).\n\
\n\
% Table 14, case 1\n\
owl_datarange(D,oneOf(L)) :-\n\
	use_owl(D,'rdf:type','rdfs:DataRange',datarange(D)),\n\
	use_owl(D,'owl:oneOf',L1,datarange(D)),\n\
	owl_individual_list(L1,L),\n\
	owl_get_bnode(D,oneOf(L)).\n\
\n\
\n\
owl_datarange(D,datatypeRestriction(DY,L)) :-\n\
	use_owl(D,'rdf:type','rdfs:Datatype',datarange(D)),\n\
	use_owl(D,'owl:onDatatype',Y,datarange(D)),\n\
	owl_datarange(Y,DY),\n\
	use_owl(D,'owl:withRestrictions',L1,datarange(D)),\n\
	owl_datatype_restriction_list(L1,L),\n\
	owl_get_bnode(D,datatypeRestriction(DY,L)).\n\
\n\
% Table 13. Parsing of Class Expressions\n\
\n\
% ----------------------------------------------------------------------\n\
%       owl_description(+Node,-Description).\n\
%\n\
%	It implements OWL AS production rules for Descriptions.\n\
%         During the construction of the Description any blank node\n\
%         is recorded for later structure sharing checks.\n\
\n\
owl_description(C,C) :-\n\
	\\+ is_bnode(C),!. % better: IRI(C).\n\
\n\
\n\
owl_description(C,D) :-\n\
	blanknode(C,D,Use),\n\
	(   Use = used,\n\
	    retractall(blanknode(C,D,used)),\n\
	    assert(blanknode(C,D,shared))\n\
	;\n\
	    true),!.\n\
\n\
% TODO: this leaves behind classAssertions of type owlClass for the bnodes\n\
owl_description(D,intersectionOf(L)) :-\n\
	use_owl(D,'owl:intersectionOf',L1,intersectionOf(D)),\n\
	owl_description_list(L1,L),\n\
	\\+L = [],\n\
	owl_get_bnode(D,intersectionOf(L)),!.\n\
\n\
owl_description(D,unionOf(L)) :-\n\
	use_owl(D,'owl:unionOf',L1,union(D)),\n\
	owl_description_list(L1,L),\n\
	owl_get_bnode(D,unionOf(L)),!.\n\
\n\
\n\
owl_description(D,complementOf(Descr)) :-\n\
	use_owl(D,'owl:complementOf',D1,complementOf(D)),\n\
	owl_description(D1,Descr),\n\
	owl_get_bnode(D,complementOf(Descr)),!.\n\
\n\
owl_description(D,oneOf(L)) :-\n\
	use_owl(D,'owl:oneOf',L1,oneOf(D)),\n\
	(   use_owl(D,'rdf:type','owl:Class',oneOf(D,L)) ; true),\n\
	owl_individual_list(L1,L),\n\
	owl_get_bnode(D,oneOf(L)),!.\n\
\n\
owl_description(D,datatypeRestriction(DY,L)) :-\n\
	use_owl(D,'rdf:type','rdfs:Datatype',datatypeRestr(D)),\n\
	use_owl(D,'owl:onDatatype',Y,dataType(D)),\n\
	owl_datarange(Y,DY),\n\
	use_owl(D,'owl:withRestrictions',L1,withRestrictions(D)),\n\
	owl_datatype_restriction_list(L1,L),\n\
	owl_get_bnode(D,datatypeRestriction(DY,L)).\n\
\n\
owl_description(D,Restriction) :-\n\
	owl_restriction(D, Restriction),\n\
	owl_get_bnode(D,Restriction),!.\n\
\n\
\n\
% Table 15 - OWL DL compatibility class expressions\n\
%\n\
owl_description(D,Result) :-\n\
	\\+ is_bnode(D), % better: IRI(C).\n\
	use_owl(D,'rdf:type','owl:Class',description(D)),\n\
	use_owl(D,'owl:unionOf',L,unionOf(L)),\n\
	owl_description_list(L,DL),\n\
	(   DL = [], Result = 'owl:Nothing' ;\n\
	    DL = [D1], Result = D1),\n\
	owl_get_bnode(D,Result),!.\n\
\n\
owl_description(D,Result) :-\n\
	\\+ is_bnode(D), % better: IRI(C).\n\
	use_owl(D,'rdf:type','owl:Class',dl_compatibility_descr(D)),\n\
	use_owl(D,'owl:intersectionOf',L,intersectionOf(D)),\n\
	owl_description_list(L,DL),\n\
	(   DL = [], Result = 'owl:Thing' ;\n\
	    DL = [D1], Result = D1),\n\
	owl_get_bnode(D,Result),!.\n\
\n\
owl_description(D,Result) :-\n\
	\\+ is_bnode(D),!, % better: IRI(C).\n\
	use_owl(D,'rdf:type','owl:Class',dl_compatibility_descr(D)),\n\
	use_owl(D,'owl:oneOf',[],oneOf(D)),\n\
	Result = 'owl:Nothing',\n\
	owl_get_bnode(D,Result).\n\
\n\
% support older deprecated versions of OWL2 spec. See for example hydrology.owl\n\
onClass(E,D) :- use_owl(E,'http://www.w3.org/2006/12/owl2#onClass',D,onClass(E)).\n\
onClass(E,D) :- use_owl(E,'owl:onClass',D,onClass(E)).\n\
\n\
onDataRange(E,D) :- use_owl(E, 'owl:onDataRange',D,onDatarange(E)).\n\
\n\
\n\
%       owl_restriction(+Element,-Restriction).\n\
%\n\
%       If Element is defined as a owl:Restriction on property P then\n\
%       Restriction binds to a restriction(Property,Type) term,\n\
%	according to OWL Abstract syntax specification.\n\
\n\
owl_restriction(Element,Restriction) :-\n\
	use_owl(Element,'rdf:type','owl:Restriction',restriction(Element)),\n\
	(   use_owl(Element, 'owl:onProperty',PropertyID,onProperty(Element,PropertyID)) ;\n\
    	    use_owl(Element, 'owl:onProperties',PropertyID,onProperties(Element,PropertyID))\n\
	),\n\
	owl_restriction_type(Element,PropertyID, Restriction),\n\
        debug(owl_parser_detail,'Restriction: ~w',[Restriction]).\n\
\n\
\n\
\n\
owl_restriction_type(E, P, someValuesFrom(PX, DX)) :-\n\
	use_owl(E, 'owl:someValuesFrom',D,someValuesFrom(E,P)),\n\
	(   owl_description(D, DX) ; owl_datarange(D,DX)),\n\
        (   P = [_|_], owl_property_list(P,PX) ;  owl_property_expression(P, PX)).\n\
\n\
\n\
owl_restriction_type(E, P, allValuesFrom(PX,DX)) :-\n\
	use_owl(E, 'owl:allValuesFrom',D,allValuesFrom(E,P)),\n\
	(   owl_description(D, DX) ; owl_datarange(D,DX)),\n\
        (   P = [_|_], owl_property_list(P,PX) ;  owl_property_expression(P, PX)).\n\
\n\
\n\
% changed from thea value-->hasValue\n\
owl_restriction_type(E, P, hasValue(PX,Value)) :-\n\
	use_owl(E, 'owl:hasValue',Value,hasValue(E)),\n\
        owl_property_expression(P, PX).\n\
\n\
% VV:check if RDF parser returns a triple with O=true for\n\
owl_restriction_type(E, P, hasSelf(PX)) :-\n\
	use_owl(E, 'owl:hasSelf', true,hasSelf(E)),\n\
        owl_property_expression(P, PX).\n\
\n\
% Support of deprecated translations:\n\
% in the OWL2 RDF mapping, unqualified CRs use owl:{min,max}Cardinality\n\
% and QCQs use owl:{min,ax}QualifiedCardinality\n\
%\n\
% however, there appear to be some ontologies; e.g. Hydrology.owl.\n\
% that use an older mapping, where the same properties are used\n\
% for QCR and unqCR\n\
%\n\
% it is relatively easy to support this legacy ontologies; however\n\
% we must process these BEFORE unqualified cardinality restrictions.\n\
\n\
owl_restriction_type(E, P, exactCardinality(N,PX,DX)) :-\n\
	test_use_owl(E, 'owl:cardinality',Lit),\n\
        onClass(E,D),\n\
	owl_description(D, DX),!,\n\
	use_owl(E, 'owl:cardinality',Lit,cardinality(E)),\n\
        literal_integer(Lit,N),\n\
        owl_property_expression(P, PX).\n\
\n\
owl_restriction_type(E, P, minCardinality(N,PX,DX)) :-\n\
	test_use_owl(E, 'owl:minCardinality',Lit),\n\
        (   onClass(E,D),owl_description(D, DX)\n\
        ;   onDataRange(E,D), owl_datarange(D,DX)),\n\
	!,\n\
        % we are sure this is an old-style unqualified CR - now consume triples\n\
	use_owl(E, 'owl:minCardinality',Lit,minCardinality(E)),\n\
        literal_integer(Lit,N),\n\
        owl_property_expression(P, PX).\n\
\n\
owl_restriction_type(E, P, maxCardinality(N,PX,DX)) :-\n\
	test_use_owl(E, 'owl:maxCardinality',Lit),\n\
        (   onClass(E,D),owl_description(D, DX)\n\
        ;   onDataRange(E,D), owl_datarange(D,DX)),\n\
	!,\n\
        % we are sure this is an old-style unqualified CR - now consume triples\n\
	use_owl(E, 'owl:maxCardinality',Lit,maxCard(E)),\n\
        literal_integer(Lit,N),\n\
        owl_property_expression(P, PX).\n\
\n\
% END OF Support of deprecated translations:\n\
\n\
% the following are all in the spec:\n\
\n\
% changed from Thea1->2: cardinality->exactCardinality\n\
owl_restriction_type(E, P,exactCardinality(N,PX)) :-\n\
	use_owl(E, 'owl:cardinality',Lit,cardinality(E)),\n\
        literal_integer(Lit,N),\n\
        owl_property_expression(P, PX).\n\
\n\
owl_restriction_type(E, P,exactCardinality(N,PX,DX)) :-\n\
	use_owl(E, 'owl:qualifiedCardinality',Lit),literal_integer(Lit,N),\n\
	(   onClass(E,D),owl_description(D, DX) ;\n\
	    onDataRange(E,D), owl_datarange(D,DX)\n\
	),\n\
        owl_property_expression(P, PX).\n\
\n\
\n\
owl_restriction_type(E, P, minCardinality(N,PX)) :-\n\
	use_owl(E, 'owl:minCardinality',Lit,cardinality(E)),literal_integer(Lit,N),\n\
        owl_property_expression(P, PX).\n\
\n\
owl_restriction_type(E, P, minCardinality(N,PX,DX)) :-\n\
	use_owl(E, 'owl:minQualifiedCardinality',Lit,cardinality(E)),literal_integer(Lit,N),\n\
	(   onClass(E,D),owl_description(D, DX);\n\
	    onDataRange(E,D), owl_datarange(D,DX)\n\
	),\n\
        owl_property_expression(P, PX).\n\
\n\
\n\
owl_restriction_type(E, P, maxCardinality(N,PX)) :-\n\
	use_owl(E, 'owl:maxCardinality',Lit,maxCardinality(E)),literal_integer(Lit,N),\n\
        owl_property_expression(P, PX).\n\
\n\
owl_restriction_type(E, P, maxCardinality(N,PX,DX)) :-\n\
	use_owl(E, 'owl:maxQualifiedCardinality',Lit,cardinality(E,Lit)),\n\
	literal_integer(Lit,N),\n\
	(   onClass(E,D),owl_description(D, DX);\n\
	    onDataRange(E,D), owl_datarange(D,DX)),\n\
        owl_property_expression(P, PX).\n\
\n\
\n\
% Table 14. Parsing of Data Ranges for Compatibility with OWL DL\n\
% Included into owl_datarange clauses above\n\
\n\
% Table 15. Parsing of Class Expressions for Compatibility with OWL DL\n\
% Included into owl_dexcription clauses above\n\
\n\
% Table 16. Parsing of Axioms without Annotations\n\
% Declarations handled previously\n\
% CLASS AXIOMS\n\
% valid_axiom_annotation_mode: add clauses for the disjoint etc ....\n\
\n\
collect_r_nodes :-\n\
	retractall(axiom_r_node(_,_,_,_)),\n\
	forall(( test_use_owl(Node,'rdf:type','owl:Axiom'),\n\
		 test_use_owl(Node,'owl:annotatedSource',S),\n\
		 test_use_owl(Node,'owl:annotatedProperty',P),\n\
		 test_use_owl(Node,'owl:annotatedTarget',O)),\n\
	       (assert(axiom_r_node(S,P,O,Node)),\n\
                debug(owl_parser_detail,'~w',[axiom_r_node(S,P,O,Node)]),\n\
		use_owl([owl(Node,'rdf:type','owl:Axiom'),\n\
			 owl(Node,'owl:annotatedSource',S),\n\
			 owl(Node,'owl:annotatedProperty',P),\n\
			 owl(Node,'owl:annotatedTarget',O)]))),\n\
\n\
	retractall(annotation_r_node(_,_,_,_)),\n\
	forall(( test_use_owl(W,'rdf:type','owl:Annotation'),\n\
		 test_use_owl(W,'owl:annotatedSource',S),\n\
		 test_use_owl(W,'owl:annotatedProperty',P),\n\
		 test_use_owl(W,'owl:annotatedTarget',O)),\n\
	       (assert(annotation_r_node(S,P,O,Node)),\n\
                debug(owl_parser_detail,'~w',[annotation_r_node(S,P,O,Node)]),\n\
		use_owl([owl(W,'rdf:type','owl:Annotation'),\n\
			 owl(W,'owl:annotatedSource',S),\n\
			 owl(W,'owl:annotatedProperty',P),\n\
			 owl(W,'owl:annotatedTarget',O)]))).\n\
\n\
%% valid_axiom_annotation_mode(+AnnMode,+S,+P,+O,?AnnotationNodes:list) is det\n\
% if AnnMode is true and annotation triples can be found then\n\
% unify AnnotationNodes with the Nodes that annotate the triple,\n\
% otherwise []\n\
\n\
valid_axiom_annotation_mode(_Mode,S,P,O,List) :-\n\
        expand_ns(P,PE),\n\
        findall(Node,axiom_r_node(S,PE,O,Node),List).\n\
\n\
\n\
owl_parse_axiom(subClassOf(DX,DY),AnnMode,List) :-\n\
	test_use_owl(X,'rdfs:subClassOf',Y),\n\
	valid_axiom_annotation_mode(AnnMode,X,'rdfs:subClassOf',Y,List),\n\
	use_owl(X,'rdfs:subClassOf',Y,subclassOf(X,Y)),\n\
        owl_description(X,DX),\n\
	owl_description(Y,DY).\n\
\n\
% Process each equivalentClass pair separately in order to capture\n\
% annotations. Block the maximally connected subgraph.\n\
% TODO. Process the equivalent(L) axioms to generate maximally connected\n\
% equivalentClasses(L) axioms. (but without annotations?)\n\
\n\
owl_parse_axiom(equivalentClasses(DL),AnnMode,List) :-\n\
	test_use_owl(X,'owl:equivalentClass',Y),\n\
	valid_axiom_annotation_mode(AnnMode,X,'owl:equivalentClass',Y,List),\n\
	use_owl(X,'owl:equivalentClass',Y,equivalentClass(X,Y)),\n\
        % maximally_connected_subgraph_over('owl:equivalentClass',L),\n\
        maplist(owl_description,[X,Y],DL),\n\
        debug(owl_parser_detail,'equivalentClasses Descs: ~w',[DL]).\n\
\n\
\n\
owl_parse_axiom(equivalentClasses([C,intersectionOf(D)]),AnnMode,List) :-\n\
	class(C),\n\
	test_use_owl(C,'owl:intersectionOf',D1),\n\
	debug(owl_parser,'equivalent collection; intersection for ~w',[C]),\n\
	valid_axiom_annotation_mode(AnnMode,C,'owl:intersectionOf',D1,List),\n\
	owl_description(C,intersectionOf(D)).\n\
\n\
owl_parse_axiom(equivalentClasses([C,unionOf(D)]),AnnMode,List) :-\n\
	class(C),\n\
	test_use_owl(C,'owl:unionOf',D1),\n\
	debug(owl_parser,'equivalent collection; union for ~w',[C]),\n\
	valid_axiom_annotation_mode(AnnMode,C,'owl:unionOf',D1,List),\n\
	owl_description(C,unionOf(D)).\n\
\n\
owl_parse_axiom(equivalentClasses([C,oneOf(D)]),AnnMode,List) :-\n\
	class(C),\n\
	test_use_owl(C,'owl:oneOf',D1),\n\
	debug(owl_parser,'equivalent collection; one of for ~w',[C]),\n\
	valid_axiom_annotation_mode(AnnMode,C,'owl:oneOf',D1,List),\n\
	owl_description(C,oneOf(D)).\n\
\n\
\n\
owl_parse_axiom(equivalentClasses([C,D])) :-\n\
        % TODO: this could be made more efficient by enforcing order of building\n\
        (   test_use_owl(C,'rdf:type','owl:Class',named)\n\
        ;   test_use_owl(C,'rdf:type','rdfs:Class',named)\n\
        ;   class(C)),\n\
        owl_description(C,D),\n\
        C\\=D.\n\
\n\
% TODO. Process the disjointClasses(L) axioms to generate\n\
% larger set of disjoint: ie if N classes are pairwise DisJoint\n\
% then we can assert a disjointClasses for all N\n\
\n\
owl_parse_axiom(disjointClasses([DX,DY]),AnnMode,List) :-\n\
	test_use_owl(X,'owl:disjointWith',Y),\n\
	valid_axiom_annotation_mode(AnnMode,X,'owl:disjointWith',Y,List),\n\
	use_owl(X,'owl:disjointWith',Y,disjointWith(X,Y)),\n\
        owl_description(X,DX),\n\
	owl_description(Y,DY).\n\
\n\
% One of the cases where annotations are those of _x and we do not seek\n\
% for further annotation axioms. Par. 3.2.5.\n\
% Whatever the AnnNode, _x is returned (will be ignored if mode false\n\
\n\
owl_parse_axiom(disjointClasses(L),_AnnMode,[X]) :-\n\
        % TODO: X may be referred to in an annotation axiom??\n\
	use_owl(X,'rdf:type','owl:AllDisjointClasses',allDisjointClasses(X)),\n\
        use_owl(X,'owl:members',L1,members(L1)),\n\
        owl_description_list(L1,L).\n\
\n\
\n\
owl_parse_axiom(disjointUnion(DX,DY),AnnMode,List) :-\n\
	test_use_owl(X,'owl:disjointUnionOf',Y),\n\
	valid_axiom_annotation_mode(AnnMode,X,'owl:disjointUnionOf',Y,List),\n\
	use_owl(X,'owl:disjointUnionOf',Y,disjointUnionOf(X,Y)),\n\
        owl_description(X,DX),\n\
        owl_description_list(Y,DY).\n\
\n\
\n\
% PROPERTY AXIOMS\n\
\n\
\n\
% introduces bnode\n\
owl_parse_axiom(subPropertyOf(propertyChain(PL),QX),AnnMode,List) :-\n\
	test_use_owl(Q,'owl:propertyChainAxiom',L1),\n\
	valid_axiom_annotation_mode(AnnMode,Q,'owl:propertyChainAxiom',L1,List),\n\
	use_owl(Q,'owl:propertyChainAxiom',L1,propertyChainAxiom(Q)),\n\
	owl_property_list(L1,PL),\n\
        owl_property_expression(Q,QX).\n\
\n\
owl_parse_axiom(subPropertyOf(PX,QX),AnnMode,List) :-\n\
	test_use_owl(P,'rdfs:subPropertyOf',Q),\n\
	valid_axiom_annotation_mode(AnnMode,P,'rdfs:subPropertyOf',Q,List),\n\
	use_owl(P,'rdfs:subPropertyOf',Q,subPropertyOf(P,Q)),\n\
        owl_property_expression(P,PX),\n\
        owl_property_expression(Q,QX).\n\
\n\
\n\
% Process each equivalentProperty pair separately in order to capture\n\
% annotations. Block the maximally connected subgraph.\n\
% TODO. Process the equivalent(L) axioms to generate maximally connected\n\
% equivalentProperties(L) axioms. (but without annotations?)\n\
\n\
owl_parse_axiom(equivalentProperties(OPEL),AnnMode,List) :-\n\
	test_use_owl(X,'owl:equivalentProperty',Y),\n\
	valid_axiom_annotation_mode(AnnMode,X,'owl:equivalentProperty',Y,List),\n\
	use_owl(X,'owl:equivalentProperty',Y,equivProperty(X,Y)),\n\
	% maximally_connected_subgraph_over('owl:equivalentProperty',L),\n\
	maplist(owl_property_expression,[X,Y],OPEL).\n\
\n\
\n\
% TODO. Process the disjointProperties(L) axioms to generate\n\
% larger set of disjoint: ie if N properties are pairwise DisJoint\n\
% then we can assert a disjointClasses for all N\n\
\n\
owl_parse_axiom(disjointProperties([DX,DY]),AnnMode,List) :-\n\
	test_use_owl(X,'owl:propertyDisjointWith',Y),\n\
	valid_axiom_annotation_mode(AnnMode,X,'owl:propertyDisjointWith',Y,List),\n\
	use_owl(X,'owl:propertyDisjointWith',Y,propertyDisjointWith(X,Y)),\n\
        owl_description(X,DX),\n\
	owl_description(Y,DY).\n\
\n\
% One more of the cases where annotations are those of _x and we do not\n\
% seek for further annotation axioms. Par. 3.2.5. Whatever the AnnNode,\n\
% _x is returned (will be ignored if mode false)\n\
\n\
owl_parse_axiom(disjointProperties(L),_AnnMode,[X]) :-\n\
        % TODO: X may be referred to in an annotation axiom??\n\
	use_owl(X,'rdf:type','owl:AllDisjointProperties',allDisjointProps(X,L1)),\n\
        use_owl(X,'owl:members',L1,members(L1)),\n\
        L1 = [_,_|_],           % length >= 2\n\
        owl_property_list(L1,L).\n\
\n\
\n\
owl_parse_axiom(propertyDomain(PX,CX),AnnMode,List) :-\n\
	test_use_owl(P,'rdfs:domain',C),\n\
	valid_axiom_annotation_mode(AnnMode,P,'rdfs:domain',C,List),\n\
        use_owl(P,'rdfs:domain',C,domain(P,C)),\n\
	(   annotationProperty(P),CX = C ;\n\
	    owl_property_expression(P,PX),\n\
	    owl_description(C,CX)\n\
	).\n\
\n\
% We need to distinguish here between object and data property\n\
% Currently we first test if the range is a class, this means OPE\n\
% otherwise if it is a datarange it means a DPE.\n\
% Ideally we should also check possible declarations of OPE or DPE.\n\
\n\
owl_parse_axiom(propertyRange(PX,CX),AnnMode,List) :-\n\
	test_use_owl(P,'rdfs:range',C),\n\
	valid_axiom_annotation_mode(AnnMode,P,'rdfs:range',C,List),\n\
        use_owl(P,'rdfs:range',C,range(P,C)),\n\
	(   annotationProperty(P) -> PX = P, CX = C ;\n\
	    owl_property_expression(P,PX),\n\
            (   owl_description(C,CX) -> true ; owl_datarange(C,CX))\n\
	).\n\
\n\
owl_parse_axiom(inverseProperties(PX,QX),AnnMode,List) :-\n\
	test_use_owl(P,'owl:inverseOf',Q),\n\
	valid_axiom_annotation_mode(AnnMode,P,'owl:inverseOf',Q,List),\n\
	use_owl(P,'owl:inverseOf',Q,inverseOf(P,Q)),\n\
        owl_property_expression(P,PX),\n\
        owl_property_expression(Q,QX).\n\
\n\
owl_parse_axiom(functionalProperty(P),AnnMode,List) :-\n\
	test_use_owl(P,'rdf:type','owl:FunctionalProperty'),\n\
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:FunctionalProperty',List),\n\
        use_owl(P,'rdf:type','owl:FunctionalProperty',functionalProperty(P)).\n\
\n\
owl_parse_axiom(inverseFunctionalProperty(P),AnnMode,List) :-\n\
	test_use_owl(P,'rdf:type','owl:InverseFunctionalProperty'),\n\
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:InverseFunctionalProperty',List),\n\
        use_owl(P,'rdf:type','owl:InverseFunctionalProperty',inverseFunctionalProperty(P)).\n\
\n\
owl_parse_axiom(reflexiveProperty(P),AnnMode,List) :-\n\
	test_use_owl(P,'rdf:type','owl:ReflexiveProperty'),\n\
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:ReflexiveProperty',List),\n\
        use_owl(P,'rdf:type','owl:ReflexiveProperty',reflexiveProperty(P)).\n\
\n\
owl_parse_axiom(irreflexiveProperty(P),AnnMode,List) :-\n\
	test_use_owl(P,'rdf:type','owl:IrreflexiveProperty'),\n\
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:IrreflexiveProperty',List),\n\
        use_owl(P,'rdf:type','owl:IrreflexiveProperty',irreflexiveProperty(P)).\n\
\n\
owl_parse_axiom(symmetricProperty(P),AnnMode,List) :-\n\
	test_use_owl(P,'rdf:type','owl:SymmetricProperty'),\n\
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:SymmetricProperty',List),\n\
        use_owl(P,'rdf:type','owl:SymmetricProperty',symmetricProperty(P)).\n\
\n\
owl_parse_axiom(asymmetricProperty(P),AnnMode,List) :-\n\
	test_use_owl(P,'rdf:type','owl:AsymmetricProperty'),\n\
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:AsymmetricProperty',List),\n\
        use_owl(P,'rdf:type','owl:AsymmetricProperty',assymetricProperty(P)).\n\
\n\
owl_parse_axiom(transitiveProperty(P),AnnMode,List) :-\n\
	test_use_owl(P,'rdf:type','owl:TransitiveProperty'),\n\
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:TransitiveProperty',List),\n\
	use_owl(P,'rdf:type','owl:TransitiveProperty',transitiveProperty(P)).\n\
\n\
owl_parse_axiom(hasKey(CX,L),AnnMode,List) :-\n\
	test_use_owl(C,'owl:hasKey',L1),\n\
	valid_axiom_annotation_mode(AnnMode,C,'owl:hasKey',L1,List),\n\
	use_owl(C,'owl:hasKey',L1,hasKey(C)),\n\
	owl_description(C,CX),\n\
        L1 = [_,_|_],           % length >= 2\n\
        owl_property_list(L1,L).\n\
\n\
% INDIVIDUALS\n\
\n\
owl_parse_axiom(sameIndividual([X,Y]),AnnMode,List) :-\n\
	test_use_owl(X,'owl:sameAs',Y),\n\
	valid_axiom_annotation_mode(AnnMode,X,'owl:sameAs',Y,List),\n\
	use_owl(X,'owl:sameAs',Y,sameAs(X,Y)).\n\
\n\
owl_parse_axiom(differentIndividuals([X,Y]),AnnMode,List) :-\n\
	test_use_owl(X,'owl:differentFrom',Y),\n\
	valid_axiom_annotation_mode(AnnMode,X,'owl:differentFrom',Y,List),\n\
	use_owl(X,'owl:differentFrom',Y,differentFrom(X,Y)).\n\
\n\
owl_parse_axiom(differentIndividuals(L),_AnnMode,[X]) :-\n\
	use_owl(X,'rdf:type','owl:AllDifferent',allDifferent(L)),\n\
	use_owl(X,'owl:distinctMembers',L1,distinctMembers(L)),\n\
        owl_individual_list(L1,L).\n\
\n\
owl_parse_axiom(differentIndividuals(L),_AnnMode,[X]) :-\n\
	use_owl(X,'rdf:type','owl:AllDifferent',allDifferent(X)),\n\
	use_owl(X,'owl:members',L1,members(L)),\n\
        owl_individual_list(L1,L).\n\
\n\
% make sure this is done before fetching classAssertion/2;\n\
% -- the annotationAssertion matching clause should preceded the classAssertion/2 matching clause\n\
owl_parse_axiom(annotationAssertion('owl:deprecated', X, true),AnnMode,List) :-\n\
	test_use_owl(X, 'rdf:type', 'owl:DeprecatedClass'),\n\
	valid_axiom_annotation_mode(AnnMode,X,'rdf:type','owl:DeprecatedClass',List),\n\
	use_owl(X, 'rdf:type', 'owl:DeprecatedClass',deprecatedClass(X)).\n\
\n\
% make sure this is done before fetching propertyAssertion/3\n\
% this clause should precede it\n\
owl_parse_axiom(annotationAssertion('owl:deprecated', X, true),AnnMode,List) :-\n\
	test_use_owl(X, 'rdf:type', 'owl:DeprecatedProperty'),\n\
	valid_axiom_annotation_mode(AnnMode,X,'rdf:type','owl:DeprecatedProperty',List),\n\
	use_owl(X, 'rdf:type', 'owl:DeprecatedProperty',deprecatedProperty(X)).\n\
\n\
% Table 17. Parsing of Annotated Axioms\n\
\n\
dothislater(annotationAssertion/3).\n\
% TODO - only on unnannotated pass?\n\
%\n\
\n\
owl_parse_axiom(annotationAssertion(P,A,B),AnnMode,List) :-\n\
        annotationProperty(P),\n\
        test_use_owl(A,P,B),         % B can be literal or individual\n\
        valid_axiom_annotation_mode(AnnMode,A,P,B,List),\n\
        use_owl(A,P,B,annotationProperty(P)).\n\
\n\
\n\
dothislater(classAssertion/2).\n\
owl_parse_axiom(classAssertion(CX,X),AnnMode,List) :-\n\
	test_use_owl(X,'rdf:type',C),\n\
        C\\='http://www.w3.org/2002/07/owl#DeprecatedClass',\n\
	% note: some ontologies may include a rdf:type with no\n\
	%  explicit class declaration. See testfiles/test_undeclared.owl\n\
	%class(C),\n\
	valid_axiom_annotation_mode(AnnMode,X,'rdf:type',C,List),\n\
	use_owl(X,'rdf:type',C,classAssertion(CX,X)),\n\
        % I added this to avoid class assertions for bNodes. Perhaps a better\n\
        % way is to simply consume the owl4/ triple at the time of translating\n\
        % the description? --CJM\n\
        C\\='http://www.w3.org/2002/07/owl#Class',\n\
        %\n\
        C\\='http://www.w3.org/1999/02/22-rdf-syntax-ns#Property',\n\
        owl_description(C,CX).\n\
\n\
dothislater(propertyAssertion/3).\n\
owl_parse_axiom(propertyAssertion(PX,A,BX),AnnMode,List) :-\n\
        test_use_owl(A,P,B), % B can be literal or individual\n\
        P\\='http://www.w3.org/1999/02/22-rdf-syntax-ns#type',\n\
	% note: some ontologies may include a triples with no\n\
	%  explicit property declaration. See testfiles/test_undeclared.owl\n\
	%property(P),\n\
	valid_axiom_annotation_mode(AnnMode,A,P,B,List),\n\
        \\+ annotationProperty(P), % these triples should have been removed before, during ann parsing\n\
	owl_property_expression(P,PX), % can also be inverse\n\
	% next line added by VV 9/3/2011 for Jochem Liem to support ID-lists as PA objects\n\
	(   owl_individual_list(B,BX) -> true ; BX = B),\n\
        use_owl(A,P,B,propertyAssertion(PX,A,BX)).\n\
\n\
\n\
owl_parse_axiom(negativePropertyAssertion(PX,A,B),_,X) :-\n\
        use_owl(X,'rdf:type','owl:NegativePropertyAssertion',negPropertyAssertion(PX,A,B)),\n\
        use_owl(X,'owl:sourceIndividual',A,negPropertyAssertion(PX,A,B)),\n\
        use_owl(X,'owl:assertionProperty',P,negPropertyAssertion(PX,A,B)),\n\
        use_owl(X,'owl:targetValue',B,negPropertyAssertion(PX,A,B)),\n\
        owl_property_expression(P,PX).\n\
\n\
\n\
% process hooks; SWRL etc\n\
\n\
% Parsing annotationAssertions\n\
%\n\
\n\
parse_annotation_assertions :-\n\
	( nb_current(rind,RIND) -> true ; RIND = []),!,\n\
	forall((aNN(X,AP,AV),findall( aNN(annotation(X,AP,AV),AP1,AV1),\n\
				      aNN(annotation(X,AP,AV),AP1,AV1),ANN), \\+member(X,RIND), \\+name(X,[95, 95, 68, 101, 115, 99, 114, 105, 112, 116, 105, 111, 110|_])),\n\
	       (   assert_axiom(annotationAssertion(AP,X,AV)),\n\
		  %  VV 10/3/2010 keep annotation/3\n\
		  % retract(annotation(X,AP,AV)),\n\
		   forall(member(aNN(_,AP1,AV1),ANN),\n\
			    assert_axiom(annotation(annotationAssertion(AP,X,AV),AP1,AV1))\n\
			 )\n\
	       )\n\
	      ),\n\
	% forall(aNN(X,Y,Z),assert(annotation(X,Y,Z))), VV remove 25/1/11\n\
	% annotation/3 axioms created already during owl_parse_annotated_axioms/1\n\
	retractall(aNN(_,_,_)).\n\
\n\
% Table 18. Parsing of Axioms for Compatibility with OWL DL\n\
\n\
owl_parse_compatibility_DL(equivalentClasses([CEX,complementOf(CEY)])) :-\n\
	use_owl(X,'owl:complementOf',Y,eq_classes),\n\
	owl_description(X,CEX),\n\
	owl_description(Y,CEY).\n\
\n\
\n\
owl_parse_compatibility_DL(equivalentClasses([CEX,CEY])) :-\n\
	use_owl(X,'owl:unionOf',Y,eq_classes),\n\
	owl_description(X,CEX),\n\
	owl_description_list(Y,DL),\n\
	(   DL = [] -> CEY = 'owl:Nothing' ; (DL=[CEY]->true;CEY = unionOf(DL))).\n\
\n\
owl_parse_compatibility_DL(equivalentClasses([CEX,CEY])) :-\n\
	use_owl(X,'owl:intersectionOf',Y,eq_classes),\n\
	owl_description(X,CEX),\n\
	owl_description_list(Y,DL),\n\
	(   DL = [] -> CEY = 'owl:Thing' ; (DL=[CEY]->true;CEY = intersectionOf(DL))).\n\
\n\
owl_parse_compatibility_DL(equivalentClasses([CEX,CEY])) :-\n\
	use_owl(X,'owl:oneOf',Y,eq_classes),\n\
	owl_description(X,CEX),\n\
	owl_description_list(Y,DL),\n\
	(   DL = [] -> CEY = 'owl:Nothing' ; CEY = oneOf(DL)).\n\
\n\
% UTIL\n\
\n\
%% maximally_connected_subgraph_over(+P,?ConnectedSets) is semidet\n\
maximally_connected_subgraph_over(P,CSet):-\n\
        maximally_connected_subgraph_over(P,[],CSetL),\n\
        member(CSet,CSetL).\n\
\n\
%% maximally_connected_subgraph_over(+P,+Used,?ListOfConnectedSets) is det\n\
maximally_connected_subgraph_over(P,Used,[CSet|All]):-\n\
        test_use_owl(X,P,Y), % seed\n\
        \\+ member(X,Used),\n\
        \\+ member(Y,Used),\n\
        use_owl(X,P,Y,maximally_conected), % seed\n\
        !,\n\
        extend_set_over(P,[X,Y],CSet),\n\
        append(CSet,Used,Used2),\n\
        maximally_connected_subgraph_over(P,Used2,All).\n\
maximally_connected_subgraph_over(_,_,[]).\n\
\n\
\n\
% det\n\
extend_set_over(P,L,L2):-\n\
        member(X,L),\n\
        test_use_owl(X,P,Y),\n\
        \\+ member(Y,L),\n\
        use_owl(X,P,Y,extend_set_over),\n\
        !,extend_set_over(P,[Y|L],L2).\n\
extend_set_over(P,L,L2):-\n\
        member(X,L),\n\
        test_use_owl(Y,P,X),\n\
        \\+ member(Y,L),\n\
        use_owl(Y,P,X,extend_set_over),\n\
        !,extend_set_over(P,[Y|L],L2).\n\
extend_set_over(_,L,L):- !.\n\
\n\
literal_integer(literal(type,A),N) :- atom_number(A,N).\n\
literal_integer(literal(type(_,A)),N) :- atom_number(A,N).\n\
\n\
%% time_goal(+Goal,?Time)\n\
%  calls Goal and unifies Time with the cputime taken\n\
time_goal(Goal,Time):-\n\
        statistics(cputime,T1), Goal,\n\
        statistics(cputime,T2), Time is T2-T1.\n\
\n\
timed_forall(Cond,Action) :-\n\
        forall(Cond,\n\
               (   time_goal(Action,Time),\n\
                   debug(owl2_bench,'Goal: ~w Time:~w',[Action,Time]))).\n\
\n\
\n\
/** <module> Translates an RDF database to OWL2 axioms\n\
  ---+ Synopsis 1\n\
==\n\
:- use_module(bio(owl2_from_rdf)).\n\
%\n\
==\n\
---+ Details\n\
---++ Hooks\n\
* owl_parse_axiom_hook/3\n\
---+ See Also\n\
The file owl2_from_rdf.plt has some examples\n\
*/\n\
load_owl(String):-\n\
  pengine_self(Self),\n\
  pengine_property(Self,module(M)),\n\
  open_chars_stream(String,S),\n\
  process_rdf(stream(S), assert_list(M), [namespaces(NSList)]),\n\
  rdf_register_prefix('disponte','https://sites.google.com/a/unife.it/ml/disponte#',[keep(true)]),\n\
  assert(M:ns4query(NSList)),\n\
  close(S),\n\
  rdf_2_owl('ont','ont'),\n\
  owl_canonical_parse_3(['ont']),\n\
  parse_probabilistic_annotation_assertions.\n\
\n\
assert_list(_M,[], _):-!.\n\
assert_list(M,[H|T], Source) :-\n\
    H=..[_|Args],\n\
    H1=..[myrdf|Args],\n\
	assert(M:H1),\n\
        assert_list(M,T, Source).\n\
\n\
/*\n\
assert_list([], _):-!.\n\
assert_list([H|T], Source) :-\n\
	assert(H),\n\
        assert_list(T, Source).\n\
*/\n\
\n\
parse_probabilistic_annotation_assertions :-\n\
  forall(annotation(Ax,'https://sites.google.com/a/unife.it/ml/disponte#probability',literal(type(_Type, PV))),\n\
       (assert_axiom(annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability',Ax,literal(PV))))\n\
  ),\n\
  % forall(aNN(X,Y,Z),assert(annotation(X,Y,Z))), VV remove 25/1/11\n\
  % annotation/3 axioms created already during owl_parse_annotated_axioms/1\n\
  retractall(annotation(_,'https://sites.google.com/a/unife.it/ml/disponte#probability',_)).\n\
\n\
query_call(Q):-\n\
  Q =.. [P|Args],\n\
  pengine_self(Self),\n\
  pengine_property(Self,module(M)),\n\
  M:ns4query(NSList),!,\n\
  retract(M:ns4query(NSList)),\n\
  expand_all_ns(Args,NSList,NewArgs),!,\n\
  NQ =.. [P|NewArgs],\n\
  call(NQ).\n\
\n\
expand_all_ns([],_,[]).\n\
expand_all_ns([H|T],NSList,[H|NewArgs]):-\n\
  check_query_arg(H),!,\n\
  expand_all_ns(T,NSList,NewArgs).\n\
\n\
expand_all_ns([H|T],NSList,[NewArg|NewArgs]):-\n\
  expand_ns4query(H,NSList,NewArg),\n\
  expand_all_ns(T,NSList,NewArgs).\n\
\n\
check_query_arg(Arg) :-\n\
  atomic(Arg),!,\n\
  axiom(Ax),\n\
  Ax =.. [_|L],\n\
  flatten(L,L1),\n\
  member(Arg,L1),!.\n\
\n\
expand_ns4query(NS_URL, [], NS_URL).\n\
expand_ns4query(NS_URL, [Short_NSL=Long_NSL|_],Full_URL):- \n\
	nonvar(NS_URL),\n\
	NS_URL \\= literal(_),\n\
	uri_split(NS_URL,Short_NS,Term, ':'),\n\
	Short_NS = Short_NSL,\n\
	Long_NS = Long_NSL,!,\n\
	concat_atom([Long_NS,Term],Full_URL).\n\
\n\
expand_ns4query(NS_URL, [[]=Long_NSL|_],Full_URL):- \n\
	nonvar(NS_URL),\n\
	NS_URL \\= literal(_),\n\
	\\+ sub_atom(NS_URL,_,_,_,':'),\n\
	Long_NS = Long_NSL,!,\n\
	concat_atom([Long_NS,NS_URL],Full_URL).\n\
\n\
expand_ns4query(NS_URL, [_|T],Full_URL):- \n\
  expand_ns4query(NS_URL, T,Full_URL),!.\n\
expand_ns4query(URL,_, URL).\n\
\n\
parse:- \n\
  pengine_self(M),\n\
  set_prolog_flag(M:unknwon,fail),\n\
  load_owl('"+
	    query.source+"')." ,
	    destroy: false,
	    format: 'json-html',
	    oncreate: handleCreate,
	    onsuccess: handleSuccess,
	    onfailure: handleFailure,
	    onstop: handleStop,
	    onprompt: handlePrompt,
	    onoutput: handleOutput,
	    onerror: handleError,
	    onabort: handleAbort});
	  data.prolog.state = "idle";
	});

	return this;
      });
    }, //_init()

    /**
     * Add a _positive_ answer to the runner.  The answer is embedded in
     * a `<div class="answer">` with an additional class `"even"` or
     * `"odd"` to simplify styling. Note that using CSS odd/even
     * selectors is not possible because there are other elements such
     * as errors.
     * @param {Answer} answer pengine response in `json-html`
     */
    renderAnswer: function(answer) {
      var data = this.data('prologRunner');
      var even = (++data.answers % 2 == 0);

      if ( data.query.tabled ) {
	if ( data.answers == 1 ) {
	  if ( answer.projection && answer.projection.length > 0 ) {
	    var table = answerTable(answer.projection);
	    addAnswer(this, table);
	    data.table = table;
	    data.projection = answer.projection;
	    answer.nth = data.answers;
	    $(data.table).prologAnswer(answer);
	    return this;
	  }
        } else
	{ answer.projection = data.projection;
	  answer.nth = data.answers;
	  $(data.table).prologAnswer(answer);
	  return this;
	}
      }

      var div = $.el.div({class:"answer "+(even ? "even" : "odd")},
			 $.el.span({class:"answer-no"}, data.answers));

      addAnswer(this, div);
      $(div).prologAnswer(answer);
    },

    /**
     * Add pengine output as `<span class="output">`
     * @param {String} data HTML that is inserted in the span.
     */
    outputHTML: function(data) {
      var span = $.el.span({class:"output"});
      $(span).html(data);
      addAnswer(this, span);
    },

    /**
     * Add an error message to the output.  The error is
     * wrapped in a `<pre class="error">` element.
     * @param {String} msg the plain-text error message
     */
    error: function(msg) {
      addAnswer(this, $.el.pre({class:"prolog-message msg-error"}, msg));
    },

    /**
     * set the placeholder of the input field.  This is normally
     * done from the pengine's onprompt handler
     * @param {String} p the new placeholder
     */
    setPrompt: function(p) {
      this.find(".controller input").attr("placeholder", p);
    },

    /**
     * send a response (to pengine onprompt handler) to the
     * pengine and add the response to the dialogue as
     * `div class="response">`
     * @param {String} s plain-text response
     */
    respond: function(s) {
      var data = this.data('prologRunner');
      addAnswer(this, $.el.div({class:"response"}, s));
      data.prolog.respond(s);
    },

    /**
     * Stop the associated Prolog engines.
     */
    stop: function() {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data('prologRunner');
	data.prolog.stop();
      });
    },

    /**
     * Stop the pengine if it is waiting for a next solution,
     * abort it if it is running or waitin for input and ignore
     * otherwise.
     */
    stopOrAbort: function() {
      return this.each(function() {
	var elem  = $(this);
	var data  = elem.data('prologRunner');
	var state = elem.prologRunner('getState');

	switch(state)
	{ case "running":
	  case "wait-input":
	    data.prolog.abort();
	    break;
	  case "wait-next":
	    data.prolog.stop();
	}
      });
    },

    /**
     * Ask the associated Prolog engines for the next answer.
     * @param {Integer} chunk maximum number of answers to return in the
     * next chunk.
     */
    next: function(chunk) {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data('prologRunner');
	data.prolog.next(chunk);
	elem.prologRunner('setState', "running");
      });
    },

    /**
     * Abort the associated Prolog engines.
     */
    abort: function() {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data('prologRunner');
	data.prolog.abort();
      });
    },

    /**
     * If the associated pengine is alive, send it a `destroy`.  Next,
     * remove the runner from its container.
     */
    close: function() {
      if ( this.length ) {
	var runners = RS(this);

	this.each(function() {
	  var elem = $(this);
	  var data = elem.data('prologRunner');

	  if ( elem.prologRunner('alive') )
	    data.prolog.destroy();
	});
	this.remove();

	runners.prologRunners('scrollToBottom', true);
      }
      return this;
    },

    /**
     * Provide help on running a query
     */
     trill_on_swish_help: function() {
       $(".trill_on_swish-event-receiver").trigger("trill_on_swish_help", {file:"runner.html"});
     },

    /**
     * Toggle or set the iconic state of the runner.
     * @param {Boolean} [on] if `true`, make iconify, `false` expanded
     * and toggle if unspecified
     */
    toggleIconic: function(on) {
      if ( on == undefined ) {
	this.toggleClass("iconic");
      } else if ( on ) {
	this.addClass("iconic");
      } else {
	this.removeClass("iconic");
      }

      RS(this).prologRunners('scrollToBottom', true);

      return this;
    },

    /**
     * Populate the menu associated with the pengine icon.
     * @param {Object} [actions] associates labels with functions.
     */
    populateActionMenu: function(actions) {
      var menu = this.find(".runner-title .btn-group.dropdown");

      actions = $.extend({ "Re-run": function() { console.log("Re-Run ", this); }
			 }, actions);

      populateMenu(menu, this, actions);

      return this;
    },

  /**
   * @param {String} state defines the new state of the pengine.
   * Known states are:
   *
   *   - "idle"	      - Pengine is not yet created
   *   - "running"    - Pengine is running
   *   - "wait-next"  - Pengine produced a non-deterministic answer
   *   - "wait-input" - Pengine waits for input
   *   - "true"       - Pengine produced the last answer
   *   - "false"      - Pengine failed
   *   - "error"      - Pengine raised an error
   *   - "stopped"    - User selected *stop* after non-det answer
   *   - "aborted"    - User aborted execution
   *
   * The widget is brought to the new  state   by  adding the state as a
   * class to all members of  the   class  `show-state`, which currently
   * implies the pengines icon at the   top-left  and a _controller_ div
   * created by controllerDiv().
   */
   setState: function(state) {
     var data = this.data('prologRunner');

     if ( data.prolog.state != state ) {
       var stateful = this.find(".show-state");

       stateful.removeClass(data.prolog.state).addClass(state);
       data.prolog.state = state;
       if ( !aliveState(state) && data.savedFocus ) {
	 $(data.savedFocus).focus();
	 data.savedFocus = null;
       } else if ( state == "wait-input" ) {
	 this.find("input").focus();
       }
       if ( !aliveState(state) )
	 data.prolog.destroy();
     }
     if ( state == "wait-next" || state == "true" ) {
       var runners = RS(this);
       setTimeout(function() { runners.prologRunners('scrollToBottom') }, 100);
     } else {
       RS(this).prologRunners('scrollToBottom');
     }
     return this;
   },

   /** @returns {String} representing the current state of the
    * query execution.
    * @see {@link setState}
    */
   getState: function() {
     var data = this.data('prologRunner');

     return data.prolog ? data.prolog.state : "idle";
   },

   /**
    * @returns {Boolean} true if the related pengine is alive.  That
    * means it has state `"running"`, `"wait-next"` or `"wait-input"`
    */
   alive: function() {
     return aliveState(this.prologRunner('getState'));
   }

  }; // methods


		 /*******************************
		 *     PRIVATE FUNCTIONS	*
		 *******************************/

  function RS(from) {			/* find runners from parts */
    return $(from).parents(".prolog-runners");
  }

  function addAnswer(runner, html) {
    var results = runner.find(".runner-results");
    results.append(html);
    return this;
  }

  function aliveState(state) {
    switch( state )
    { case "running":
      case "wait-next":
      case "wait-input":
	return true;
      default:
	return false;
    }
  }

  function answerTable(projection) {
    var tds = [{class:"projection"}];

    for(i=0; i<projection.length; i++)
      tds.push($.el.th({class:"pl-pvar"}, projection[i]));
    tds.push($.el.th({class:"answer-nth"}, "No"));

    var table = $.el.table({class:"prolog-answers"},
			   $.el.tbody($.el.tr.apply(this, tds)));

    return table;
  }



		 /*******************************
		 *   HANDLE PROLOG CALLBACKS	*
		 *******************************/

  function handleCreate() {
    var elem = this.pengine.options.runner;
    var data = elem.data('prologRunner');

    this.pengine.ask("parse,query_call("+termNoFullStop(data.query.query)+")");
    elem.prologRunner('setState', "running");
  }

  function handleSuccess() {
    var elem = this.pengine.options.runner;

    for(var i=0; i<this.data.length; i++) {
      var answer = this.data[i];
      if ( this.projection )
	answer.projection = this.projection;

      elem.prologRunner('renderAnswer', answer);
    }
    if ( this.time > 0.1 )	/* more than 0.1 sec. CPU (TBD: preference) */
      addAnswer(elem, $.el.div(
	{class:"cputime"},
	$.el.span(this.time.toFixed(3),
		  " seconds cpu time")));

    elem.prologRunner('setState', this.more ? "wait-next" : "true");
  }

  function handleFailure() {
    var elem = this.pengine.options.runner;

    addAnswer(elem, $.el.span({class: "prolog-false"}, "false"));
    elem.prologRunner('setState', "false");
  }

  function handleStop() {
    var elem = this.pengine.options.runner;

    elem.prologRunner('setState', "stopped");
  }

  function handlePrompt() {
    var elem   = this.pengine.options.runner;
    var prompt = this.data ? this.data : "Please enter a Prolog term";

    elem.prologRunner('setPrompt', prompt);
    elem.prologRunner('setState', "wait-input");
  }

  /**
   * handle `pengine_output/1`.  Note that compiler warnings and errors
   * also end up here. If they have a location, this is provided through
   * this.location, which contains `file`, `line` and `ch`.  We must use
   * this to indicate the location of the error in CodeMirror.
   */

  function handleOutput() {
    var elem = this.pengine.options.runner;

    this.data = this.data.replace(new RegExp("'[-0-9a-f]{36}':", 'g'), "")
    if ( this.location ) {
      this.data = this.data.replace(/pengine:\/\/[-0-9a-f]*\//, "");
      $(".trill_on_swish-event-receiver").trigger("source-error", this);
    }

    elem.prologRunner('outputHTML', this.data);
    RS(elem).prologRunners('scrollToBottom');
  }

  function handleError() {
    var elem = this.pengine.options.runner;
    var msg;

    if ( this.code == "too_many_pengines" ) {
      msg = "Too many open queries.  Please complete some\n"+
	    "queries by using |Next|, |Stop| or by\n"+
	    "closing some queries.";
    } else
    { msg = String(this.data)
                .replace(new RegExp("'"+this.pengine.id+"':", 'g'), "");
    }

    elem.prologRunner('error', msg);
    elem.prologRunner('setState', "error");
  }

  function handleAbort() {
    var elem = this.pengine.options.runner;

    elem.prologRunner('error', "** Execution aborted **");
    elem.prologRunner('setState', "aborted");
  }

  /**
   * @param {Object} answer a positive answer from the Pengine
   * @returns {Boolean} true if the answer has printable part, i.e., no
   * variable bindings nor residual goals.
   */

  function answerHasOutput(answer) {
    return answer.variables.length > 0 || answer.residuals;
  }

  function termNoFullStop(s) {
    return String($.trim(s)).replace(/\.$/, "");
  }

  /**
   * Run a Prolog query by starting a remote pengine.
   *
   * @class prologRunner
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.prologRunner = function(method) {
    if ( methods[method] ) {
      return methods[method]
	.apply(this, Array.prototype.slice.call(arguments, 1));
    } else if ( typeof method === 'object' || !method ) {
      return methods._init.apply(this, arguments);
    } else {
      $.error('Method ' + method + ' does not exist on jQuery.' + pluginName);
    }
  };
}(jQuery));


		   /*******************************
		   *	       UTIL		*
		   *******************************/

  function dropdownButton(icon, options) {
    if ( !options ) options = {};
    var cls     = options.divClass;
    var ulClass = options.ulClass;

    var dropdown = $.el.div(
      {class: "btn-group dropdown"+(cls?" "+cls:"")},
      $.el.button(
	{class:"dropdown-toggle",
	 "data-toggle":"dropdown"},
	icon),
      $.el.ul({class:"dropdown-menu"+(ulClass?" "+ulClass:"")}));

    if ( options.actions )
      populateMenu($(dropdown), options.client, options.actions);

    return dropdown;
  }

  function populateMenu(menu, client, actions) {
    var ul = menu.find(".dropdown-menu");

    function runMenu(a) {
      var action = $(a).data('action');

      if ( action )
	action.call(client);

      return false;
    }

    function addMenuItem(label, onclick) {
      var a = $.el.a(label);

       $(a).data('action', onclick);
       ul.append($.el.li(a));
    }

    for(var a in actions) {
      if ( actions.hasOwnProperty(a) ) {
	addMenuItem(a, actions[a]);
      }
    }

    ul.on("click", "a", function() { runMenu(this); } );

    return menu;
  }
});
