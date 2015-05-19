/**
 * @fileOverview
 * Prolog editor plugin based on [CodeMirror](http://codemirror.net)
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 * @requires codemirror
 */

define([ "tos_cm/lib/codemirror",
	 "config",
	 "preferences",
	 "form",
	 //"../bower_components/codemirror/mode/javascript/javascript",
	 //"../bower_components/codemirror/mode/xml/xml",
	 //"../bower_components/codemirror/mode/clike/clike",
	 //"tos_cm/mode/xml/xml-template-hint",
	 //"tos_cm/mode/prolog/prolog-template-hint",
	 "trill_on_swish_gitty",
	 "modal",

	 "tos_cm/addon/edit/matchbrackets",
	 "tos_cm/addon/comment/continuecomment",
	 "tos_cm/addon/comment/comment",
	 "tos_cm/addon/hint/show-hint",
	 "tos_cm/addon/hint/anyword-hint",
	 "tos_cm/addon/display/placeholder",
	 "tos_cm/addon/runmode/runmode",
	 "tos_cm/mode/xml/xml",
	 //"tos_cm/mode/xml/xml_keys",
	 //"tos_cm/mode/xml/xml_query",
	 //"tos_cm/mode/xml/xml_server",
	 //"tos_cm/addon/hover/xml-hover",
	 //"tos_cm/mode/prolog/prolog",
	 //"tos_cm/mode/prolog/prolog_keys",
	 //"tos_cm/mode/prolog/prolog_query",
	 //"tos_cm/mode/prolog/prolog_server",
	 //"tos_cm/addon/hover/prolog-hover",
	 //"tos_cm/addon/hover/text-hover",

	 //"tos_cm/addon/hint/templates-hint",
	 //"tos_cm/addon/hint/show-context-info",

         "jquery", "laconic"
         /*"tos_cm/lib/codemirror",
	 "config",
	 "preferences",
	 "form",
	 "tos_cm/mode/prolog/prolog-template-hint",
	 "trill_on_swish_gitty",
	 "modal",

	 	 "tos_cm/mode/prolog/prolog",
	 "tos_cm/mode/prolog/prolog_keys",
	 "tos_cm/mode/prolog/prolog_query",
	 "tos_cm/mode/prolog/prolog_server",

	 "tos_cm/addon/edit/matchbrackets",
	 "tos_cm/addon/comment/continuecomment",
	 "tos_cm/addon/comment/comment",
	 "tos_cm/addon/hint/show-hint",
	 "tos_cm/addon/hint/anyword-hint",
	 "tos_cm/addon/display/placeholder",
	 "tos_cm/addon/runmode/runmode",

	 "tos_cm/addon/hover/text-hover",
	 "tos_cm/addon/hover/prolog-hover",

	 "tos_cm/addon/hint/templates-hint",
	 "tos_cm/addon/hint/show-context-info",

         "jquery", "laconic",*/
       ],
       function(CodeMirror, config, preferences, form, /*templateHint,*/ trill_on_swish_gitty) {

(function($) {
  var pluginName = 'xmlEditor';

  /** @lends $.fn.xmlEditor */
  var methods = {
    /**
     * Initialize a Prolog editor.
     * @param {Object} [options]
     * @param {String} [options.role="source"] determines the role of
     * the editor. It is one of `source` or `query`.
     * @param {String} [options.placeholder="Your Prolog program goes here ..."]
     * sets the placeholder for the editor.
     * @param {Boolean} [options.lineNumbers=true] defines whether or
     * not a left-gutter with line numbers is displayed.
     * @param {String} [options.mode="prolog"] defines the mode used by
     * CodeMirror.
     * @param {String} [options.theme="prolog"] defines the CSS used for
     * highlighting.
     * @param {Boolean} [options.matchBrackets=true] defines whether the
     * matching bracket is highlighted.
     * @param {Boolean} [options.prologKeys=true] defines whether "(",
     * ">" and ";" act as active keys to support if-then-else layout.
     * @param {Object} [options.extraKeys] specifies additional key
     * bindings.  Default is to bind "Ctrl-Space" and "Alt-/" to
     * "autocomplete".
     *
     */
    _init: function(options) {

      return this.each(function() {
	var elem = $(this);
	var data = {};
	var ta;					/* textarea */

	options = $.extend({
	  role: "source",
	  placeholder: "Your ontology goes here (use RDF/XML format) ...",
	  lineNumbers: true,
	  mode: "xml",
//	  theme: "xml",
//          matchBrackets: true,
//          textHover: true,
//          prologKeys: true
	}, options);


	if ( options.role != "query" )
	  options.continueComments = "Enter";

	if ( (ta=elem.children("textarea")[0]) ) {
	  var file = $(ta).attr("data-file");

	  if ( file )
	    data.file = file;
	  if ( window.trill_on_swish && window.trill_on_swish.meta_data )
	    data.meta = window.trill_on_swish.meta_data;
	} else {
	  ta = $.el.textarea({placeholder:options.placeholder},
			     elem.text());
	  elem.append(ta);
	}
  //      CodeMirror.defaults.mode="javascript";
	data.tos_cm              = CodeMirror.fromTextArea(ta, options);
	console.log(CodeMirror.defaults);
	console.log(data.tos_cm.getMode());
	console.log(data.tos_cm.getOption("mode"));
	data.cleanGeneration = data.tos_cm.changeGeneration();
	data.role            = options.role;

	elem.data(pluginName, data);
	elem.addClass("trill_on_swish-event-receiver");

	if ( data.role == "source" ) {
	  elem.on("source", function(ev, src) {
	    elem.xmlEditor('setSource', src);
	  });
	  elem.on("saveProgram", function(ev, data) {
	    elem.xmlEditor('save', data);
	  });
	  elem.on("fileInfo", function() {
	    elem.xmlEditor('info');
	  });
	}
      });
    },

    /**
     * @example // Get the CodeMirror instance
     * $(element).xmlEditor('getOption', 'tos_cm');
     * @param {String} opt Name of option to fetch.
     * @return {*}
     */

    getOption: function(opt) {
      var elem = this;
      return elem.data(pluginName)[opt];
    },

    /**
     * @returns {String} current contents of the editor
     */
    getSource: function() {
      return this.data(pluginName).tos_cm.getValue();
    },

    /**
     * @return {String|null} UUID of the source used for server-side
     * analysis
     */
     getSourceID: function() {
       var tos_cm = this.data(pluginName).tos_cm;

       return null;
     },

    /**
     * @param {String|Object} src becomes the new contents of the editor
     * @param {String} Object.data contains the data in the case that
     * `src` is an object.
     */
    setSource: function(src) {
      var options = this.data(pluginName);

      if ( typeof(src) == "string" )
	src = {data:src};

      this.data(pluginName).tos_cm.setValue(src.data);

      if ( options.role == "source" ) {
	if ( src.meta ) {
	  options.file = src.meta.name;
	  options.meta = src.meta;
	} else {
	  options.file = null;
	  options.meta = null;
	}

	if ( !src.url )
	  src.url = config.http.locations.trill_on_swish;

	updateHistory(src);
      }

      return this;
    },

    /**
     * Load document from the server.
     */
    load: function(file) {
      if ( file ) {
	var that = this;
	var options = this.data(pluginName);

	$.ajax({ url: config.http.locations.trill_on_swish_web_storage + "/" + file,
		 dataType: "text",
		 success: function(data) {
		   that.xmlEditor('setSource', data);
		   options.file = file;
		 },
		 error: function(jqXHDR, textStatus) {
		   alert("Failed to load document: "+textStatus);
		 }
	       });
      }
      return this;
    },

    /**
     * Save the current document to the server.  Depending on the
     * arguments, this function implements several forms of saving:
     *
     *   - Without arguments arguments, it implements "Save".
     *   - With ("as"), it implements "Save as", which opens a
     *     dialog which calls this method again, but now with
     *     meta-data in the first argument.
     *   - With ({...}) it performs the save operation of "Save as"
     *   - With ({...}, "only-meta-data") it only updates the meta
     *     data on the server.
     *
     * @param {Object} [meta] provides additional meta-information.
     * Currently defined fields are `author`, `email`,
     * `title`, `keywords` and `description`. Illegal fields are ignored
     * by the server.
     * @param {String} [what] If `"only-meta-data"`, only the meta-data
     * is updated.
     */
    save: function(meta, what) {
      var options = this.data(pluginName);
      var url     = config.http.locations.trill_on_swish_web_storage;
      var method  = "POST";
      var data;

      if ( meta == "as" ) {
	this.xmlEditor('saveAs');
	return this;
      }

      if ( options.file &&
	   (!meta || !meta.name || meta.name == options.file) ) {
	url += "/" + encodeURI(options.file);
	method = "PUT";
      }

      if ( what == "only-meta-data" ) {
	meta = trill_on_swish_gitty.reduceMeta(meta, options.meta)
	if ( $.isEmptyObject(meta) ) {
	  alert("No change");
	  return;
	}
	data = { update: "meta-data" };
      } else if ( method == "POST" ) {
	data = { data: this.xmlEditor('getSource'),
		 type: "owl"
	       };
	if ( options.meta ) {			/* rename */
	  data.previous = options.meta.commit;
	}
      } else {
	if ( !options.tos_cm.isClean(options.cleanGeneration) ) {
	  data = { data: this.xmlEditor('getSource'),
		   type: "owl"
		 };
	} else if ( sameSet(options.meta.tags, meta.tags) ) {
	  alert("No change");
	  return;
	}
      }

      if ( meta )
	data.meta = meta;

      $.ajax({ url: url,
               dataType: "json",
	       contentType: "application/json",
	       type: method,
	       data: JSON.stringify(data),
	       success: function(reply) {
		 if ( reply.error ) {
		   alert(JSON.stringify(reply));
		 } else {
		   options.url  = reply.url;
		   options.file = reply.file;
		   options.meta = reply.meta;
		   updateHistory(reply);
		 }
	       },
	       error: function() {
		 alert("Failed to save document");
	       }
	     });

      return this;
    },

    /**
     * Provide a Save As dialog
     */
    saveAs: function() {
      var options = this.data(pluginName);
      var meta    = options.meta||{};
      var editor  = this;
      var update  = Boolean(options.file);
      var fork    = options.meta && meta.symbolic != "HEAD";

      if ( meta.public === undefined )
	meta.public = true;

      function saveAsBody() {
	this.append($.el.form({class:"form-horizontal"},
			      form.fields.fileName(fork ? null: options.file,
						   meta.public),
			      form.fields.title(meta.title),
			      form.fields.author(meta.author),
			      update ? form.fields.commit_message() : undefined,
			      form.fields.tags(meta.tags),
			      form.fields.buttons(
				{ label: fork   ? "Fork program" :
					 update ? "Update program" :
						  "Save program",
				  action: function(ev,data) {
					    console.log(data);
				            editor.xmlEditor('save', data);
					    return false;
				          }
				})));
      }

      form.showDialog({ title: fork   ? "Fork from "+meta.commit.substring(0,7) :
			       update ? "Save new version" :
			                "Save program as",
			body:  saveAsBody
		      });

      return this;
    },

    /**
     * Provide information about the current source in a modal
     * dialog.
     */
    info: function() {
      var options = this.data(pluginName);
      var meta = options.meta;
      var editor = this;
      var title;

      if ( options.meta ) {
	title = $().trill_on_swish_gitty('title', options.meta);
      } else {
	title = "Local source";
      }

      function infoBody() {
	if ( options.meta ) {
	  options.editor = editor;		/* circular reference */
	  this.trill_on_swish_gitty(options);
	} else {
	  this.append($.el.p("The source is not associated with a file. ",
			     "Use ",
			     $.el.b("Save ..."),
			     " to save the source with meta information."
			    ));
	}
      }

      form.showDialog({ title: title,
			body:  infoBody
		      });

      return this;
    },

    /**
     * print the current content of the editor after applying the
     * the CodeMirror mode to it.
     * @param {String} [src] Prolog source to print. Default is to print
     * the content of the editor.
     */
    print: function(src) {
      var pre = $.el.pre({class:"tos_cm-s-neo"});

      if ( !src ) src = this.xmlEditor('getSource');

      CodeMirror.runMode(src, "application/xml", pre);

      function printWithIframe(elem) {
	var iframe = $.el.iframe({src:"about:blank"});
	$("body").append(iframe);
	$("body", iframe.contentWindow.document).append(elem);
	iframe.contentWindow.print();
      }

      $.ajax({ url: "/trill_on_swish/bower_components/codemirror/theme/neo.css",
	       dataType: "text",
	       success: function(data) {
		 printWithIframe($.el.div($.el.style(data),
					  pre));
	       },
	       error: function() {
		 printWithIframe(pre);
		 console.log("ciao");
	       }
             });

      return this;
    },

    /**
     * Manage user preference changes.  Defines preferences are:
     *
     *   - "highlight" -- one of `semantic` or `syntactic`
     *
     * @param {Object} pref describes a preference
     * @param {String} pref.name name of the preference
     * @param {Any}    pref.value value of the preference
     */
    preference: function(pref) {
      var data = this.data(pluginName);

      if ( pref.name == "semantic-highlighting" ) {
	data.tos_cm.setOption("prologHighlightServer",
			  { enabled: pref.value });
      }

      return this;
    },

    /**
     * Highlight a (syntax) error in the source.
     * @param {Object} error
     * @param {String} error.data contains the error message
     * @param {Object} error.location contains the location, providing
     * `line` and `ch` attributes.
     */

    /**
     * Remove all inline messages from the editor
     */

    /**
     * Extract example queries from text.  By   default,  this looks for
     * structured  comment  blocks  labelled   *examples*  and  extracts
     * fragments between `^ *?-` and `.`
     *
     * @param {String} [src] Source to parse. Default is the editor
     * content.
     * @param {Boolean} [inComment=true] if `true`, only process text
     * that is in an *examples* structured comment block
     * @returns {null|Array} Examples extracted from the source code.  If
     * there is _no source_ code, `null` is returned.
     */
    getExamples: function(src, inComment) {
      var source = src ? src : this.xmlEditor('getSource');
      var comments;
      var exlist = [];

      if ( $.trim(source) == "" )
	return null;

      if ( inComment == false )
	comments = [src];
      else
	comments = source.match(/\/\*\* *<?examples>?[\s\S]*?\*\//igm);

      if ( comments ) {
	for(var i=0; i<comments.length; i++) {
	  var exl = comments[i].match(/^ *\?-[\s\S]*?[^-#$&*+./:<=>?@\\^~]\.\s/gm);

	  if ( exl ) {
	    for(var j=0; j<exl.length; j++) {
	      var ex = exl[j].replace(/^ *\?-\s*/, "")
			     .replace(/\s*$/, "")
			     .replace(/\\'/g,"'");
	      exlist.push(ex);
	    }
	  }
	}
      }

      return exlist;
    },

    /**
     * @param {RegExp} re is the regular expression to search for
     * @param {Object} [options]
     * @param {number} [options.max] is the max number of hits to return
     * @returns {Array.object} list of objects holding the matching line
     * content and line number.
     */
    search: function(re, options) {
      var tos_cm      = this.data(pluginName).tos_cm;
      var start   = tos_cm.firstLine();
      var end     = tos_cm.lastLine();
      var matches = [];

      for(var i=start; i<=end; i++) {
	var line = tos_cm.getLine(i);
	if ( line.search(re) >= 0 ) {
	  matches.push({line:i+1, text:line});
	  if ( options.max && options.max === matches.length )
	    return matches;
	}
      }

      return matches;
    },

    /**
     * Go to a given 1-based line number and optionally highlight the
     * match(es).
     *
     * @param {number} line
     * @param {Object} [options]
     * @param {RegExp} [options.regex] If provided, highlight the
     * matches.
     * @param {Boolean} [options.showAllMatches] if `true`, show all
     * matches in the viewport.
     */
    gotoLine: function(line, options) {
      var data = this.data(pluginName);
      var tos_cm   = data.tos_cm;
      var ch   = 0;
      var re;

      function clearSearchMarkers(tos_cm) {
	if ( tos_cm._searchMarkers !== undefined ) {
	  for(var i=0; i<tos_cm._searchMarkers.length; i++)
	    tos_cm._searchMarkers[i].clear();
	  tos_cm.off("cursorActivity", clearSearchMarkers);
	}
	tos_cm._searchMarkers = [];
      }

      line = line-1;
      re   = options.regex;
      clearSearchMarkers(tos_cm);
      options = options||{};

      if ( re ) {
	ch = tos_cm.getLine(line).search(re);
	if ( ch < 0 )
	  ch = 0;
      }

      tos_cm.setCursor({line:line,ch:ch});
      var myHeight = tos_cm.getScrollInfo().clientHeight;
      var coords = tos_cm.charCoords({line: line, ch: 0}, "local");
      tos_cm.scrollTo(null, (coords.top + coords.bottom - myHeight) / 2);

      if ( re ) {
	function markMatches(line, className) {
	  var match;

	  while( (match=re.exec(tos_cm.getLine(line))) ) {
	    tos_cm._searchMarkers.push(
	      tos_cm.markText({line:line,ch:match.index},
			  {line:line,ch:match.index+match[0].length},
			  {className:className,
			   clearOnEnter: true,
			   clearWhenEmpty: true,
			   title: "Search match"
			  }));
	  }
	}

	markMatches(line, "CodeMirror-search-match");
	if ( options.showAllMatches ) {
	  var vp = tos_cm.getViewport();

	  for(var i=vp.from; i<vp.to; i++) {
	    if ( i != line ) {
	      markMatches(i, "CodeMirror-search-alt-match");
	    }
	  }
	}

	if ( tos_cm._searchMarkers.length > 0 )
	  tos_cm.on("cursorActivity", clearSearchMarkers);
      }
    }

  }; // methods

  function updateHistory(reply) {
    var cpath = window.location.pathname;

    if ( cpath != reply.url ) {
      window.history.pushState({location:reply.url},
			       "",
			       reply.url);
      document.title = "TRILL on SWISH -- "
                     + (reply.file ? reply.file
			           : "SWI-Prolog for SHaring");
    }
  }

  window.onpopstate = function(e) {
    if ( e.state ) {
      if ( e.state.location ) {
	window.location =  e.state.location;
      }
    } else
      window.location.reload(true);
  }

  /**
   * The xmlEditor jQuery plugin converts a `<div>` into an code
   * editor based on [CodeMirror](http://codemirror.net)
   *
   * @class xmlEditor
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @example // Create a default Prolog editor
   * $("#editor").xmlEditor();
   * @example // Extract embedded examples
   * $("#editor").xmlEditor('getExamples');
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.xmlEditor = function(method) {
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
		 *	STYLE CONFIGURATION	*
		 *******************************/

/**
 * Include styles provided through the configuration object.
 *
 * @param {Object} style is an object mapping style names into style
 * properties.  The properties are also in an object, linking style
 * names to values.  For example:
 *
 *    ```
 *    { column: {color: "#8b008b},
 *      table:  {color: "#8b008b, "font-weight":"bold"}
 *    }
 *    ```
 */


}); // define
