(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  var templatesMap = [];
  var Pos = CodeMirror.Pos;

  function startsWith(str, token) {
    return str.slice(0, token.length).toUpperCase() == token.toUpperCase();
  }

  function DEBUG(topic) {
    //console.log.apply(null, Array.slice(arguments, 1));
  }

  CodeMirror.templatesHint = {};

  function getLabel(proposal) {
    var template = proposal.template;
    return document.createTextNode(template.name);
  }

  var ourMap = {
    Tab : selectNextVariable,
    Enter : function(tos_cm) { selectNextVariable(tos_cm, true) },
    Esc : uninstall,
    "Ctrl-Space": hintValue
  }

  function TemplateState() {
    this.marked = [];
    this.selectableMarkers = [];
    this.varIndex = -1;
  }

  function isNested(tos_cm) {
    return tos_cm._templateStack ? tos_cm._templateStack.length : 0;
  }


  // A Template instance represents an autocompletion template.
  // It can be parsed from an eclipse-type template string,
  // or supplied with a pre-parsed token array.
  //
  // The token array may consist of the following tokens:
  //   "\n" (newline character)
  //       Single newline character per token.
  //   text (string)
  //       Normal text, no newline characters allowed.
  //   { variable: "name" }
  //       Variable token, to be populated by the user.
  //   { cursor: true }
  //       The cursor will be placed here after completing the template
  //   { line_separator: true }
  //       If the template surrounds existing text, the existing text will be
  //       placed here. Not implemented currently.
  function Template(data) {
    this.name = data.name; // Optional
    this.description = data.description; // Optional
    this.text = data.text; // Optional
    if ( data.varTemplates ) {
      this.varTemplates = data.varTemplates;
    }
    if(data.template != null) {
      this.source = data.template;
    } else if(data.tokens != null) {
      this._tokens = data.tokens;
    }
  }

  Template.prototype.tokens = function() {
    if(this._tokens == null) {
      this._tokens = parseTemplate(this.source);
    }
    return this._tokens;
  };

  Template.prototype.content = function() {
    if(this._content == null) {
      var tokens = this.tokens();
      var content = '';
      for ( var i = 0; i < tokens.length; i++) {
        var token = tokens[i];
        if (typeof token == 'string') {
          content += token;
        } else if (token.variable) {
          content += token.variable;
        } else {
          // Ignore special tokens
        }
      }
      this._content = content;
    }
    return this._content;
  };

  function parseTemplate(content) {
    var tokens = [];
    var varParsing = false;
    var last = null;
    var token = '';
    for ( var i = 0; i < content.length; i++) {
      var current = content.charAt(i);
      if (current == "\n") {
        if (token != '') {
          tokens.push(token);
        }
        token = '';
        tokens.push(current);
        last = null;
      } else {
        var addChar = true;
        if (varParsing) {
          if (current == "}") {
            varParsing = false;
            addChar = false;
            if(token == 'cursor') {
              tokens.push({
                "cursor" : true
              });
            } else if(token == 'line_selection') {
              tokens.push({
                "line_selection" : true
              });
            } else {
              tokens.push({
                "variable" : token
              });
            }
            token = '';
          }
        } else {
          if (current == "$" && (i + 1) <= content.length) {
            i++;
            var next = content.charAt(i);
            if (next == "{") {
              varParsing = true;
              addChar = false;
              if (token != '') {
                tokens.push(token);
              }
              token = '';
            }
          }

        }
        if (addChar && last != "$") {
          token += current;
          last = current;
        } else {
          last = null;
        }
      }
    }
    if (token != '') {
      tokens.push(token);
    }
    return tokens;
  }


  function getMarkerChanged(tos_cm, textChanged) {
    var markers = tos_cm.findMarksAt(textChanged.from);
    if (markers) {
      for ( var i = 0; i < markers.length; i++) {
        var marker = markers[i];
        if (marker._templateVar) {
          return marker;
        }
      }
    }
    return null;
  }

  /**
   * Track changes.  If the change is outside any template variable,
   * uninstall() the template editing.  If it is inside, see whether
   * there are any other template variables with the same name and
   * update them accordingly.
   */
  function onChange(tos_cm, textChanged) {
    var state = tos_cm._templateState;
    if (!textChanged.origin || !state || state.updating) {
      return;
    }
    try {
      state.updating = true;
      var markerChanged = getMarkerChanged(tos_cm, textChanged);
      if (markerChanged == null) {
        uninstall(tos_cm);
      } else {
        var posChanged = markerChanged.find();
        var newContent = tos_cm.getRange(posChanged.from, posChanged.to);
        for ( var i = 0; i < state.marked.length; i++) {
          var marker = state.marked[i];
          if (marker != markerChanged
              && marker._templateVar == markerChanged._templateVar) {
            var pos = marker.find();
            tos_cm.replaceRange(newContent, pos.from, pos.to);
          }
        }
      }
    } finally {
      state.updating = false;
    }
  }

  function onEndCompletion(tos_cm) {
    DEBUG("template", "endCompletion()", isNested(tos_cm));
    if ( isNested(tos_cm) )
      uninstall(tos_cm, true);
  }

  function selectNextVariable(tos_cm, exitOnEnd) {
    var state = tos_cm._templateState;
    if (state.selectableMarkers.length > 0) {
      state.varIndex++;
      if (state.varIndex >= state.selectableMarkers.length) {
        // If we reach the last token and exitOnEnd is true, we exit instead of
        // looping back to the first token.
        if (exitOnEnd) {
          exit(tos_cm);
          return;
        }
        state.varIndex = 0;
      }
      var marker = state.selectableMarkers[state.varIndex];
      var pos = marker.find();
      tos_cm.setSelection(pos.from, pos.to);
      var templateVar = marker._templateVar;
      for ( var i = 0; i < state.marked.length; i++) {
        var m = state.marked[i];
        if (m == marker) {
          m.className = "";
          m.startStyle = "";
          m.endStyle = "";
        } else {
          if (m._templateVar == marker._templateVar) {
            m.className = "CodeMirror-templates-variable-selected";
            m.startStyle = "";
            m.endStyle = "";
          } else {
            m.className = "CodeMirror-templates-variable";
            m.startStyle = "CodeMirror-templates-variable-start";
            m.endStyle = "CodeMirror-templates-variable-end";
          }
        }
      }
      tos_cm.refresh();
    } else {
      // No tokens - exit.
      exit(tos_cm);
    }
  }

  /**
   * Recursively use hinting for the values
   */
  function hintValue(tos_cm) {
    var state  = tos_cm._templateState;
    var marker = state.selectableMarkers[state.varIndex];
    var prev   = {state:state};

    if ( tos_cm._hintTemplateMarker )
      prev.marker = tos_cm._hintTemplateMarker;

    if ( !tos_cm._templateStack )
      tos_cm._templateStack = [];
    tos_cm._templateStack.push(prev);
    delete tos_cm._templateState;

    function samePos(p1, p2) {
      return p1.ch == p2.ch && p1.line == p2.line;
    }

    tos_cm._hintTemplateMarker = marker;
    var pos = marker.find();
    var sels = tos_cm.listSelections();
    if ( sels.length == 1 &&
	 samePos(sels[0].anchor, pos.from) &&
	 samePos(sels[0].head,   pos.to) ) {
      tos_cm.replaceRange("\u2630", pos.from, pos.to);
    }

    CodeMirror.commands.autocomplete(tos_cm);
  }

  Template.prototype.insert = function(tos_cm, data) {
    var template = this;
    var nested = isNested(tos_cm);

    DEBUG("template", "Insert, nested", nested, "template", template);
    if ( tos_cm._templateState || nested ) {
      DEBUG("template", "Uninstall from insert()", nested);
      uninstall(tos_cm);
    }

    if ( template.text ) {
      tos_cm.replaceRange(template.text, data.from, data.to);
      return;
    }

    var state = new TemplateState();
    tos_cm._templateState = state;

    var tokens = this.tokens();
    var content = '';
    var line = data.from.line;
    var col = data.from.ch;
    var markers = [];
    var variables = [];
    var cursor = null;
    for ( var i = 0; i < tokens.length; i++) {
      var token = tokens[i];
      if(typeof token == 'string') {
        content += token;
        if (token == "\n") {
          line++;
          col = 0;
        } else {
          col += token.length;
        }
      } else if (token.variable) {
        content += token.variable;
        var from = Pos(line, col);
        var to = Pos(line, col
            + token.variable.length);
        var selectable = variables[token.variable] != false;
        col += token.variable.length;
        markers.push({
          from : from,
          to : to,
          variable : token.variable,
          selectable : selectable
        });
        variables[token.variable] = false;
      } else if(token.cursor) {
        cursor = Pos(line, col);
      } else {
        // Unhandled tokens, e.g. line_selection. Ignore.
      }
    }

    var from = data.from;
    var to = data.to;
    var startLine = from.line;
    tos_cm.replaceRange(content, from, to);

    for ( var i = 0; i < markers.length; i++) {
      function subTemplate(tvar) {
	if ( template.varTemplates && template.varTemplates[tvar] )
	  return template.varTemplates[tvar];
	return undefined;
      }

      var marker = markers[i], from = marker.from, to = marker.to;
      var markText = tos_cm.markText(from, to, {
        className : "CodeMirror-templates-variable",
        startStyle : "CodeMirror-templates-variable-start",
        endStyle : "CodeMirror-templates-variable-end",
        inclusiveLeft : true,
        inclusiveRight : true,
        clearWhenEmpty: false,  // Works in CodeMirror 4.6
        _templateVar : marker.variable,
	_templates : subTemplate(marker.variable)
      });
      state.marked.push(markText);
      if (marker.selectable == true) {
        state.selectableMarkers.push(markText);
      }
    }

    if (cursor != null) {
      state.cursor = tos_cm.setBookmark(cursor);
    }

    // Auto-indent everything except the first line.
    // This will typically indent the rest of the code according
    // to the indentation of the first line.
    // We do the indentation after creating the markers, so that the
    // markers are moved accordingly.
    var lines = content.split("\n");
    for ( var x = 1; x < lines.length; x++) {
      var targetLine = startLine + x;
      tos_cm.indentLine(targetLine);
    }

    // Have to be before selectNextVariable, since selectNextVariable
    // may exit and remove the keymap again.
    if ( !nested ) {
      tos_cm.on("change", onChange);
      DEBUG("template", "Installing endCompletion");
      tos_cm.on("endCompletion", onEndCompletion);
      tos_cm.addKeyMap(ourMap);
    }

    selectNextVariable(tos_cm, true);
  }

  function exit(tos_cm) {
    // Move to ${cursor} in the template, then uninstall.
    var cursor = tos_cm._templateState.cursor;
    if (cursor != null) {
      var cursorPos = cursor.find();
      if (cursorPos != null) {
        tos_cm.setSelection(cursorPos, cursorPos);
      }
    }
    uninstall(tos_cm);
  }

  function uninstall(tos_cm, canceled) {
    var state = tos_cm._templateState;

    function canceledMarker() {
      DEBUG("template", "Canceled?");

      for ( var i = 0; i < state.marked.length; i++) {
	var mark = state.marked[i];
	if ( mark == tos_cm._hintTemplateMarker ) {
	  var pos = mark.find();
	  if ( pos && tos_cm.getRange(pos.from, pos.to) == "\u2630" )
	    tos_cm.replaceRange(mark._templateVar, pos.from, pos.to);
	}
      }
    }

    if ( state ) {
      DEBUG("template", "Uninstall, clearing: ", state.marked.length);
      for ( var i = 0; i < state.marked.length; i++) {
	state.marked[i].clear();
      }
      if (state.cursor != null) {
	state.cursor.clear();
      }
      state.marked.length = 0;
      state.selectableMarkers.length = 0;
    } else {
      DEBUG("template", "Uninstall, no state");
    }

    if ( tos_cm._templateStack && tos_cm._templateStack.length > 0 ) {
      DEBUG("template", "Popping from level", tos_cm._templateStack.length);
      var prev = tos_cm._templateStack.pop();
      state = tos_cm._templateState = prev.state;
      if ( canceled && tos_cm._hintTemplateMarker )
	canceledMarker();
      if ( prev.marker ) {
	tos_cm._hintTemplateMarker = prev.marker;
      } else {
	delete tos_cm._hintTemplateMarker;
      }
    } else {
      DEBUG("template", "Leaving template mode");
      tos_cm.off("change", onChange);
      tos_cm.off("endCompletion", onEndCompletion);
      tos_cm.removeKeyMap(ourMap);
      delete tos_cm._templateState;
      delete tos_cm._hintTemplateMarker;
    }
  }

  CodeMirror.templatesHint.getCompletions = function(tos_cm, completions, text) {
    var mode = tos_cm.doc.mode.name;
    var list = templatesMap[mode];
    if (list) {
      for ( var i = 0; i < list.length; i++) {
        var template = list[i];
        if (startsWith(template.name, text)) {
          var label = template.name;
          if (template.description) {
            label += '- ' + template.description;
          }
          var className = "CodeMirror-hint-template";
          if (template.className)
            className = template.className;
          var completion = {
            "className" : className,
            "text" : label,
            "template" : template,
          };
          completion.data = completion;
          completion.hint = function(tos_cm, data, completion) {
            completion.template.insert(tos_cm, data);
          };
          completion.info = function(completion) {
            var content = completion.template.content();

            if (CodeMirror.runMode) {
              var result = document.createElement('div');
              result.className = 'cm-s-default';
              if (tos_cm.options && tos_cm.options.theme)
                result.className = 'cm-s-' + tos_cm.options.theme;
              CodeMirror.runMode(content, tos_cm.getMode().name, result);
              return result;
            }
            return content;
          };
          completions.push(completion);
        }
      }
    }
  }

  CodeMirror.templatesHint.Template = Template;

  CodeMirror.templatesHint.addTemplates = function(templates) {
    var context = templates.context;
    if (context) {
      var list = templatesMap[context];
      if (!list) {
        list = [];
        templatesMap[context] = list;
      }
      templates.templates.forEach(function(template) {
        list.push(new Template(template));
      });
    }
  }

});

