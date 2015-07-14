// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

  CodeMirror.commands.prologFireQuery = function(tos_cm) {
    var start = tos_cm.getCursor("start");
    var token = tos_cm.getTokenAt(start, true);

    if ( token.type == "fullstop" )
      return tos_cm.prologFireQuery(tos_cm.getValue());

    return CodeMirror.Pass;
  }

  CodeMirror.defineOption("prologQuery", null, function(tos_cm, func, prev) {
    if (prev && prev != CodeMirror.Init)
      tos_cm.removeKeyMap("prologQuery");
    if ( typeof(func) == "function" ) {
      var map = { name:     "prologQuery",
		  "Enter":  "prologFireQuery"
		};
      tos_cm.addKeyMap(map);
      tos_cm.prologFireQuery = func;
    }
  });
});
