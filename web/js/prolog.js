/**
 * @fileOverview
 *
 * RequireJS module providing some general support methods for accessing
 * Prolog
 *
 * @version 0.1.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

/* Also depends on "editor", but requireJS cannot handle cyclic dependencies.
   As downloadCSV() is only called after initialisation we dropped this
   dependency.
*/

define([ "jquery", "config", "form", "preferences",
	 /* "editor" */
       ],
       function($, config, form, preferences) {
  var prolog = {
    /**
     * Download query results as CSV.
     * @param {Object} [options]
     * @param {String} [options.projection] holds the Prolog projection
     * variables, separated by commas, e.g., `"X,Y"`
     * @param {String} [options.format="prolog"] holds a string that
     * defines the variation of the CSV format, e.g., `"prolog"` or
     * `"rdf"`
     * @param {String|Number} [options.limit] defines the max number of
     * results.
     * @param {Boolean} [options.distinct] requests only distinct
     * results.
     * @param {String} [options.disposition] provides the default for
     * the download file.  If no extension is given, ".csv" is added.
     */
    downloadCSV: function(query, source, options) {
      options = options||{};
      options.disposition = (options.disposition||
			     options.filename||
			     "swish-results.csv");

      if ( options.projection ) {
	var formel;
	var format = options.format||"prolog";

	function attr(name,value) {
	  return $.el.input({type:"hidden", name:name, value:value});
	}

	if ( options.distinct )
	  query = "distinct(["+options.projection+"],("+query+"))";
	if ( options.limit ) {
	  var limit = parseInt(options.limit.replace(/[ _]/g,""));

	  if ( typeof(limit) == "number" ) {
	    query = "limit("+limit+",("+query+"))";
	  } else {
	    alert("Not an integer: ", options.limit);
	    return false;
	  }
	}

	formel = $.el.form({ method:"POST",
                             action:config.http.locations.pengines+"/create",
			     target:"_blank"
		           },
			   attr("format", "csv"),
			   attr("chunk", "10"),
			   attr("solutions", "all"),
			   attr("disposition", options.disposition),
			   attr("application", "swish"),
			   attr("ask", query),
			   attr("src_text", source),
			   attr("template", format+"("+options.projection+")"));
	console.log(formel);
	$("body").append(formel);
	formel.submit();
	$(formel).remove();
      } else {
	var vars = $().prologEditor('variables', query);
	var disposition = options.disposition;
	if ( disposition.indexOf(".") < 0 )
	  disposition += ".csv";

	function infoBody() {
	  var formel = $.el.form(
            {class:"form-horizontal"},
	    form.fields.projection(vars.join(",")),
	    form.fields.csvFormat(config.swish.csv_formats,
				  preferences.getVal("csvFormat")),
	    form.fields.limit("10 000", false),
	    form.fields.filename(disposition, 2),
	    form.fields.buttons(
	      { label: "Download CSV",
		action: function(ev, params) {
		  ev.preventDefault();
		  if ( config.swish.csv_formats.length > 1 )
		    preferences.setVal("csvFormat", params.format);
		  prolog.downloadCSV(query, source, params);

		  return false;
		}
	      }));
	  this.append(formel);
	}

	form.showDialog({ title: "Download query results as CSV",
			  body:  infoBody
		        });
      }

      return this;
      },

    /**
     * Remove the full-stop from a query string
     */
    trimFullStop: function(s) {
      return s.replace(/\.\s*$/m, "");
    },

    /**
     * Default options for $.swish()
     */
    options: {
      application: "swish",
      chunk: 5
    }
  }

		 /*******************************
		 *	     PENGINES		*
		 *******************************/

  /**
   * $.swish(options) creates a new Pengine with given default
   * options.  The default options are determined by `prolog.options`.
   * This function expects pengines.js to be already loaded.  The
   * bootstrapping of that is achieved in `swish.js`.
   *
   * @return {Pengine} the created pengine object
   */
  $.swish = function(options) {
    for(var opt in prolog.options) {
      if ( prolog.options.hasOwnProperty(opt) &&
	   !options.hasOwnProperty(opt) ) {
	options[opt] = prolog.options[opt];
      }
    }

    return new Pengine(options);
  };

  return prolog;
});

