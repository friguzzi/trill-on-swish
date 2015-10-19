/**
 * @fileOverview
 * Deal with subwindow layout
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "splitter" ],
       function() {

(function($) {
  var pluginName = 'tile';

  /** @lends $.fn.tile */
  var methods = {
    /**
     * @param {Object} [options] currently ignored
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var dir   = elem.hasClass("horizontal") ? "vertical" : "horizontal";
	var pos   = elem.attr("data-split");
	var panes = elem.children();

	if ( !pos ) pos = "50%";

	panes.each(function() {
	  $(this).wrap('<div class="pane-wrapper"></div>')
	});
	elem.addClass("pane-container");
	elem.split({ orientation:dir,
	             position:pos,
		     limit:10,
		     onDrag: function(ev) { panes.trigger("pane.resize"); },
		     onDragEnd: function() { elem.tile('resize_save'); }
	           });
	elem.tile('resize_save');
      });
    },

    /**
     * Save the current split location as a percentage, so we can
     * maintain this percentage at subsequent resize events.  This
     * is normally called after establishing the tile and after a
     * user-initiated resize.
     */
    resize_save: function() {
      this.each(function() {
	var elem    = $(this);
	var info    = paneInfo(elem);
	var length, pos;

	if ( info.splitter.orientation == 'horizontal' ) {
	  length = elem.height();
	  pos    = $(info.first).height();
	} else {
	  length = elem.width();
	  pos    = $(info.first).width();
	}

	var percent = Math.round(((100 * pos) / length)) + "%";

	info.splitter.resizestart = percent;
      });

      this.find(".reactive-size").trigger("reactive-resize");
      return this;
    },

    /**
     * Act on a resize by keeping the relative distribution and respect
     * min/max style properties. This assumes that {@link resizestart}
     * is called at the start of the windowresize.
     * @example $(window).resize(function() { $(".tile").tile('resize'); });
     */
    resize: function() {
      return this.each(function() {
	var elem     = $(this);
	var splitter = elem.split();

	if ( splitter.resizestart ) {
	  splitter.position(splitter.resizestart);
	  splitter.settings.onDrag(elem);
	}
      });
    }
  }; // methods

  function paneInfo(pane) {
    var panes = pane.children();

    return { splitter: pane.split(),
             first:    $(panes[0]).children()[0],
	     second:   $(panes[2]).children()[0]
           };
  }

  /**
   * Generate a tiled subwindow layout from a hierarchy of `<div>`
   * elements.  Below is the HTML that creates the SWISH 2.0 subwindow
   * layout.  This plugin uses the class `horizontal` or `vertical` to
   * decide on the direction of the split and the attribute `data-split`
   * to locate the split location.
   *
   *     <div class="tile horizontal" data-split="60%">
   *       <div class="prolog-editor"></div>
   *       <div class="tile vertical" data-split="70%">
   *         <div class="prolog-runners"></div>
   *         <div class="prolog-query"></div>
   *       </div>
   *     </div>
   *
   * @class tile
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @example $(".tile").tile();
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.tile = function(method) {
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Stuff left here for future addition and deletion of tiles subwindows.
Must be turned into a plugin.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

function addPane(relto, pane, rel)
{ var parent = relto.wrap('<div class="pane-container"></div>').parent();

  if ( rel == "above" || rel == "left" )
    parent.prepend(pane);
  else
    parent.append(pane);

  relto.wrap('<div class="pane-wrapper"></div>');
  pane.wrap('<div class="pane-wrapper"></div>');

  if ( rel == "above" || rel == "below" )
    dir = "horizontal";
  else
    dir = "vertical";

  parent.split({orientation:dir, limit:10});
}


/* closePane() removes a pane and the accompagnying splitter from
   the DOM.  Note that .split() on a splitted div returns the splitter
   object. After removing this, we are left with our two wrapper layers.
*/

function closePane(pane)
{ var splitContainer = pane.parents(".pane-container").first();

  splitContainer.split().destroy();
  pane.parent().remove();
  splitContainer.children().first().children().first().unwrap().unwrap();
}

});
