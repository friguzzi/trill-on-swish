MAIN=trill_on_swish.css
CSS=$(MAIN) answer.css editor.css navbar.css pane.css query.css \
	    runner.css term.css

trill_on_swish-min.css: $(CSS)
	cleancss -o $@ $(MAIN)
	swipl css-inline.pl $@
	gzip -fk $@

clean::
	rm -f trill_on_swish-min.css trill_on_swish-min.css.gz
