JS=editor.js navbar.js answer.js pane.js query.js runner.js trill_on_swish.js \
   config.js jswish.js search.js history.js preferences.js tabbed.js storage.js \
   notebook.js
CM=$(shell find codemirror -name '*.js')
JQ=jquery.jsdoc
CONF=conf.json
MAIN=trill_on_swish.js

all:	min require.js

doc::	$(CONF) $(JQ) $(JS)
	-jsdoc -p -c $(CONF) $(JQ) $(JS)

min:	trill_on_swish-min.js trill_on_swish-min.js.gz

trill_on_swish-min.js:	build.js $(JS) $(CM)
	r.js -o build.js

trill_on_swish-min.js.gz: trill_on_swish-min.js
	gzip < $< > $@

build.js: $(MAIN) build.js.in
	swipl mkjsbuild.pl $(MAIN) build.js.in

require.js: ../bower_components/requirejs/require.js
	cp -p $< $@

clean:
	rm -f require.js trill_on_swish-min.js trill_on_swish-min.js.gz
