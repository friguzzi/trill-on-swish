:-use_module(library(trill)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- endif.

:- trill. % or :- trillp. or :- tornado.

/** <examples>

?- prob_instanceOf(goodPerson,donVito,Prob).

*/

classAssertion(cat, tom).
propertyAssertion(hasPet, donVito, tom).
subClassOf(cat, pet).
subClassOf(someValuesFrom(hasAnimal, pet), natureLover).
subClassOf(natureLover,goodPerson).
subPropertyOf(hasPet,hasAnimal).
 
annotationAssertion('disponte:probability',subClassOf(natureLover,goodPerson),literal('0.2')).
 