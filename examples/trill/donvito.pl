:-use_module(library(trill)).

:- trill. % or :- trillp. or :- tornado.

classAssertion(cat, tom).
propertyAssertion(hasPet, donVito, tom).
subClassOf(cat, pet).
subClassOf(someValuesFrom(hasAnimal, pet), natureLover).
subClassOf(natureLover,goodPerson).
subPropertyOf(hasPet,hasAnimal).
 
annotationAssertion('disponte:probability',subClassOf(natureLover,goodPerson),literal('0.2')).
 