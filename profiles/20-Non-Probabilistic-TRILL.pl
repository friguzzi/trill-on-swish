% TRILL loaded with non probabilistic KB
:- use_module(library(trill)).

:- trill.

owl_rdf('<?xml version="1.0"?>

<!--

/** <examples>

Here examples of the form
?- instanceOf(\'className\',\'indName\',Prob).

*/
-->

<!DOCTYPE rdf:RDF [
    <!ENTITY owl "http://www.w3.org/2002/07/owl#" >
    <!ENTITY xsd "http://www.w3.org/2001/XMLSchema#" >
    <!ENTITY rdfs "http://www.w3.org/2000/01/rdf-schema#" >
    <!ENTITY rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#" >
]>


<rdf:RDF xmlns="http://here.the.IRI.of.your.ontology#"
     xml:base="http://here.the.IRI.of.your.ontology"
     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     xmlns:owl="http://www.w3.org/2002/07/owl#"
     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#">
    <owl:Ontology rdf:about="http://here.the.IRI.of.your.ontology"/>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Your Axioms Here
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

</rdf:RDF>
').

/****************************
 * Other axioms here
 ****************************/
