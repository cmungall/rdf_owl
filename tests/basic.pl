:- begin_tests(basic).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).

:- use_module(library(rdf_owl)).
:- use_module(library(rdf_owl/owl)).

:- rdf_register_ns(x,'http://x.org/').

l :-
        rdf_load('tests/svf.ttl').


test(reasoner) :-
        l,
        rdf_global_id(x:fingernail,Fingernail),
        rdf_global_id(x:finger,Finger),
        rdf_global_id(x:hand,Hand),
        rdf_global_id(x:forelimb,Forelimb),
        rdf_global_id(x:toe,Toe),
        rdf_global_id(x:limb,Limb),
        rdf_global_id(x:c1,C1),
        rdf_global_id(x:c2,C2),
        rdf_global_id(x:c3,C3),
        rdf_global_id(x:'connected-to',Conn),
        rdf_global_id(x:'part-of',PartOf),
        subClassOf(Forelimb,Limb),
        subClassOf(Hand,R),
        owl_some(R,PartOf,Forelimb),
        nl.

    
:- end_tests(basic).
    
