:- begin_tests(reasoner).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).

:- use_module(library(rdf_owl)).
:- use_module(library(rdf_owl/reasoner)).

:- rdf_register_ns(x,'http://x.org/').

l :-
        rdf_load('tests/svf.ttl').


test_subClassOf(A,B) :-
        (   entailed(subClassOf(A1,B)),
            A1=A
        ->  true
        ;   format(user_error('FAIL: ~w < ~w',[A,B])),
            fail).

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
        forall(entailed(E),writeln(E)),
        test_subClassOf(Forelimb,Limb),
        test_subClassOf(Fingernail,some(PartOf,Hand)),
        test_subClassOf(Finger,some(PartOf,Hand)),
        test_subClassOf(Hand,some(PartOf,Forelimb)),
        test_subClassOf(Finger,some(PartOf,Forelimb)),
        test_subClassOf(Finger1,some(PartOf,Forelimb)),
        test_subClassOf(Finger1,some(PartOf,Limb)),
        
        test_subClassOf(C1,some(Conn,C1)),
        test_subClassOf(C1,some(Conn,C2)),
        test_subClassOf(C1,some(Conn,C3)),
        test_subClassOf(C2,some(Conn,C3)),
        nl.

    
:- end_tests(reasoner).
    
