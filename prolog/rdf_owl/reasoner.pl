:- module(reasoner,
          [entailed/1]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(rdf_owl)).
:- use_module(library(rdf_owl/owl)).

:- use_module(library(tabling)).

:- table entailed/1.


% base case
entailed(subClassOf(C,D)) :-
        direct_subClassOf(C,D).


% base case
entailed(subClassOf(C,some(P,D))) :-
        direct_subClassOf_owl_some(C,P,D).

entailed(subClassOf(C,some(P,D))) :-
        \+ var(C),
        entailed(subClassOf(C,some(P,X))),
        entailed(subClassOf(X,D)),
        \+ compound(D).
entailed(subClassOf(C,some(P,D))) :-
        var(C),
        entailed(subClassOf(X,D)),
        \+ compound(D),
        \+ compound(X),
        entailed(subClassOf(C,some(P,X))).

% subPropertyOf
entailed(subClassOf(C,some(P,D))) :-
        entailed(subClassOf(C,some(P1,D))),
        subPropertyOf(P1,P).

% property chains and transitivity
entailed(subClassOf(C,some(P,D))) :-
        entailed(subClassOf(C,some(P1,X))),
        %direct_subClassOf_owl_some(C,P1,X),
        property_chain(P,P1,P2),
        entailed(subClassOf(X,some(P2,D))).

% transitivity
entailed(subClassOf(C,D)) :-
        \+ var(C),
        entailed(subClassOf(C,X)),
        entailed(subClassOf(X,D)).
entailed(subClassOf(C,D)) :-
        var(C),
        entailed(subClassOf(X,D)),
        entailed(subClassOf(C,X)).

:- table direct_subClassOf_owl_some/2.
direct_subClassOf_owl_some(C,P,D) :-
        \+ compound(C),
        \+ compound(D),
        subClassOf(C,Restr),
        owl_some(Restr,P,D).

:- table direct_subClassOf/2.
direct_subClassOf(C,D) :-
        \+ compound(C),
        \+ compound(D),
        subClassOf(C,D),
        \+rdf_is_bnode(C),
        \+rdf_is_bnode(D).

% NOTE: order last to avoid early binding
% reflexive case
entailed(subClassOf(C,C)) :- \+ compound(C), is_cls(C).

:- table is_cls/1.
is_cls(C) :- class(C).
is_cls(C) :- subClassOf(C,_),\+rdf_is_bnode(C).
is_cls(C) :- subClassOf(_,C),\+rdf_is_bnode(C).

:- table property_chain/3.
% TODO Len>2
property_chain(P,P,P) :-
        transitiveProperty(P).
property_chain(P,P1,P2) :-
        rdf(P,owl:propertyChainAxiom,RL),
        rdf_list(RL,[P1,P2]).
