:- module(rdf_owl,
          [owl_some/3,
           owl_all/3]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(rdf_owl/owl)).
:- use_module(library(regex)).

:-op(300,xfy,some).
:-op(300,xfy,all).

%! owl_some(?Restr, ?Property, ?Obj) is nondet.
%
% true if Restr is an OWL expression SomeValuesFrom(Property,Obj)
owl_some(R,P,O) :-
        onProperty(R,P),
        someValuesFrom(R,O).

%! owl_all(?Restr, ?Property, ?Obj) is nondet.
%
% true if Restr is an OWL expression AllValuesFrom(Property,Obj)
owl_all(R,P,O) :-
        onProperty(R,P),
        allValuesFrom(R,O).

owl_restriction(R, some(P,O)) :-
        onProperty(R,P),
        someValuesFrom(R,O).

owl_restriction(R, all(P,O)) :-
        onProperty(R,P),
        someValuesFrom(R,O).





