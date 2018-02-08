:- module(rdf_nlp,
          [
           mutate_atom/4
          ]).

:- use_module(library(porter_stem)).


%! tokenize_atom_wrap(+Atom:atom, ?Tokens:list) is det
%
% wraps tokenize_atom/2
tokenize_atom_wrap(A,TL) :-
        tokenize_atom(A,TL_1),
        findall(T,(member(T,TL_1),\+ignore_token(T)),TL).

ignore_token(',').
ignore_token('.').
ignore_token('-').
ignore_token(';').
ignore_token(':').
ignore_token('_').


%! atom_token(+Atom:atom, ?Token:atom) is nondet
%
% true if Token is a member of the tokens for Atom
atom_token(A,T) :-
	tokenize_atom_wrap(A,TL),
	member(T,TL).

atom_token_stemmed(A,T) :-
	atom_token_stemmed(A,T,true).

%! atom_token(+Atom:atom, ?Token:atom, ?Stemmed:bool) is nondet
%
% true if Token is a member of the tokens for Atom, with or without stemming
atom_token_stemmed(A,T,true) :-
        % SPECIAL CASE: acronym
        \+ ((sub_atom(A,_,1,_,C),
             C@>='a',
             C@=<'z')),
        !,
        % we still convert to lowercase for matching purposes,
        % but we don't stem.
        % this is still not ideal: someone can search with
        % an acronym, but the porter stemming part may have truncated the acronym
        % already, so best to stem anyway??
        % ideally this would be a hook in the porter stemming..
        downcase_atom(A,A_dn),
        atom_token_stemmed(A_dn,T,false).
atom_token_stemmed(A,T,true) :-
        % not acronym
	atom_token(A,T1),
	custom_porter_stem(T1,T).
atom_token_stemmed(A,T,false) :-
        downcase_atom(A,A2),
	atom_token(A2,T).

custom_porter_stem(T,S) :-
        atom_concat(X,eous,T),
        atom_concat(X,eus,T2),
        !,
        porter_stem(T2,S).
custom_porter_stem(T,S) :-
        porter_stem(T,S).




% HOOK
:- multifile exclude_entity/1.

%% atom_nlabel_stemmed(+Term:atom,?NLabel:atom,+Stemmed:bool) 
%
% translates a term such as 'foo bar two hippocampi' to a label like
% '2barfoohippocamp'
% this is used for exact matching, but is not useful for finding labels
% nested inside larger labels or blocks of text.
%
% canonical ordering  fails in certain cases: e.g. 'chordo neural hinge' vs 'chordoneural hinge',
% so we also provide the same ordering too
atom_nlabel_stemmed(Term,NLabel,St) :-
        atom_tokenset_stemmed(Term,Toks,St),
        debug(nlp,'  term:~w ==> ~w',[Term,Toks]),
	concat_atom(Toks,'',NLabel).
% original ordering
atom_nlabel_stemmed(Term,NLabel,St) :-
	dehyphenate(Term,TermNoHyphen), % nd
        (   St=true;St=false),
        findall(Tok,atom_token_stemmed(TermNoHyphen,Tok,St),Toks_1),
        % both with replacements ( if different) and without
	(   maplist(token_syn_refl,Toks_1,Toks_2),
            Toks_1\=Toks_2,
            debug(nlp,'    toks[syn]:~w ==> ~w',[Toks_1,Toks_2]),
            concat_atom(Toks_2,'',NLabel)
        ;   concat_atom(Toks_1,'',NLabel)).

dehyphenate(X,X).
dehyphenate(X,Y) :- concat_atom(L,'-',X),L\=[_],concat_atom(L,'',Y).


:- multifile synset_hook/1.

synset(['1','1st',first,'i','01']).
synset(['2','2nd',second,'ii','02']).
synset(['3','3rd',third,'iii','03']).
synset(['4','4th',fourth,'iv','04']).
synset(['5','5th',fifth,'v','05']).
synset(['6','6th',sixth,'vi','06']).
synset(['7','7th',seventh,'vii','07']).
synset(['8','8th',eight,'viii','08']).
synset(['9','9th',ninth,'ix','09']).
synset(['10','10th',tenth,'x']).
synset(['11','11th',eleventh,'xi']).
synset(['12','12th',twelfth,'xii']).
synset(['13','13th',thirteenth,'xiii']).
synset(['14','14th',fourteenth,'xiv']).
synset(['15','15th',fifteenth,'xv']).
synset(['16','16th',sixteenth,'xvi']).
synset(['17','17th',seventeenth,'xvii']).
synset(['18','18th',eighteenth,'xviii']).
synset(['19','19th',nineteenth,'xviv']).
synset(['20','20th',twentyth,'xx']).

synset([absent,absence]).
synset([cavity,lumen]).

synset([caudal,posterior]).
synset([rostral,anterior]).
synset([dorsal,superior]).
synset([ventral,inferior]).
synset([future,presumptive]).
synset([division,segment]).

% TODO: make this configurable, otherwise too many false positives
synset(['S',sacral]).
synset(['L',lumbar]).
synset(['T',thoracic]).
synset(['C',cervical]).


% eliminate prepositions. assumes we flatten without spaces
synset(['',P]) :- prep(P).
%%%synset(['',a]).
synset(['',an]).
synset(['',the]).
synset(['','-']).
synset(['','/']).
synset(['',':']).

% eliminate filler-words
synset(['',region]).
synset(['',structure]).



synset(X) :- synset_hook(X).

prep(of).
prep(to).
prep(by).


/*
  NONDET
% consider indexing
token_syn(T,S) :- synset(L),member(T,L),member(S,L),T\=S.
token_syn(T,S) :- relational_adj(T,S,_,_).
token_syn(T,S) :- relational_adj(S,T,_,_).
*/

% DET
%token_syn(T,S) :- relational_adj(T,S,_,_),!.   % ensure noun-form is used preferentially
token_syn(T,S) :- synset([S|L]),member(T,L). % normalize to 1st member of synset

% reflexive
token_syn_refl(T,S) :- token_syn(T,S),!.
token_syn_refl(T,T) :- nonvar(T).


mutate_atom(stem,_,V,V2) :-
        custom_porter_stem(V,V2).
mutate_atom(downcase,_,V,V2) :-
        downcase_atom(V,V2).


entity_mutated_property_value(C,T,P,V,V2) :-
        namepred(P), % TODO
        rdf(C,P,Lit),
        ensure_atom(Lit,V),
        mutate_atom(T,P,V,V2).
