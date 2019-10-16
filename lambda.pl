% Author: Jonas A. Vander

% This is a library for (pseudo) lambda predicates in prolog

% The desired notation to get such a lambda predicate is:
% make_lambda(Expression, OutputPredicate)
% where the Expression part is of this shape:
% Expression = (Arguments :- YourRules)
% where Arguments is a list of arguments as in :
%  Arguments =   [PartA,PartB,PartC...]
% or if it is only one, it can be written by itself
%  Arguments =   OnlyArgument
% and where the YourRules part is an arbitrary set
% of other predicates containing your Parts
% If it is more than one rule you will need brackets around them,
% in order to use commas correctly

% If needed the reference of the created rule can be extracted as well,
% using:
%       make_lambda(Expression,Predicate,Reference)
% that way the rule can be removed from memory at will using:
%       clear_lambda(Reference)

% When you are save that no lambda predicates currently defined will
% be used any more, call flush_lambda, or flush_lambda_log.


:- module(lambdas,[
    is_lambda_ex/1,
    make_lambda/2,
    make_lambda/3,
    flush_lambda/0,
    flush_lambda_log/0,
    clear_lambda/1,
    lambda_map/2,
    lambda_map/3,
    lambda_map/4,
    lambda_map/5,
    '>>>'/3,
    '>>>'/4,
    '>>>'/5,
    '>>>'/6,
    '>>>'/7,
    '>>>'/8,
    '>>>'/9,
    add_lambda_arity/1
    ]).
:- dynamic(is_ref/3).

% Confirms that a expression is actually a lambda expression
% is_lambda_ex(+Expression)
is_lambda_ex(Expression):-
    is_lambda_ex(Expression,_,_).
is_lambda_ex(Expression,[Part],Rules):-
    Expression = (Part:- Rules),
    var(Part),
    is_rules(Rules).
is_lambda_ex(Expression,Parts,Rules):-
    Expression = (Parts :- Rules),
    is_list(Parts),
    is_rules(Rules).

% Confirms that Rule is actually a rule or a set of rules
% is_rule(+Rule)
is_rules(Rule):-
    is_rule(Rule).
is_rules((Rule:Rest)):-
    is_rule(Rule),
    is_rules(Rest);
    is_rule(Rest).
is_rule(Rule):-
    callable(Rule).

% Creates a lambda predicate
% make_lambda(+Expression,-Predicate)
make_lambda(Expression,lambdas:Predicate):-
    make_lambda(Expression,lambdas:Predicate,_).
% Creates a lambda predicate, and gives you the reference
% make_lambda(+Expression,-Predicate,-Reference)
make_lambda(Expression,lambdas:Predicate,Reference):-
    is_lambda_ex(Expression,Arguments,Rules),
    length(Arguments,Arity),
    new_name(Predicate,Arity),
    !,
    instantiate_rules(Rules,Arguments,Predicate,Arity,Reference).
% anchors a given set of rules in memory
instantiate_rules(Rules,Args,Pred,Arity,Reference):-
    Term =.. [Pred|Args],
    assertz((Term:-Rules),Reference),
    assertz(is_ref(Reference,Pred,Arity))
    .

% Removes all lambda predicates from memory, and tells you the details
flush_lambda_log :-
    findall(Ref,is_ref(Ref,_,_),References),
    maplist(clear_lambda,References),
    length(References,Len),
    write(Len), write(" lambda predicates cleared"),nl.
flush_lambda_log :-
    findall(Ref,is_ref(Ref,_,_),[]),
    write("No lambda predicates to clear"),nl.

% Removes all lambda predicates from memory
flush_lambda :-
    findall(Ref, is_ref(Ref,_,_), References),
    maplist(clear_lambda,References).
flush_lambda :-
    findall(Ref,is_ref(Ref,_,_),[]).

% Removes one lambda predicate from memory by their given reference
% clear_lambda(+Reference)
clear_lambda(Reference):-
    retract(is_ref(Reference,_,_)),
    erase(Reference).

% Finds a new name using a new predicate
% Needs more steps for how many lambda predicates are in memory
% So that would be one benifit for clearing it regularly
% new_name(-Name,+Arity)
new_name(Name,Arity):-
    new_name(Name,Arity,1).
new_name(Name,Arity,Seq):-
    string_concat("lambda",Seq,Name_STR),
    term_string(Name,Name_STR),
    not(exists_as(lambdas:Name,Arity)).
new_name(Name,Arity,Seq):-
    Next is Seq+1,
    new_name(Name,Arity,Next).
% Confirms that a predicate name already exists
% exists_as(+Name,+Arity)
exists_as(lambdas:Name,Arity):-
    is_ref(_,Name,Arity).

% lambda_map replaces maplist, just instead of a predicate name a 
% lambda expression has to be provided.
% This will also remove the predicate from the memory when possible,
% which can lead to better runtime results
% lambda_map(+Expression,L1,?L2,?L3,?L4,?L5)
lambda_map(Expression,L1):-
    !,
    make_lambda(Expression,Func,Ref),
    exists_as(Func,1), % To confirm that the arity is right
    maplist(Func,L1),
    clear_lambda(Ref).
lambda_map(_,_):-
    write("Map could not be applied."),nl,
    write("Likely the arity of the supplied function was not 1").
lambda_map(Expression,L1,L2):-
    !,
    make_lambda(Expression,Func,Ref),
    exists_as(Func,2), % To confirm that the arity is right
    maplist(Func,L1,L2),
    clear_lambda(Ref).
lambda_map(_,_,_):-
    write("Map could not be applied."),nl,
    write("Likely the arity of the supplied function was not 2").
lambda_map(Expression,L1,L2,L3):-
    !,
    make_lambda(Expression,Func,Ref),
    exists_as(Func,3), % To confirm that the arity is right
    maplist(Func,L1,L2,L3),
    clear_lambda(Ref).
lambda_map(_,_,_,_):-
    write("Map could not be applied."),nl,
    write("Likely the arity of the supplied function was not 3").
lambda_map(Expression,L1,L2,L3,L4):-
    !,
    make_lambda(Expression,Func,Ref),
    exists_as(Func,4), % To confirm that the arity is right
    maplist(Func,L1,L2,L3,L4),
    clear_lambda(Ref).
lambda_map(_,_,_,_,_):-
    write("Map could not be applied."),nl,
    write("Likely the arity of the supplied function was not 4").

% The following was an attempt to make a imediately callable
% lambda expression, as in the yall library.
% The upside is that it works with free variables without any 
% more action. No {}/ required
% But it is also computationally expencive, and should be avoided.

:- op(1200,xfy,>>>).
'>>>'(Args,Rules,A):-
    make_lambda(Args:-Rules,Lambda,Ref),
    call(Lambda,A),
    clear_lambda(Ref).
'>>>'(Args,Rules,A,B):-
    make_lambda(Args:-Rules,Lambda,Ref),
    call(Lambda,A,B),
    clear_lambda(Ref).
'>>>'(Args,Rules,A,B,C):-
    make_lambda(Args:-Rules,Lambda,Ref),
    call(Lambda,A,B,C),
    clear_lambda(Ref).
'>>>'(Args,Rules,A,B,C,D):-
    make_lambda(Args:-Rules,Lambda,Ref),
    call(Lambda,A,B,C,D),
    clear_lambda(Ref).
'>>>'(Args,Rules,A,B,C,D,E):-
    make_lambda(Args:-Rules,Lambda,Ref),
    call(Lambda,A,B,C,D,E),
    clear_lambda(Ref).
'>>>'(Args,Rules,A,B,C,D,E,F):-
    make_lambda(Args:-Rules,Lambda,Ref),
    call(Lambda,A,B,C,D,E,F),
    clear_lambda(Ref).
'>>>'(Args,Rules,A,B,C,D,E,F,G):-
    make_lambda(Args:-Rules,Lambda,Ref),
    call(Lambda,A,B,C,D,E,F,G),
    clear_lambda(Ref).
% Using the following predicate one can dynamically add >>> predicates of varying parameters.
% If you have some expression that for some reason needs to have 50 parameters, you could call:
% add_lambda_arity(50).
add_lambda_arity(Arity):-
    length(Args,Arity),
    Signature =.. ['>>>',Args,Rules|Args],
    assert(Signature:-(
    make_lambda(Args:-Rules,Lambda,Ref),
    apply(Lambda,Args),
    clear_lambda(Ref)    
    )).
