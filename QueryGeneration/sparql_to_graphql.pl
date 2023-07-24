:- module(sparql_to_graphql, []).
:- use_module(library(pcre)).
:- use_module(library(plunit)).

node_expression('<[^>]+>').
group_node_expression('<([^>]+)>').

var_expression('\\?[a-zA-z][a-zA-z0-9]*').
group_var_expression('\\?([a-zA-z][a-zA-z0-9]*)').

:- table path_expression/1.
path_expression(X) :-
    node_expression(NE),
    format(atom(X), '\\^?\\(([\\^\\(\\)\\|/\\*\\+\\?]|~s)+\\)', [NE]).

:- table sparql_expression/1.
sparql_expression(X) :-
    node_expression(NE),
    var_expression(VE),
    path_expression(PE),
    format(atom(X), '\\s*(?P<subject>~s|~s) (?P<predicate>~s|~s|~s) (?P<object>~s|~s)\\s*(\\.\\s*|$)', [NE, VE, NE, VE, PE, NE, VE]).

is_node(X) :-
    node_expression(NE),
    re_match(NE, X).

is_var(X) :-
    node_expression(VE),
    re_match(VE, X).

is_path(X) :-
    path_expression(PE),
    re_match(PE, X).

as_node(X, Node) :-
    group_node_expression(NE),
    re_matchsub(NE, X, Dict),
    get_dict(1, Dict, Node_String),
    atom_string(Node, Node_String).

as_var(X, Var) :-
    group_var_expression(VE),
    re_matchsub(VE, X, Dict),
    get_dict(1, Dict, Var_String),
    atom_string(Var, Var_String).

node_or_var(X, O),
as_node(X, N) =>
    O = node(N).
node_or_var(X, O),
as_var(X, V) =>
    O = var(V).

node_or_var_or_path(X, O),
as_node(X, N) =>
    O = node(N).
node_or_var_or_path(X, O),
as_var(X, V) =>
    O = var(V).
node_or_var_or_path(X, O),
is_path(X) =>
    O = path(X).


match_sparql(Query, Statements) :-
    sparql_expression(SE),
    re_foldl([Dict, Last, [sparql(Subject, Predicate, Object)|Last]]>>(
                 get_dict(subject, Dict, S),
                 get_dict(predicate, Dict, P),
                 get_dict(object, Dict, O),

                 node_or_var(S, Subject),
                 node_or_var_or_path(P, Predicate),
                 % todo this should probably match data too?
                 node_or_var(O, Object)
             ),
             SE,
             Query,
             [],
             Result,
             []),
    reverse(Result, Statements).

sparql_edges(Query, Edges) :-
    match_sparql(Query, Statements),
    maplist([sparql(S,P,O),[edge(S,P,O,Visited),edge(O,P,S,Visited)]]>>true,
            Statements,
            Edges_),
    append(Edges_, Edges).


visit_next_edge(Node, Edges, Dict_In, Dict_Out) :-
    member(edge(Node, node(P), O, Visited), Edges),
    var(Visited),
    !,
    put_dict(P, Dict_In, O, Dict_Out),
    Visited = visited.

visit_edges(Node, Edges, Dict) :-
    visit_edges_(Node, Edges, _{}, Dict).

visit_edges_(Node, Edges, Dict_In, Dict_Out) :-
    (   visit_next_edge(Node, Edges, Dict_In, Dict_Inter)
    ->  visit_edges_(Node, Edges, Dict_Inter, Dict_Out)
    ;   Dict_Out = Dict_In).

visit_next_var_child(Dict_In, Edges, Dict_Out) :-
    get_dict(Prop, Dict_In, var(Var)),
    visit_edges(var(Var), Edges, Child_Dict),
    put_dict(Prop, Dict_In, Child_Dict, Dict_Out).

:- begin_tests(sparql_pattern_match).
test(match_node) :-
    as_node('<foo>', foo).

test(match_var) :-
    as_var('?foo', foo).

test(match_path) :-
    is_path('((((((<http://www.wikidata.org/prop/direct/P31>/(<http://www.wikidata.org/prop/direct/P279>)?)/(<http://www.wikidata.org/prop/direct/P279>)?)/(<http://www.wikidata.org/prop/direct/P279>)?)/(<http://www.wikidata.org/prop/direct/P279>)?)/(<http://www.wikidata.org/prop/direct/P279>)?)/(<http://www.wikidata.org/prop/direct/P279>)?)').

test(match_sparql) :-
    match_sparql('<http://www.wikidata.org/entity/Q10990> ?x1 ?x2 . ?x3 ?x4 ?x5.',
                 [sparql(node('http://www.wikidata.org/entity/Q10990'), var(x1), var(x2)),
                  sparql(var(x3), var(x4), var(x5))]).

:- end_tests(sparql_pattern_match).
