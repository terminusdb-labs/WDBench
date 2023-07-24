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
    convlist([sparql(S,node(P),O),[edge(S,forward(P),O,Visited),edge(O,backward(P),S,Visited)]]>>true,
             Statements,
             Edges_),
    append(Edges_, Edges).


visit_next_edge(Node, Edges, Dict_In, Dict_Out) :-
    member(edge(Node, P, O, Visited), Edges),
    var(Visited),
    !,
    (   node_already_visited(O, Edges)
    ->  throw(error(revisited_node(O), _))
    ;   true),

    (   P = forward(P_)
    ->  put_dict(P_, Dict_In, forward(O), Dict_Out)
    ;   P = backward(P_)
    ->  put_dict(P_, Dict_In, backward(O), Dict_Out)),
    Visited = visited.

visit_edges(Node, Edges, Dict) :-
    visit_edges_(Node, Edges, _{}, Dict).

visit_edges_(Node, Edges, Dict_In, Dict_Out) :-
    (   visit_next_edge(Node, Edges, Dict_In, Dict_Inter)
    ->  visit_edges_(Node, Edges, Dict_Inter, Dict_Out)
    ;   Dict_Out = Dict_In).

destination(forward(X), forward, X).
destination(backward(X), backward, X).

var_child(Dict, Prop, Dest, Var) :-
    get_dict(Prop, Dict, X),
    destination(X, Dest, var(Var)).

unvisited_edge_away_from(Node, Edges) :-
    member(edge(Node, _, _, Visited), Edges),
    var(Visited).

visit_next_var_child(Dict_In, Edges, Dict_Out) :-
    var_child(Dict_In, Prop, Dest, Var),
    !,
    (   unvisited_edge_away_from(var(Var), Edges)
    ->  visit_edges(var(Var), Edges, Child_Dict),
        visit_var_children(Child_Dict, Edges, Child_Dict_Resolved),
        destination(Child, Dest, Child_Dict_Resolved)
    ;   destination(Child, Dest, leaf_var(Var))),
    put_dict(Prop, Dict_In, Child, Dict_Out).

visit_var_children(Dict_In, Edges, Dict_Out) :-
    (   visit_next_var_child(Dict_In, Edges, Dict_Inter)
    ->  visit_var_children(Dict_Inter, Edges, Dict_Out)
    ;   Dict_Out = Dict_In).

node_already_visited(Node, Edges) :-
    memberchk(edge(_, _, Node,Visited), Edges),
    ground(Visited).

disconnected_edges(Edges, Disconnecteds) :-
    findall(Disconnected,
            (
                member(edge(S, forward(P), O, Visited), Edges),
                var(Visited),
                Disconnected = edge(S, P, O)
            ),
            Disconnecteds).

sparql_ast(Query, Ast) :-
    sparql_edges(Query, Edges),
    Edges = [edge(Root, _, _, _)|_],
    print_term(Root, []),
    visit_edges(Root, Edges, Dict_1),
    visit_var_children(Dict_1, Edges, Ast),
    disconnected_edges(Edges, Disconnecteds),
    (   length(Disconnecteds, 0)
    ->  true
    ;   throw(error(disconnected_edges(Disconnecteds), _))).

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
