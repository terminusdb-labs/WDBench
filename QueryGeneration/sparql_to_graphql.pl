:- module(sparql_to_graphql, []).
:- use_module(library(pcre)).
:- use_module(library(plunit)).

compress_schema(Uri, Prefixes, Result) :-
    get_dict('@schema', Prefixes, Schema),
    atom_concat(Schema, Result, Uri).

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
    visit_edges(Root, Edges, Dict_1),
    visit_var_children(Dict_1, Edges, Ast),
    disconnected_edges(Edges, Disconnecteds),
    (   length(Disconnecteds, 0)
    ->  true
    ;   throw(error(disconnected_edges(Disconnecteds), _))).

ast_graphql(Ast, GraphQL) :-
    ast_node_query(Ast, GraphQL).

render_fields_(P, [Rendered]) :-
    atom(P),
    !,
    format(atom(Rendered), ' ~w, ', [P]).
render_fields_([], []).
render_fields_([P-Query|Fields], [Rendered|Rendered_Fields]) :-
    render_graphql_(Query, Result),
    format(atom(Rendered), ' ~w~w, ', [P, Result]),
    render_fields_(Fields, Rendered_Fields).

render_fields(Fields, Rendered) :-
    render_fields_(Fields, Rendered_Fields),
    atomic_list_concat(Rendered_Fields, Rendered).

render_filters_([], []).
render_filters_([P-Filter|Filters], [Rendered|Rendered_Filters]) :-
    (   is_list(Filter)
    ->  render_filters_(Filter, Result),
        format(atom(Rendered), ' ~w:{someHave:{~w}}, ', [P, Result])
    ;   format(atom(Rendered), ' ~w:{eq:~w}, ', [P, Filter])
    ),
    render_filters_(Filters, Rendered_Filters).

render_filters(Filters, Rendered_Filters) :-
    render_filters_(Filters, Rendered_Filters0),
    maplist([RF, NRF]>>format(atom(NRF), '{~w},', [RF]), Rendered_Filters0,
            Rendered_Filters).

render_graphql_(query(Filter, Fields), Rendered) :-
    render_filters(Filter, Rendered_Filters),
    render_fields(Fields, Rendered_Fields),
    (   Rendered_Filters = []
    ->  format(atom(Rendered), '{ ~w }',
               [Rendered_Fields])
    ;   atomic_list_concat(Rendered_Filters, Filter_Atom),
        format(atom(Rendered), '(filter:{_and : [ ~w ] }){ ~w }',
               [Filter_Atom,
                Rendered_Fields])
    ).

render_graphql(Query, Atom) :-
    render_graphql_(Query, GQL),
    format(atom(Atom), 'Node~w', [GQL]).

create_graphql_propname(Prop, Direction, P) :-
    compress_schema(Prop, _{'@schema' : 'http://www.wikidata.org/prop/direct/' }, Short),
    (   Direction = forward
    ->  Short = P
    ;   Direction = backward
    ->  format(atom(P), '_~w_of_Node', [Short])
    ).

ast_filter(Ast, Filter) :-
    findall(
        Pair,
        (   get_dict(Prop, Ast, Destination),
            destination(Destination, Direction, Child),
            create_graphql_propname(Prop, Direction, P),
            print_term(P, []),
            (   Child = node(Node)
            ->  Pair = P-[id-Node]
            ;   is_dict(Node)
            ->  ast_filter(Node, SubFilter),
                Pair = P-SubFilter
            )
        ),
        Filter
    ).

ast_node_query(Ast, query(Filter, SubQueries)) :-
    ast_filter(Ast, Filter),
    findall(
        Subquery,
        (    get_dict(Prop, Ast, Destination),
             destination(Destination, Direction, Child),
             create_graphql_propname(Prop, Direction, P),
             (   Child = leaf_var(_)
             ->  Subquery = P-query([], '_id')
             ;   Child = node(Node)
             ->  Subquery = P-query([id-Node], '_id')
             ;   is_dict(Child)
             ->  ast_node_query(Child, Child_Query),
                 Subquery = P-Child_Query
             )
        ),
        SubQueries
    ).


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

test(sparql_ast_disconnected, [
         error(disconnected_edges([edge(var(x3),'http://www.wikidata.org/prop/direct/P18',var(x4))]))
     ]) :-
    sparql_ast('?x1 <http://www.wikidata.org/prop/direct/P1010> ?x2 . ?x3 <http://www.wikidata.org/prop/direct/P18> ?x4 . ', _Ast).

test(sparql_ast) :-
    sparql_ast('?x1 <http://www.wikidata.org/prop/direct/P105> <http://www.wikidata.org/entity/Q7432> . ?x1 <http://www.wikidata.org/prop/direct/P225> ?x2 . ', Ast),
    Ast = _{ 'http://www.wikidata.org/prop/direct/P105':
             forward(node('http://www.wikidata.org/entity/Q7432')),
			 'http://www.wikidata.org/prop/direct/P225':
             forward(leaf_var(x2))
		   }.

test(ast_graphql) :-
    sparql_ast('?x1 <http://www.wikidata.org/prop/direct/P105> <http://www.wikidata.org/entity/Q7432> . ?x1 <http://www.wikidata.org/prop/direct/P225> ?x2 . ', Ast),
    sparql_to_graphql:ast_graphql(Ast, GraphQL),
    GraphQL = query([ 'P105' - [ id - 'http://www.wikidata.org/entity/Q7432'
							  ]
				    ],
				    [ 'P105' - query([ id - 'http://www.wikidata.org/entity/Q7432'
								     ],
								     '_id'),
					  'P225' - query([],'_id')
				    ]).

test(render_graphql) :-
    sparql_ast('?x1 <http://www.wikidata.org/prop/direct/P105> <http://www.wikidata.org/entity/Q7432> . ?x1 <http://www.wikidata.org/prop/direct/P225> ?x2 . ', Ast),
    sparql_to_graphql:ast_graphql(Ast, GraphQL),
    render_graphql(GraphQL, Rendered),
    print_term(Rendered, []),
    Rendered = 'Node(filter:{_and : [ { P105:{someHave:{[ id:{eq:http://www.wikidata.org/entity/Q7432}, ]}}, }, ] }){  P105(filter:{_and : [ { id:{eq:http://www.wikidata.org/entity/Q7432}, }, ] }){  _id,  },  P225{  _id,  },  }'.

:- end_tests(sparql_pattern_match).
