:- module(sparql_to_graphql, [sparql_csv_to_woql_csv/2]).
:- use_module(library(pcre)).
:- use_module(library(plunit)).
:- use_module(library(csv)).
:- use_module(library(http/json)).

compress_schema(Uri, Prefixes, Result) :-
    get_dict('@schema', Prefixes, Schema),
    atom_concat(Schema, Result, Uri).

compress_wiki(Uri, Prefixes, Result) :-
    get_dict('wiki', Prefixes, Schema),
    atom_concat(Schema, Fragment, Uri),
    atom_concat('wiki:', Fragment, Result).

node_expression('<[^>]+>').
group_node_expression('<([^>]+)>').

var_expression('\\?[a-zA-z][a-zA-z0-9]*').
group_var_expression('\\?([a-zA-z][a-zA-z0-9]*)').

:- table path_expression/1.
path_expression(X) :-
    node_expression(NE),
    format(atom(X), '\\^?\\(([\\^\\(\\)\\|/\\*\\+\\?]|~s)+\\)[\\*,\\+]?|\\^~s|~s[\\*,\\+]', [NE,NE,NE]).

:- table sparql_expression/1.
sparql_expression(X) :-
    node_expression(NE),
    var_expression(VE),
    path_expression(PE),
    format(atom(X), '\\s*(?P<optional>OPTIONAL\s*{)?\\s*(?P<subject>~s|~s) (?P<predicate>~s|~s|~s) (?P<object>~s|~s)\\s*}?(\\.\\s*|$)', [NE, VE, NE, VE, PE, NE, VE]).

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
is_path(X) =>
    O = path(X).
node_or_var_or_path(X, O),
as_node(X, N) =>
    O = node(N).
node_or_var_or_path(X, O),
as_var(X, V) =>
    O = var(V).

group(group(Path)) --> "(", path(Path), ")" .

anyBut(S,A,[H|T],T) :- atom_codes(S,C), \+ member(H,C), atom_codes(A,[H]) .

url(S) --> anyBut('>', C), url(Rest), !, { atom_concat(C, Rest, S) } .
url(S) --> anyBut('>', S) .

caret(neg(Path)) --> "^", path(Path) .

pred(p(Path)) --> "<", url(Path), ">" .

star(star(Path)) --> simple_base_path(Path), "*" .

plus(plus(Path)) --> simple_base_path(Path), "+" .

question(times(Path, 0, 1)) --> simple_base_path(Path), "?" .

simple_base_path(Path) --> pred(Path), ! .
simple_base_path(Path) --> caret(Path), ! .
simple_base_path(Path) --> group(Path) .

path_type(Path, plus(Path)) --> "+", ! .
path_type(Path, star(Path)) --> "*", ! .
path_type(Path, times(Path, 0, 1)) --> "?", ! .
path_type(Path, Path) --> {true} .

base_path(PathType) --> simple_base_path(Path), !, path_type(Path, PathType) .

or((PathA | PathB)) --> base_path(PathA), "|", !, path(PathB) .
or(Path) --> base_path(Path) .

and((PathA,PathB)) --> or(PathA), "/", !, and(PathB) .
and(Path) --> or(Path) .

path(Path) -->
    and(Path) .

negate(p(Pred), n(Pred)).
negate(n(Pred), p(Pred)).
negate(neg(Path), Path).
negate(plus(Path), plus(Neg)) :-
    negate(Path, Neg).
negate(star(Path), star(Neg)) :-
    negate(Path, Neg).
negate(times(Path, M, N), times(Neg, N, M)) :-
    negate(Path, Neg).
negate((PathA,PathB), (NegB, NegA)) :-
    negate(PathA, NegA),
    negate(PathB, NegB).
negate((PathA | PathB), (NegA | NegB)) :-
    negate(PathA, NegA),
    negate(PathB, NegB).
negate(group(Path), Neg) :- negate(Path, Neg).

negation_normal_form(p(Pred), p(Pred)).
negation_normal_form(n(Pred), n(Pred)).
negation_normal_form(plus(Path), plus(Normal)) :-
    negation_normal_form(Path, Normal).
negation_normal_form(star(Path), star(Normal)) :-
    negation_normal_form(Path, Normal).
negation_normal_form(times(Path, M, N), times(Normal, M, N)) :-
    negation_normal_form(Path, Normal).
negation_normal_form(neg(Path), Normal) :-
    negate(Path, Neg),
    negation_normal_form(Neg, Normal).
negation_normal_form(group(Path), Normal) :-
    negation_normal_form(Path, Normal).
negation_normal_form((PathA,PathB), (NormalA, NormalB)) :-
    negation_normal_form(PathA, NormalA),
    negation_normal_form(PathB, NormalB).
negation_normal_form((PathA|PathB), (NormalA | NormalB)) :-
    negation_normal_form(PathA, NormalA),
    negation_normal_form(PathB, NormalB).

parse_path(Path_String, Normal) :-
    atom_codes(Path_String, Codes),
    phrase(path(Path), Codes, []),
    !,
    negation_normal_form(Path, Normal).

path_to_woql(n(Pred), WOQL) :-
    WOQL = _{ '@type' : "InversePathPredicate", predicate: Pred }.
path_to_woql(p(Pred), WOQL) :-
    WOQL = _{ '@type' : "PathPredicate", predicate: Pred }.
path_to_woql(star(Path), WOQL) :-
    path_to_woql(Path, Inner),
    WOQL = _{ '@type' : "PathStar", star : Inner }.
path_to_woql(plus(Path), WOQL) :-
    path_to_woql(Path, Inner),
    WOQL = _{ '@type' : "PathPlus", plus : Inner }.
path_to_woql(times(Path, M, N), WOQL) :-
    path_to_woql(Path, Inner),
    WOQL = _{ '@type' : "PathTimes", times : Inner, from: M, to: N }.
path_to_woql(group(Path), Inner) :-
    path_to_woql(Path, Inner).
path_to_woql((PathA,PathB), WOQL) :-
    path_to_woql(PathA, InnerA),
    path_to_woql(PathB, InnerB),
    WOQL = _{ '@type' : "PathSequence",
              sequence : [InnerA, InnerB] }.
path_to_woql((PathA|PathB), WOQL) :-
    path_to_woql(PathA, InnerA),
    path_to_woql(PathB, InnerB),
    WOQL = _{ '@type' : "PathOr",
              or : [InnerA, InnerB] }.

subject_woql(node(N), WOQL) :-
    WOQL = _{ '@type' : "NodeValue", node: N }.
subject_woql(var(V), WOQL) :-
    WOQL = _{ '@type' : "NodeValue", variable: V }.

predicate_woql(node(N), WOQL) :-
    WOQL = _{ '@type' : "NodeValue", node: N }.
predicate_woql(var(V), WOQL) :-
    WOQL = _{ '@type' : "NodeValue", variable: V }.

object_woql(node(N), WOQL) :-
    WOQL = _{ '@type' : "Value", node: N }.
object_woql(var(V), WOQL) :-
    WOQL = _{ '@type' : "Value", variable: V }.

ast_to_woql(Ast, WOQL) :-
    findall(
        WOQL_Statement,
        (   member(Term, Ast),
            Term =.. [Type, Subject, Predicate, Object],
            subject_woql(Subject, WOQL_Subject),
            object_woql(Object, WOQL_Object),
            (   Predicate = path(PathExp)
            ->  parse_path(PathExp, Path),
                path_to_woql(Path, WOQL_Path),
                WOQL_Statement = _{ '@type' : "Path",
                                    subject: WOQL_Subject,
                                    pattern: WOQL_Path,
                                    object: WOQL_Object }
            ;   predicate_woql(Predicate, WOQL_Predicate),
                Pre_Statement = _{ '@type' : "Triple",
                                   subject: WOQL_Subject,
                                   predicate: WOQL_Predicate,
                                   object: WOQL_Object },
                (   Type = sparql
                ->  WOQL_Statement = Pre_Statement
                ;   Type = optional
                ->  WOQL_Statement = _{ '@type' : "Optional",
                                        query: Pre_Statement }
                )
            )
        ),
        WOQL_Statements
    ),
    (   WOQL_Statements = []
    ->  fail
    ;   WOQL = _{ '@type' : "And",
                  and : WOQL_Statements }
    ).

match_sparql(Query, Statements) :-
    sparql_expression(SE),
    re_foldl([Dict, Last, [Term|Last]]>>(
                 get_dict(subject, Dict, S),
                 get_dict(predicate, Dict, P),
                 get_dict(object, Dict, O),

                 node_or_var(S, Subject),
                 node_or_var_or_path(P, Predicate),
                 % todo this should probably match data too?
                 node_or_var(O, Object),
                 (   get_dict(optional, Dict, "")
                 ->  Term = sparql(Subject, Predicate, Object)
                 ;   Term = optional(Subject, Predicate, Object)
                 )
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
    put_dict(_{'_id': Root}, Dict_1, Dict_2),
    visit_var_children(Dict_2, Edges, Ast),
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
        atomic_list_concat(Result, Result_Atom),
        format(atom(Rendered), ' ~w:{someHave:{~w}}, ', [P, Result_Atom])
    ;   format(atom(Rendered), ' ~w:"~w", ', [P, Filter])
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
    ->  format(atom(P), "wiki_~w", [Short])
    ;   Direction = backward
    ->  format(atom(P), '_wiki_~w_of_Node', [Short])
    ).

ast_filter(Ast, Filter) :-
    findall(
        Pair,
        (   get_dict(Prop, Ast, Destination),
            (   Prop = '_id'
            ->  Destination = node(Node),
                Pair = '_id'-Node
            ;   destination(Destination, Direction, Child),
                create_graphql_propname(Prop, Direction, P),
                (   Child = node(Node)
                ->  Pair = P-['_id'-Node]
                ;   is_dict(Node)
                ->  ast_filter(Node, SubFilter),
                    Pair = P-SubFilter
                )
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
             ->  Subquery = P-query(['_id'-Node], '_id')
             ;   is_dict(Child)
             ->  ast_node_query(Child, Child_Query),
                 Subquery = P-Child_Query
             )
        ),
        SubQueries
    ).

sparql_woql(Sparql, WOQL) :-
    match_sparql(Sparql, Edges),
    ast_to_woql(Edges, WOQL).

sparql_graphql(Sparql, Rendered) :-
    catch(
        (   sparql_ast(Sparql, Ast),
            ast_graphql(Ast, GraphQL),
            render_graphql(GraphQL, Rendered)
        ),
        E,
        (   E = error(disconnected_edges(_), _)
        ->  fail
        ;   E = error(revisited_node(_), _)
        ->  fail
        ;   throw(E)
        )
    ).

sparql_csv_to_woql_csv(CSV_In, CSV_Out) :-
    setup_call_cleanup(
        open(CSV_Out, write, Out),
        forall(
            csv_read_file_row(CSV_In, row(Num,Sparql_Query), []),
            (   sparql_woql(Sparql_Query, WOQL_Query)
            ->  atom_json_dict(WOQL_Query_Atom, WOQL_Query, [width(0)]),
                csv_write_stream(Out, [row(Num,WOQL_Query_Atom)], [])
            ;   true
            )
        ),
        close(Out)
    ).

sparql_csv_to_graphql_csv(CSV_In, CSV_Out) :-
    setup_call_cleanup(
        open(CSV_Out, write, Out),
        forall(
            csv_read_file_row(CSV_In, row(Num,Sparql_Query), []),
            (   sparql_graphql(Sparql_Query, Query)
            ->  csv_write_stream(Out, [row(Num,Query)], [])
            ;   true
            )
        ),
        close(Out)
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
    Ast = _{ '_id': var('x1'),
             'http://www.wikidata.org/prop/direct/P105':
             forward(node('http://www.wikidata.org/entity/Q7432')),
			 'http://www.wikidata.org/prop/direct/P225':
             forward(leaf_var(x2))
		   }.

test(ast_graphql) :-
    sparql_ast('?x1 <http://www.wikidata.org/prop/direct/P105> <http://www.wikidata.org/entity/Q7432> . ?x1 <http://www.wikidata.org/prop/direct/P225> ?x2 . ', Ast),
    sparql_to_graphql:ast_graphql(Ast, GraphQL),

    GraphQL = query([ 'wiki_P105' - [ '_id' - 'http://www.wikidata.org/entity/Q7432'
							  ]
				    ],
				    [ 'wiki_P105' - query([ '_id' - 'http://www.wikidata.org/entity/Q7432'
								     ],
								     '_id'),
					  'wiki_P225' - query([],'_id')
				    ]).

test(render_graphql) :-
    sparql_ast('?x1 <http://www.wikidata.org/prop/direct/P105> <http://www.wikidata.org/entity/Q7432> . ?x1 <http://www.wikidata.org/prop/direct/P225> ?x2 . ', Ast),
    sparql_to_graphql:ast_graphql(Ast, GraphQL),
    render_graphql(GraphQL, Rendered),

    Rendered = 'Node(filter:{_and : [ { wiki_P105:{someHave:{ _id:"http://www.wikidata.org/entity/Q7432", }}, }, ] }){  wiki_P105(filter:{_and : [ { _id:"http://www.wikidata.org/entity/Q7432", }, ] }){  _id,  },  wiki_P225{  _id,  },  }'.

test(parse_complex_path) :-
    parse_path("((<http://www.wikidata.org/prop/direct/P279>/((<http://www.wikidata.org/prop/direct/P279>)*|(<http://www.wikidata.org/prop/direct/P31>)*))|(<http://www.wikidata.org/prop/direct/P31>/((<http://www.wikidata.org/prop/direct/P279>)*|(<http://www.wikidata.org/prop/direct/P31>)*)))", Path),
    Path = (p('http://www.wikidata.org/prop/direct/P279'),
            (star(p('http://www.wikidata.org/prop/direct/P279'))|
             star(p('http://www.wikidata.org/prop/direct/P31')))|
            p('http://www.wikidata.org/prop/direct/P31'),
            (star(p('http://www.wikidata.org/prop/direct/P279'))| star(p('http://www.wikidata.org/prop/direct/P31')))),
    path_to_woql(Path, WOQL),

    WOQL = _{ '@type':"PathOr",
	          or:[ _{ '@type':"PathSequence",
					  sequence:[ _{ '@type':"PathPredicate",
								    predicate:'http://www.wikidata.org/prop/direct/P279'
								  },
								 _{ '@type':"PathOr",
								    or:[ _{ '@type':"PathStar",
										    star:_{ '@type':"PathPredicate",
											        predicate:'http://www.wikidata.org/prop/direct/P279'
											      }
										  },
										 _{ '@type':"PathStar",
										    star:_{ '@type':"PathPredicate",
											        predicate:'http://www.wikidata.org/prop/direct/P31'
											      }
										  }
									   ]
								  }
							   ]
				    },
				   _{ '@type':"PathSequence",
					  sequence:[ _{ '@type':"PathPredicate",
								    predicate:'http://www.wikidata.org/prop/direct/P31'
								  },
								 _{ '@type':"PathOr",
								    or:[ _{ '@type':"PathStar",
										    star:_{ '@type':"PathPredicate",
											        predicate:'http://www.wikidata.org/prop/direct/P279'
											      }
										  },
										 _{ '@type':"PathStar",
										    star:_{ '@type':"PathPredicate",
											        predicate:'http://www.wikidata.org/prop/direct/P31'
											      }
										  }
									   ]
								  }
							   ]
				    }
				 ]
	        }.

test(multiple_bgp17, []) :-
    Query = '?x1 <http://www.wikidata.org/prop/direct/P105> ?x2 . ?x3 <http://www.wikidata.org/prop/direct/P105> ?x4 . ?x1 <http://www.wikidata.org/prop/direct/P171> ?x3 . ?x1 <http://www.wikidata.org/prop/direct/P31> <http://www.wikidata.org/entity/Q16521> . ?x3 <http://www.wikidata.org/prop/direct/P31> <http://www.wikidata.org/entity/Q16521> . ?x4 <http://www.wikidata.org/prop/direct/P361> ?x2 . ',
    match_sparql(Query, Edges),
    ast_to_woql(Edges, WOQL),
    WOQL = _{ '@type':"And",
			  and:[ _{ '@type':"Triple",
					   object:_{ '@type':"Value",
								 variable:x2
							   },
					   predicate:_{ '@type':"NodeValue",
								    node:'http://www.wikidata.org/prop/direct/P105'
								  },
					   subject:_{ '@type':"NodeValue",
								  variable:x1
							    }
					 },
					_{ '@type':"Triple",
					   object:_{ '@type':"Value",
								 variable:x4
							   },
					   predicate:_{ '@type':"NodeValue",
								    node:'http://www.wikidata.org/prop/direct/P105'
								  },
					   subject:_{ '@type':"NodeValue",
								  variable:x3
							    }
					 },
					_{ '@type':"Triple",
					   object:_{ '@type':"Value",
								 variable:x3
							   },
					   predicate:_{ '@type':"NodeValue",
								    node:'http://www.wikidata.org/prop/direct/P171'
								  },
					   subject:_{ '@type':"NodeValue",
								  variable:x1
							    }
					 },
					_{ '@type':"Triple",
					   object:_{ '@type':"Value",
								 node:'http://www.wikidata.org/entity/Q16521'
							   },
					   predicate:_{ '@type':"NodeValue",
								    node:'http://www.wikidata.org/prop/direct/P31'
								  },
					   subject:_{ '@type':"NodeValue",
								  variable:x1
							    }
					 },
					_{ '@type':"Triple",
					   object:_{ '@type':"Value",
								 node:'http://www.wikidata.org/entity/Q16521'
							   },
					   predicate:_{ '@type':"NodeValue",
								    node:'http://www.wikidata.org/prop/direct/P31'
								  },
					   subject:_{ '@type':"NodeValue",
								  variable:x3
							    }
					 },
					_{ '@type':"Triple",
					   object:_{ '@type':"Value",
								 variable:x2
							   },
					   predicate:_{ '@type':"NodeValue",
								    node:'http://www.wikidata.org/prop/direct/P361'
								  },
					   subject:_{ '@type':"NodeValue",
								  variable:x4
							    }
					 }
				  ]
			}.

test(optional3) :-
    match_sparql('?x1 <http://www.wikidata.org/prop/direct/P102> <http://www.wikidata.org/entity/Q6721203> . OPTIONAL { ?x1 <http://www.wikidata.org/prop/direct/P569> ?x2 . } OPTIONAL { ?x1 <http://www.wikidata.org/prop/direct/P19> ?x3 . } OPTIONAL { ?x1 <http://www.wikidata.org/prop/direct/P21> ?x4 . } OPTIONAL { ?x3 <http://www.wikidata.org/prop/direct/P625> ?x5 . } ', Results),
    Results = [ sparql(var(x1),
					   node('http://www.wikidata.org/prop/direct/P102'),
					   node('http://www.wikidata.org/entity/Q6721203')),
				optional(var(x1),
						 node('http://www.wikidata.org/prop/direct/P569'),
						 var(x2)),
				optional(var(x1),
						 node('http://www.wikidata.org/prop/direct/P19'),
						 var(x3)),
				optional(var(x1),
						 node('http://www.wikidata.org/prop/direct/P21'),
						 var(x4)),
				optional(var(x3),
						 node('http://www.wikidata.org/prop/direct/P625'),
						 var(x5))
			  ].

test(optional3_woql) :-
    match_sparql('?x1 <http://www.wikidata.org/prop/direct/P102> <http://www.wikidata.org/entity/Q6721203> . OPTIONAL { ?x1 <http://www.wikidata.org/prop/direct/P569> ?x2 . } OPTIONAL { ?x1 <http://www.wikidata.org/prop/direct/P19> ?x3 . } OPTIONAL { ?x1 <http://www.wikidata.org/prop/direct/P21> ?x4 . } OPTIONAL { ?x3 <http://www.wikidata.org/prop/direct/P625> ?x5 . } ', Edges),
    ast_to_woql(Edges, WOQL),
    WOQL = _{ '@type':"And",
			  and:[ _{ '@type':"Triple",
					   object:_{ '@type':"Value",
								 node:'http://www.wikidata.org/entity/Q6721203'
							   },
					   predicate:_{ '@type':"NodeValue",
								    node:'http://www.wikidata.org/prop/direct/P102'
								  },
					   subject:_{ '@type':"NodeValue",
								  variable:x1
								}
					 },
					_{ '@type':"Optional",
					   query:_{ '@type':"Triple",
								object:_{ '@type':"Value",
									      variable:x2
									    },
								predicate:_{ '@type':"NodeValue",
									         node:'http://www.wikidata.org/prop/direct/P569'
									       },
								subject:_{ '@type':"NodeValue",
									       variable:x1
									     }
							  }
					 },
					_{ '@type':"Optional",
					   query:_{ '@type':"Triple",
								object:_{ '@type':"Value",
									      variable:x3
									    },
								predicate:_{ '@type':"NodeValue",
									         node:'http://www.wikidata.org/prop/direct/P19'
									       },
								subject:_{ '@type':"NodeValue",
									       variable:x1
									     }
							  }
					 },
					_{ '@type':"Optional",
					   query:_{ '@type':"Triple",
								object:_{ '@type':"Value",
									      variable:x4
									    },
								predicate:_{ '@type':"NodeValue",
									         node:'http://www.wikidata.org/prop/direct/P21'
									       },
								subject:_{ '@type':"NodeValue",
									       variable:x1
									     }
							  }
					 },
					_{ '@type':"Optional",
					   query:_{ '@type':"Triple",
								object:_{ '@type':"Value",
									      variable:x5
									    },
								predicate:_{ '@type':"NodeValue",
									         node:'http://www.wikidata.org/prop/direct/P625'
									       },
								subject:_{ '@type':"NodeValue",
									       variable:x3
									     }
							  }
					 }
				  ]
			}.

test(negate_simple_path) :-
    P = ((star(n('http://www.wikidata.org/prop/direct/P279'))| star(n('http://www.wikidata.org/prop/direct/P31'))), n('http://www.wikidata.org/prop/direct/P279')),
    Neg = (p('http://www.wikidata.org/prop/direct/P279'), (star(p('http://www.wikidata.org/prop/direct/P279'))| star(p('http://www.wikidata.org/prop/direct/P31')))),
    negate(Neg, P),
    negate(P, Neg).

test(simple_negation_normal_path) :-
    P = neg(((star(n('http://www.wikidata.org/prop/direct/P279'))
              | star(n('http://www.wikidata.org/prop/direct/P31'))),
             n('http://www.wikidata.org/prop/direct/P279'))),
    negation_normal_form(P, Norm),
    Norm = (p('http://www.wikidata.org/prop/direct/P279'),
            (star(p('http://www.wikidata.org/prop/direct/P279'))
             | star(p('http://www.wikidata.org/prop/direct/P31')))).

test(triple_negation_normal_path) :-
    P = neg(neg(neg(p('http://www.wikidata.org/prop/direct/P279')))),
    negation_normal_form(P, Norm),
    Norm = n('http://www.wikidata.org/prop/direct/P279').


test(single_bgp_3_graphql) :-
    Query = '<http://www.wikidata.org/entity/Q1236511> <http://www.wikidata.org/prop/direct/P31> ?x1 . ',
    sparql_ast(Query, Ast),
    Ast = _{ '_id':node('http://www.wikidata.org/entity/Q1236511'),
	         'http://www.wikidata.org/prop/direct/P31':forward(leaf_var(x1))
	       },
    sparql_to_graphql:ast_graphql(Ast, GraphQL),
    render_graphql(GraphQL, Rendered),
    Rendered = 'Node(filter:{_and : [ { _id:"http://www.wikidata.org/entity/Q1236511", }, ] }){  wiki_P31{  _id,  },  }'.


:- end_tests(sparql_pattern_match).
