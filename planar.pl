show_dot(DotString):-
        open('/tmp/swipl_g.dot',write,Out),
        write(Out,DotString),
        close(Out),
        shell('dot -Tpng /tmp/swipl_g.dot -o /tmp/swipl_g.png'),
        shell('feh /tmp/swipl_g.png').

nonmember(_,[]).
nonmember(X,[H|T]):-
        X \= H,
        nonmember(X,T).

two_directions_edges([edge(A,B)|T],[edge(A,B),edge(B,A)|T2]):-
        two_directions_edges(T,T2).
two_directions_edges([],[]).

compare_edges(edge(From1,_),edge(From2,_)):-
        atom_greater(From2,From1).
compare_edges(edge(From,To1),edge(From,To2)):-
        atom_greater(To2,To1).

edges_to_dict(Edges,GraphDict):-
        two_directions_edges(Edges,Edges2),
        merge_sort(Edges2,compare_edges,SortedEdges),
        GraphIn = graph_dict{},
        SortedEdges = [edge(From,To)|Tail],
        insert_edges(Tail,From,[To],GraphIn,GraphDict).

intlist_greater([H1|_],[H2|_]):-
        H1 > H2,!.
intlist_greater([H|T1],[H|T2]):-
        intlist_greater(T1,T2).
intlist_greater([_],[]).

atom_greater(A,B):-
        atom_string(A,AString),
        atom_string(B,BString),
        string_codes(AString,AList),
        string_codes(BString,BList),
        intlist_greater(AList,BList).

merge_sort(List,Comparator,SortedList):-
        list_to_lists(List,ListOfLists),
        merge_sort_rec(ListOfLists,Comparator,[SortedList]).
list_to_lists([],[]).
list_to_lists([H|T],[[H]|T2]):-
        list_to_lists(T,T2).
merge_sort_rec([],_,[]).
merge_sort_rec([X],_,[X]):-!.
merge_sort_rec([A,B|T],Comparator,R):-
        merge_lists(A,B,Comparator,H2),
        merge_sort_rec(T,Comparator,T2),
        merge_sort_rec([H2|T2],Comparator,R).
merge_lists([],A,_,A):-!.
merge_lists(A,[],_,A):-!.
merge_lists(L1,L2,Comparator,[H1|T3]):-
        L1 = [H1|T1],
        L2 = [H2|_],
        call(Comparator,H1,H2), !,
        merge_lists(T1,L2,Comparator,T3).
merge_lists(L1,L2,Comparator,[H2|T3]):-
        L2 = [H2|T2],
        merge_lists(L1,T2,Comparator,T3).

%% insert_edges: while creating a new graph, should be called on an empty
%%   graph.  Inserts edges from a sorted list into the graph dictionary.
%%   Checks for doubles.  Insert each vertex as an adjacency list.
%%   Each edge should be represented by edge(A,B) and edge(B,A0), which is
%%   accomplished by two_directions_edges(EdgesIn,EdgesOut).
insert_edges([edge(From,To)|Tail],CFrom,CList,GraphIn,GraphOut):-
        From \= CFrom,!,
        term_to_atom(adj-CFrom,Key),
        Graph1 = GraphIn.put(Key,CList),
        insert_edges(Tail,From,[To],Graph1,GraphOut).
insert_edges([edge(From,To)|Tail],From,CList,GraphIn,GraphOut):-
        CList = [CTo|_],
        To \= CTo,!,
        insert_edges(Tail,From,[To|CList],GraphIn,GraphOut).
insert_edges([edge(From,To)|Tail],From,CList,GraphIn,GraphOut):-
        CList = [To|_],!,
        insert_edge(Tail,From,CList,GraphIn,GraphOut).
insert_edges([],CFrom,CList,GraphIn,GraphOut):-
        term_to_atom(adj-CFrom,Key),
        GraphOut = GraphIn.put(Key,CList).

test:-
        Edges = [edge(a,d),edge(a,b),edge(b,c),edge(b,d), edge(c,d)],
        edges_to_dict(Edges,GraphDict),
        write(GraphDict),nl,
        dfs_fronds(a,GraphDict,GraphDict2),
        write(GraphDict2),nl.
%%        simple_dot(First,DotString),
%%        show_dot(DotString).

%% DFS that assigns properties to edges: tree-edge or frond
dfs_fronds(Vertex,GraphIn,GraphOut):-
        dfs_fronds_rec(Vertex,[],_,GraphIn,GraphOut).
dfs_fronds_rec(Vertex,AccIn,AccOut,GraphIn,GraphOut):-
        term_to_atom(adj-Vertex,Key),
        Adj = GraphIn.get(Key),
        dfs_fronds_list(Vertex,Adj,[Vertex|AccIn],AccOut,GraphIn,GraphOut).
dfs_fronds_list(Vertex,[H|T],AccIn,AccOut,GraphIn,GraphOut):-
        member(H,AccIn),!,
        term_to_atom(edge(H,Vertex),BackKey),
        (   BackEdge = GraphIn.get(BackKey)
        ->  EdgeType = BackEdge
        ;   EdgeType = frond),
        term_to_atom(edge(Vertex,H),Key),
        Graph1 = GraphIn.put(Key,EdgeType),
        dfs_fronds_list(Vertex,T,AccIn,AccOut,Graph1,GraphOut).
dfs_fronds_list(Vertex,[H|T],AccIn,AccOut,GraphIn,GraphOut):-
        nonmember(H,AccIn),!,
        term_to_atom(edge(Vertex,H),Key),
        Graph1 = GraphIn.put(Key,tree-edge),
        dfs_fronds_rec(H,AccIn,Acc1,Graph1,Graph2),
        dfs_fronds_list(Vertex,T,Acc1,AccOut,Graph2,GraphOut).
dfs_fronds_list(_,[],Acc,Acc,Graph,Graph).
