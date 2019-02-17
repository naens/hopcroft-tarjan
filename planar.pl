show_dot(DotString):-
        open('/tmp/swipl_g.dot',write,Out),
        write(Out,DotString),
        close(Out),
        shell('dot -Tpng /tmp/swipl_g.dot -o /tmp/swipl_g.png'),
        shell('feh /tmp/swipl_g.png').

test:-
        Edges = [edge(a,d),edge(a,b),edge(b,c),edge(b,d), edge(c,d)],
        edges_to_dict(Edges,GraphDict),
        write(GraphDict).
%%        simple_dot(First,DotString),
%%        show_dot(DotString).

two_directions_edges([edge(A,B)|T],[edge(A,B),edge(B,A)|T2]):-
        two_directions_edges(T,T2).
two_directions_edges([],[]).

compareEdges(edge(From1,_),edge(From2,_)):-
        atom_greater(From2,From1).
compareEdges(edge(From,To1),edge(From,To2)):-
        atom_greater(To2,To1).

edges_to_dict(Edges,Dict):-
        two_directions_edges(Edges,Edges2),
        merge_sort(Edges2,compareEdges,SortedEdges),
        Dict = SortedEdges.

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
