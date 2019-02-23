show_dot(Graph):-
        VPropList = Graph.get(vprop),
        EPropList = Graph.get(eprop),
        show_dot(Graph,VPropList,EPropList).
show_dot(Graph,List):-
        properties_from_list(Graph,List,VPropList,EPropList),
        show_dot(Graph,VPropList,EPropList).
show_dot(Graph,VPropList,EPropList):-
        open('/tmp/swipl_g.dot',write,Out),
        dfs_dot(Graph,VPropList,EPropList,Out),
        close(Out),
        shell('dot -Tsvg /tmp/swipl_g.dot -o /tmp/swipl_g.svg'),
        shell('eog /tmp/swipl_g.svg').

todo(_).
todo(_,_).
todo(_,_,_).
todo(_,_,_,_).
todo(_,_,_,_,_).

memberget(X,[X|_],X).
memberget(X,[H|T],Y):-
        X \= H,!,
        memberget(X,T,Y).

properties_from_list(Graph,List,VPropList,EPropList):-
        AllVProp = Graph.get(vprop),
        AllEProp = Graph.get(eprop),
        pfl_rec(List,AllVProp,AllEProp,VPropList,EPropList).
pfl_rec([H|T],Vs,Es,[V|V1],EOut):-
        memberget([H,_],Vs,V),!,
        pfl_rec(T,Vs,Es,V1,EOut).
pfl_rec([H|T],Vs,Es,VOut,[E|E1]):-
        memberget([H,_],Es,E),!,
        pfl_rec(T,Vs,Es,VOut,E1).
pfl_rec([],_,_,[],[]).


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
        insert_edges(Tail,From,[To],GraphIn,Graph1),
        Graph2 = Graph1.put(vprop,[]),
        Graph3 = Graph2.put(eprop,[]),
        GraphDict = Graph3.put(root,From).

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

%% accessors
getAdjKey(Vertex,Key):-
        term_to_atom(adj-Vertex,Key).
getAdj(Graph,Vertex,Adj):-
        getAdjKey(Vertex,Key),
        Adj = Graph.get(Key).
setAdj(GraphIn,GraphOut,Vertex,Adj):-
        getAdjKey(Vertex,Key),
        GraphOut = GraphIn.put(Key,Adj).

getEdgeTypeKey(From,To,Key):-
        term_to_atom(edge(From,To),Key).
getEdgeType(Graph,From,To,Type):-
        getEdgeTypeKey(From,To,Key),
        Type = Graph.get(Key).
setEdgeType(GraphIn,GraphOut,From,To,Type):-
        getEdgeTypeKey(From,To,Key),
        GraphOut = GraphIn.put(Key,Type).

getDFNumKey(Vertex,Key):-
        term_to_atom(dfnum-Vertex,Key).
getDFNum(Graph,Vertex,DFNum):-
        getDFNumKey(Vertex,Key),
        DFNum = Graph.get(Key).
setDFNum(GraphIn,GraphOut,Vertex,DFNum):-
        getDFNumKey(Vertex,Key),
        GraphOut = GraphIn.put(Key,DFNum).

getAncestorKey(Vertex,Key):-
        term_to_atom(ancestor-Vertex,Key).
getAncestor(Graph,Vertex,Ancestor):-
        getAncestorKey(Vertex,Key),
        Ancestor = Graph.get(Key).
setAncestor(GraphIn,GraphOut,Vertex,Ancestor):-
        getAncestorKey(Vertex,Key),
        GraphOut = GraphIn.put(Key,Ancestor).

getTKey(Vertex,Key):-
        term_to_atom(t-Vertex,Key).
getT(Graph,Vertex,T):-
        getTKey(Vertex,Key),
        T = Graph.get(Key).
setT(GraphIn,GraphOut,Vertex,T):-
        getTKey(Vertex,Key),
        GraphOut = GraphIn.put(Key,T).


%% vprop and eprop are initialized to []
add_vertex_property(GraphIn,GraphOut,VPropKey,VPropString):-
        VPropList = GraphIn.get(vprop),
        VProp = [VPropKey,VPropString],
        GraphOut = GraphIn.put(vprop,[VProp|VPropList]).
add_edge_property(GraphIn,GraphOut,EPropKey,EPropString):-
        EPropList = GraphIn.get(eprop),
        EProp = [EPropKey,EPropString],
        GraphOut = GraphIn.put(eprop,[EProp|EPropList]).


%% insert_edges: while creating a new graph, should be called on an empty
%%   graph.  Inserts edges from a sorted list into the graph dictionary.
%%   Checks for doubles.  Insert each vertex as an adjacency list.
%%   Each edge should be represented by edge(A,B) and edge(B,A0), which is
%%   accomplished by two_directions_edges(EdgesIn,EdgesOut).
insert_edges([edge(From,To)|Tail],CFrom,CList,GraphIn,GraphOut):-
        From \= CFrom,!,
        setAdj(GraphIn,Graph1,CFrom,CList),
        insert_edges(Tail,From,[To],Graph1,GraphOut).
insert_edges([edge(From,To)|Tail],From,CList,GraphIn,GraphOut):-
        CList = [CTo|_],
        To \= CTo,!,
        insert_edges(Tail,From,[To|CList],GraphIn,GraphOut).
insert_edges([edge(From,To)|Tail],From,CList,GraphIn,GraphOut):-
        CList = [To|_],!,
        insert_edge(Tail,From,CList,GraphIn,GraphOut).
insert_edges([],CFrom,CList,GraphIn,GraphOut):-
        setAdj(GraphIn,GraphOut,CFrom,CList).

%% DFS that assigns properties to edges: tree-edge or frond
dfs_fronds(GraphIn,GraphOut):-
        Root = GraphIn.get(root),
        dfs_fronds_rec(GraphIn,GraphOut,Root,[],_).
dfs_fronds_rec(GraphIn,GraphOut,Vertex,AccIn,AccOut):-
        getAdj(GraphIn,Vertex,Adj),
        dfs_fronds_list(GraphIn,GraphOut,Vertex,Adj,[Vertex|AccIn],AccOut).
dfs_fronds_list(GraphIn,GraphOut,Vertex,[H|T],AccIn,AccOut):-
        member(H,AccIn),!,
        (   getEdgeType(GraphIn,H,Vertex,EdgeType)
        ->  setEdgeType(GraphIn,Graph1,Vertex,H,EdgeType)
        ;   setEdgeType(GraphIn,Graph1,Vertex,H,frond)),
        dfs_fronds_list(Graph1,GraphOut,Vertex,T,AccIn,AccOut).
dfs_fronds_list(GraphIn,GraphOut,Vertex,[H|T],AccIn,AccOut):-
        nonmember(H,AccIn),!,
        setEdgeType(GraphIn,Graph1,Vertex,H,tree-edge),
        dfs_fronds_rec(Graph1,Graph2,H,AccIn,Acc1),
        dfs_fronds_list(Graph2,GraphOut,Vertex,T,Acc1,AccOut).
dfs_fronds_list(Graph,Graph,_,[],Acc,Acc).


dfs_count_vertices(Graph,Count):-
        Root = Graph.get(root),
        dfs_count_vertices_rec(Graph,Root,[],_,0,Count).
dfs_count_vertices_rec(G,V,A,AOut,C,COut):-
        getAdj(G,V,Adj),
        C1 is C+1,
        dfs_count_vertices_list(G,Adj,[V|A],AOut,C1,COut).
dfs_count_vertices_list(G,[H|T],A,AOut,C,COut):-
        member(H,A),!,
        dfs_count_vertices_list(G,T,A,AOut,C,COut).
dfs_count_vertices_list(G,[H|T],A,AOut,C,COut):-
        nonmember(H,A),!,
        dfs_count_vertices_rec(G,H,A,A1,C,C1),
        dfs_count_vertices_list(G,T,A1,AOut,C1,COut).
dfs_count_vertices_list(_,[],A,A,C,C).

dfs_dfnum(GraphIn,GraphOut):-
        Root = GraphIn.get(root),
        add_vertex_property(GraphIn,Graph1,dfnum,"D"),
        dfs_dfnum_rec(Graph1,GraphOut,Root,[Root],_,1,_).
dfs_dfnum_rec(G,GOut,V,A,AOut,N,NOut):-
        setDFNum(G,G1,V,N),
        getAdj(G1,V,Adj),
        dfs_dfnum_list(G1,GOut,Adj,A,AOut,N,NOut).
dfs_dfnum_list(G,GOut,[H|T],A,AOut,N,NOut):-
        member(H,A),!,
        dfs_dfnum_list(G,GOut,T,A,AOut,N,NOut).
dfs_dfnum_list(G,GOut,[H|T],A,AOut,N,NOut):-
        nonmember(H,A),!,
        N1 is N+1,
        dfs_dfnum_rec(G,G1,H,[H|A],A1,N1,N2),
        dfs_dfnum_list(G1,GOut,T,A1,AOut,N2,NOut).
dfs_dfnum_list(G,G,_,A,A,N,N).


%% dfs_dot: write dot representation of the graph to file
dfs_dot(Graph,VPropList,EPropList,Out):-
        write(Out,"graph {\n"),
        Root = Graph.get(root),
        dfs_dot_rec(Graph,VPropList,EPropList,Root,[],_,Out),
        write(Out,"}\n").
makeVPropString(G,V,[[P,S]|T],VPropStr):-
        makeVPropString(G,V,T,SubStr),!,
        term_to_atom(P-V,K),
        (   Val = G.get(K)
        ->  string_concat(S,":",S1),
            term_to_atom(Val,ValAtom),
            string_concat(S1,ValAtom,S2),
            string_concat(S2,"<br/>",S3),
            string_concat(S3,SubStr,VPropStr)
        ;   VPropStr = "").
makeVPropString(_,_,[],"").
makeVString(V,"",VStr):-
        string_concat("    ",V,VStr),!.
makeVString(V,VPropStr,VStr):-
        string_concat("    ",V,S1),
        string_concat(S1," [xlabel=<<FONT POINT-SIZE=\"12\">",S2),
        string_concat(S2,VPropStr,S3),
        string_concat(S3,"</FONT>>]\n",VStr).
dfs_dot_rec(G,Vs,Es,V,A,AOut,Out):-
        makeVPropString(G,V,Vs,VPropStr),
        makeVString(V,VPropStr,VStr),
        write(Out,VStr),
        getAdj(G,V,Adj),
        dfs_dot_list(G,Vs,Es,V,Adj,A,AOut,Out).
makeEPropString(G,From,To,EPropList,EPropString):-
        todo(G,From,To,EPropList,EPropString).
makeEString(From,To,frond,EString,EPropString):-
        string_concat("    ",From,S1),
        string_concat(S1," -- ",S2),
        string_concat(S2,To,S3),
        todo(EPropString),
        string_concat(S3," [style=dashed]\n",EString),!.
makeEString(From,To,tree-edge,EString,EPropString):-
        string_concat("    ",From,S1),
        string_concat(S1," -- ",S2),
        string_concat(S2,To,S3),
        todo(EPropString),
        string_concat(S3," [penwidth=1.5]\n",EString),!.
makeEString(From,To,_,EString,_):-
        string_concat("    ",From,S1),
        string_concat(S1," -- ",S2),
        string_concat(S2,To,S3),
        string_concat(S3,"\n",EString).
minmax(A,B,B,A):-
        atom_greater(A,B),!.
minmax(A,B,A,B).
dfs_dot_list(G,Vs,Es,V,[H|T],A,AOut,Out):-
        minmax(V,H,Min,Max),
        member(edge(Min,Max),A),!,
        dfs_dot_list(G,Vs,Es,V,T,A,AOut,Out).
dfs_dot_list(G,Vs,Es,V,[H|T],A,AOut,Out):-
        minmax(V,H,Min,Max),
        nonmember(edge(Min,Max),A),
        getEdgeType(G,V,H,frond),!,
        makeEPropString(G,V,H,Es,EPropString),
        makeEString(V,H,frond,EString,EPropString),
        write(Out,EString),
        dfs_dot_list(G,Vs,Es,V,T,[edge(Min,Max)|A],AOut,Out).
dfs_dot_list(G,Vs,Es,V,[H|T],A,AOut,Out):-
        minmax(V,H,Min,Max),
        nonmember(edge(Min,Max),A),
        getEdgeType(G,V,H,tree-edge),!,
        makeEPropString(G,V,H,Es,EPropString),
        makeEString(V,H,tree-edge,EString,EPropString),
        write(Out,EString),
        dfs_dot_rec(G,Vs,Es,H,[edge(Min,Max)|A],A1,Out),
        dfs_dot_list(G,Vs,Es,V,T,A1,AOut,Out).
dfs_dot_list(_,_,_,_,[],A,A,_).

dfs_ancestor(GraphIn,GraphOut):-
        Root = GraphIn.get(root),
        add_vertex_property(GraphIn,Graph1,ancestor,"ancestor"),
        dfs_ancestor_rec(Graph1,GraphOut,Root,[],_).
dfs_ancestor_rec(G,GOut,V,A,AOut):-
        getAdj(G,V,Adj),
        dfs_ancestor_list(G,GOut,V,Adj,[V|A],AOut).
dfs_ancestor_list(G,GOut,V,[H|T],A,AOut):-
        member(H,A),!,
        dfs_ancestor_list(G,GOut,V,T,A,AOut).
dfs_ancestor_list(G,GOut,V,[H|T],A,AOut):-
        nonmember(H,A),
        getEdgeType(G,V,H,frond),!,
        dfs_ancestor_list(G,GOut,V,T,A,AOut).
dfs_ancestor_list(G,GOut,V,[H|T],A,AOut):-
        nonmember(H,A),
        getEdgeType(G,V,H,tree-edge),!,
        setAncestor(G,G1,H,V),
        dfs_ancestor_rec(G1,G2,H,A,A2),
        dfs_ancestor_list(G2,GOut,V,T,A2,AOut).
dfs_ancestor_list(G,G,_,[],A,A).

insert(H,[],[H]):-!.
insert(H,[H|T],[H|T]):-!.
insert(X,[H|T],[H|T2]):-
        atom_greater(X,H),!,
        insert(X,T,T2).
insert(X,[H|T],[X,H|T]).

insert_all([],L,L).
insert_all([H|T],L,R):-
        insert(H,L,L1),
        insert_all(T,L1,R).

dfs_t(GraphIn,GraphOut):-
        Root = GraphIn.get(root),
        add_vertex_property(GraphIn,Graph1,t,"T"),
        dfs_t_rec(Graph1,GraphOut,Root,[],_,_).
dfs_t_rec(G,GOut,V,A,AOut,T_Out):-
        getAdj(G,V,Adj),
        dfs_t_list(G,G1,V,Adj,[V|A],AOut,[],T_1),
        insert(V,T_1,T_Out),
        setT(G1,GOut,V,T_Out).
dfs_t_list(G,GOut,V,[H|T],A,AOut,T_In,T_Out):-
        getEdgeType(G,V,H,tree-edge),
        nonmember(H,A),!,
        insert(H,T_In,T_1),
        dfs_t_rec(G,G1,H,A,A1,T_H),
        insert_all(T_H,T_1,T_2),
        dfs_t_list(G1,GOut,V,T,A1,AOut,T_2,T_Out).
dfs_t_list(G,GOut,V,[H|T],A,AOut,T_In,T_Out):-
        getEdgeType(G,V,H,tree-edge),
        member(H,A),!,
        insert(H,T_In,T_1),
        dfs_t_list(G,GOut,V,T,A,AOut,T_1,T_Out).
dfs_t_list(G,GOut,V,[H|T],A,AOut,T_In,T_Out):-
        getEdgeType(G,V,H,frond),!,
        insert(H,T_In,T_1),
        dfs_t_list(G,GOut,V,T,A,AOut,T_1,T_Out).
dfs_t_list(G,G,_,[],A,A,T,T).


graph1([
        edge(a,b),
        edge(a,h),
        edge(a,i),
        edge(a,l),
        edge(b,c),
        edge(b,j),
        edge(b,l),
        edge(c,d),
        edge(c,j),
        edge(c,k),
        edge(d,e),
        edge(d,k),
        edge(d,l),
        edge(e,f),
        edge(e,i),
        edge(e,l),
        edge(f,g),
        edge(f,i),
        edge(f,k),
        edge(g,h),
        edge(g,j),
        edge(g,k),
        edge(h,i),
        edge(h,j)
       ]).




test:-
        Edges = [edge(a,d),edge(a,b),edge(b,c),edge(b,d), edge(c,d)],
        edges_to_dict(Edges,GraphDict),
%%        write(GraphDict),nl,
        dfs_fronds(GraphDict,G2),
%%        write(G2),nl,
%%        dfs_count_vertices(G2,Count),
%%        write(Count),nl,
        dfs_dfnum(G2,G3),
%        write(G3),nl,
        dfs_ancestor(G3,G4),
%        write(G4),nl,
        dfs_t(G4,G5),
%        write(G5),nl,
        show_dot(G5).

test1:-
        graph1(Edges),
        edges_to_dict(Edges,G0),
        dfs_fronds(G0,G2),
        dfs_dfnum(G2,G3),
        dfs_ancestor(G3,G4),
        dfs_t(G4,G5),
        show_dot(G5,[dfnum]).
