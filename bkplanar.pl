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
intlist_greater([_|_],[]).

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
get_adj_key(Vertex,Key):-
        term_to_atom(adj-Vertex,Key).
get_adj(Graph,Vertex,Adj):-
        get_adj_key(Vertex,Key),
        Adj = Graph.get(Key).
set_adj(GraphIn,GraphOut,Vertex,Adj):-
        get_adj_key(Vertex,Key),
        GraphOut = GraphIn.put(Key,Adj).

get_edge_type_key(From,To,Key):-
        term_to_atom(edge(From,To),Key).
get_edge_type(Graph,From,To,Type):-
        get_edge_type_key(From,To,Key),
        Type = Graph.get(Key).
set_edge_type(GraphIn,GraphOut,From,To,Type):-
        get_edge_type_key(From,To,Key),
        GraphOut = GraphIn.put(Key,Type).

get_dfnum_key(Vertex,Key):-
        term_to_atom(dfnum-Vertex,Key).
get_dfnum(Graph,Vertex,DFNum):-
        get_dfnum_key(Vertex,Key),
        DFNum = Graph.get(Key).
set_dfnum(GraphIn,GraphOut,Vertex,DFNum):-
        get_dfnum_key(Vertex,Key),
        GraphOut = GraphIn.put(Key,DFNum).

get_ancestor_key(Vertex,Key):-
        term_to_atom(ancestor-Vertex,Key).
get_ancestor(Graph,Vertex,Ancestor):-
        get_ancestor_key(Vertex,Key),
        Ancestor = Graph.get(Key).
set_ancestor(GraphIn,GraphOut,Vertex,Ancestor):-
        get_ancestor_key(Vertex,Key),
        GraphOut = GraphIn.put(Key,Ancestor).

get_t_key(Vertex,Key):-
        term_to_atom(t-Vertex,Key).
get_t(Graph,Vertex,T):-
        get_t_key(Vertex,Key),
        T = Graph.get(Key).
set_t(GraphIn,GraphOut,Vertex,T):-
        get_t_key(Vertex,Key),
        GraphOut = GraphIn.put(Key,T).

get_l1_key(V,K):-
        term_to_atom(l1-V,K).
get_l1(G,V,L1):-
        get_l1_key(V,K),
        L1 = G.get(K).
set_l1(G,GOut,V,L1):-
        get_l1_key(V,K),
        GOut = G.put(K,L1).

get_l2_key(V,K):-
        term_to_atom(l2-V,K).
get_l2(G,V,L2):-
        get_l2_key(V,K),
        L2 = G.get(K).
set_l2(G,GOut,V,L2):-
        get_l2_key(V,K),
        GOut = G.put(K,L2).

get_edge_weight_key(V1,V2,K):-
        term_to_atom(w-(V1,V2),K).
get_edge_weight(G,V1,V2,W):-
        get_edge_weight_key(V1,V2,K),
        W = G.get(K).
set_edge_weight(G,GOut,V1,V2,W):-
        get_edge_weight_key(V1,V2,K),
        GOut = G.put(K,W).


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
        reverse(CList,CListR),
        set_adj(GraphIn,Graph1,CFrom,CListR),
        insert_edges(Tail,From,[To],Graph1,GraphOut).
insert_edges([edge(From,To)|Tail],From,CList,GraphIn,GraphOut):-
        CList = [CTo|_],
        To \= CTo,!,
        insert_edges(Tail,From,[To|CList],GraphIn,GraphOut).
insert_edges([edge(From,To)|Tail],From,CList,GraphIn,GraphOut):-
        CList = [To|_],!,
        insert_edges(Tail,From,CList,GraphIn,GraphOut).
insert_edges([],CFrom,CList,GraphIn,GraphOut):-
        reverse(CList,CListR),
        set_adj(GraphIn,GraphOut,CFrom,CListR).

%% DFS that assigns properties to edges: tree-edge or frond
dfs_fronds(GraphIn,GraphOut):-
        Root = GraphIn.get(root),
        dfs_fronds_rec(GraphIn,GraphOut,Root,[],_).
dfs_fronds_rec(GraphIn,GraphOut,Vertex,AccIn,AccOut):-
        get_adj(GraphIn,Vertex,Adj),
        dfs_fronds_list(GraphIn,GraphOut,Vertex,Adj,[Vertex|AccIn],AccOut).
dfs_fronds_list(GraphIn,GraphOut,Vertex,[H|T],AccIn,AccOut):-
        member(H,AccIn),!,
        (   get_edge_type(GraphIn,H,Vertex,EdgeType)
        ->  set_edge_type(GraphIn,Graph1,Vertex,H,EdgeType)
        ;   set_edge_type(GraphIn,Graph1,Vertex,H,frond)),
        dfs_fronds_list(Graph1,GraphOut,Vertex,T,AccIn,AccOut).
dfs_fronds_list(GraphIn,GraphOut,Vertex,[H|T],AccIn,AccOut):-
        nonmember(H,AccIn),!,
        set_edge_type(GraphIn,Graph1,Vertex,H,tree-edge),
        dfs_fronds_rec(Graph1,Graph2,H,AccIn,Acc1),
        dfs_fronds_list(Graph2,GraphOut,Vertex,T,Acc1,AccOut).
dfs_fronds_list(Graph,Graph,_,[],Acc,Acc).


dfs_count_vertices(Graph,Count):-
        Root = Graph.get(root),
        dfs_count_vertices_rec(Graph,Root,[],_,0,Count).
dfs_count_vertices_rec(G,V,A,AOut,C,COut):-
        get_adj(G,V,Adj),
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
        set_dfnum(G,G1,V,N),
        get_adj(G1,V,Adj),
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
        string_concat(S1," [xlabel=<<FONT POINT-SIZE=\"12\">",S2), % use label?
        string_concat(S2,VPropStr,S3),
        string_concat(S3,"</FONT>>]\n",VStr).
dfs_dot_rec(G,Vs,Es,V,A,AOut,Out):-
        makeVPropString(G,V,Vs,VPropStr),
        makeVString(V,VPropStr,VStr),
        write(Out,VStr),
        get_adj(G,V,Adj),
        dfs_dot_list(G,Vs,Es,V,Adj,A,AOut,Out).
%% TODO: what if other direction???
getEdgeVal(G,P,From,To,V):-
        term_to_atom(P-(From,To),K),
        (   Val = G.get(K)
        ->  V = Val
        ;   term_to_atom(P-(To,From),K2),
            V = G.get(K2)).
makeEPropString(G,From,To,[[P,S]|T],EPropString):-
        makeEPropString(G,From,To,T,SubStr),!,
        (   getEdgeVal(G,P,From,To,Val)
        ->  string_concat(S,":",S1),
            term_to_atom(Val,ValAtom),
            string_concat(S1,ValAtom,S2),
            string_concat(S2,"<br/>",S3),
            string_concat(S3,SubStr,EPropString)
        ;   EPropString = "").
makeEPropString(_,_,_,[],"").
makeELabelString("","").
makeELabelString(EPropString,LabelString):-
        EPropString \= "",
        string_concat(" [label=<",EPropString,Lab1),
        string_concat(Lab1,">]",LabelString).
makeEString(From,To,frond,EString,EPropString):-
        string_concat("    ",From,S1),
        string_concat(S1," -- ",S2),
        string_concat(S2,To,S3),
        makeELabelString(EPropString,LabelString),
        string_concat(S3,LabelString,S4),
        string_concat(S4," [style=dashed]\n",EString),!.
makeEString(From,To,tree-edge,EString,EPropString):-
        string_concat("    ",From,S1),
        string_concat(S1," -- ",S2),
        string_concat(S2,To,S3),
        string_concat(S3," [penwidth=1.5]",S4),!,
        makeELabelString(EPropString,LabelString),
        string_concat(S4,LabelString,S5),
        string_concat(S5,"\n",EString).
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
        get_edge_type(G,V,H,frond),!,
        makeEPropString(G,V,H,Es,EPropString),
        makeEString(V,H,frond,EString,EPropString),
        write(Out,EString),
        dfs_dot_list(G,Vs,Es,V,T,[edge(Min,Max)|A],AOut,Out).
dfs_dot_list(G,Vs,Es,V,[H|T],A,AOut,Out):-
        minmax(V,H,Min,Max),
        nonmember(edge(Min,Max),A),
        get_edge_type(G,V,H,tree-edge),!,
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
        get_adj(G,V,Adj),
        dfs_ancestor_list(G,GOut,V,Adj,[V|A],AOut).
dfs_ancestor_list(G,GOut,V,[H|T],A,AOut):-
        member(H,A),!,
        dfs_ancestor_list(G,GOut,V,T,A,AOut).
dfs_ancestor_list(G,GOut,V,[H|T],A,AOut):-
        nonmember(H,A),
        get_edge_type(G,V,H,frond),!,
        dfs_ancestor_list(G,GOut,V,T,A,AOut).
dfs_ancestor_list(G,GOut,V,[H|T],A,AOut):-
        nonmember(H,A),
        get_edge_type(G,V,H,tree-edge),!,
        set_ancestor(G,G1,H,V),
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
        get_adj(G,V,Adj),
        dfs_t_list(G,G1,V,Adj,[V|A],AOut,[],T_1),
        insert(V,T_1,T_Out),
        set_t(G1,GOut,V,T_Out).
dfs_t_list(G,GOut,V,[H|T],A,AOut,T_In,T_Out):-
        get_edge_type(G,V,H,tree-edge),
        nonmember(H,A),!,
        insert(H,T_In,T_1),
        dfs_t_rec(G,G1,H,A,A1,T_H),
        insert_all(T_H,T_1,T_2),
        dfs_t_list(G1,GOut,V,T,A1,AOut,T_2,T_Out).
dfs_t_list(G,GOut,V,[H|T],A,AOut,T_In,T_Out):-
        get_edge_type(G,V,H,tree-edge),
        member(H,A),!,
        insert(H,T_In,T_1),
        dfs_t_list(G,GOut,V,T,A,AOut,T_1,T_Out).
dfs_t_list(G,GOut,V,[H|T],A,AOut,T_In,T_Out):-
        get_edge_type(G,V,H,frond),!,
        insert(H,T_In,T_1),
        dfs_t_list(G,GOut,V,T,A,AOut,T_1,T_Out).
dfs_t_list(G,G,_,[],A,A,T,T).


skip([H|T],H,R):-
        skip(T,H,R),!.
skip([H|T],X,[H|T]):-
        X \= H.
skip([],_,[]).

%% get minimum number R from List not equal to X
min_ne(List,X,R):-
        skip(List,X,[H|T]),
        min_ne_rec(T,X,H,R).
min_ne_rec([H|T],H,A,R):-
        min_ne_rec(T,H,A,R),!.
min_ne_rec([H|T],X,A,R):-
        X \= H,
        A > H,!,
        min_ne_rec(T,X,H,R).
min_ne_rec([H|T],X,A,R):-
        X \= H,
        A =< H,!,
        min_ne_rec(T,X,A,R).
min_ne_rec([],_,A,A).

min(X,X,X):-!.
min(X1,X2,X1):-
        X1 =< X2,!.
min(X1,X2,X2):-
        X1 > X2.

%% get L1 and L2 properties and set them for every vertex
dfs_l1l2(G,GOut):-
        Root = G.get(root),
        add_vertex_property(G,G1,l1,"L1"),
        add_vertex_property(G1,G2,l2,"L2"),
        dfs_l1l2_rec(G2,GOut,Root,Root,[],_,_,_).
dfs_l1l2_rec(G,GOut,P,V,A,AOut,L1,L2):-
        get_adj(G,V,Adj),
        get_dfnum(G,P,PNum),
        get_dfnum(G,V,VNum),
        dfs_l1l2_list(G,G1,V,Adj,[V|A],AOut,PNum,L1,VNum,L2),
        set_l1(G1,G2,V,L1),
        set_l2(G2,GOut,V,L2).
dfs_l1l2_list(G,GOut,V,[H|T],A,AOut,L1_In,L1_Out,L2_In,L2_Out):-
        (   get_edge_type(G,V,H,tree-edge), nonmember(H,A)),!,
        dfs_l1l2_rec(G,G1,V,H,A,A1,SubL1,SubL2),
        min(L1_In,SubL1,L1),
        min_ne([L1_In,L2_In,SubL1,SubL2],L1,L2),
        dfs_l1l2_list(G1,GOut,V,T,A1,AOut,L1,L1_Out,L2,L2_Out).
dfs_l1l2_list(G,GOut,V,[H|T],A,AOut,L1_In,L1_Out,L2_In,L2_Out):-
        get_edge_type(G,V,H,EdgeType),
        (   EdgeType = frond
        ;   EdgeType = tree-edge, member(H,A)),!,
        get_dfnum(G,H,D),
        min(L1_In,D,L1),
        min_ne([L1_In,L2_In,D],L1,L2),
        dfs_l1l2_list(G,GOut,V,T,A,AOut,L1,L1_Out,L2,L2_Out).
dfs_l1l2_list(G,G,_,[],A,A,L1,L1,L2,L2).

dfs_w(G,GOut):-
        Root = G.get(root),
        add_edge_property(G,G1,w,"W"),
        dfs_w_rec(G1,GOut,Root).
dfs_w_rec(G,GOut,V):-
        get_adj(G,V,Adj),
        dfs_w_list(G,GOut,V,Adj).
dfs_w_list(G,GOut,V,[H|T]):-
        get_dfnum(G,V,DV),
        get_dfnum(G,H,DH),
        DH < DV,!,
        dfs_w_list(G,GOut,V,T).
dfs_w_list(G,GOut,V,[H|T]):-
        get_dfnum(G,V,DV),
        get_dfnum(G,H,DH),
        DH > DV,
        get_edge_type(G,V,H,frond),!,
        W is 2*DH,
        set_edge_weight(G,G1,V,H,W),
        dfs_w_list(G1,GOut,V,T).
dfs_w_list(G,GOut,V,[H|T]):-
        get_dfnum(G,V,DV),
        get_dfnum(G,H,DH),
        DH > DV,
        get_edge_type(G,V,H,tree-edge),
        get_l1(G,H,L1),
        get_l2(G,H,L2),
        (   L1 = DV
        ->  W is 2*L2
        ;   W is 2*L2 + 1),!,
        set_edge_weight(G,G1,V,H,W),
        dfs_w_rec(G1,G2,H),
        dfs_w_list(G2,GOut,V,T).
dfs_w_list(G,G,_,[]).

%% sort the adjacency lists
sort_adjacency_list(G,GOut,V,NewAdj):-
        get_adj(G,V,Adj),
        make_wname_list(G,V,Adj,List,NoWList),
        sort_wname_list(List,ListSorted),
        make_simple_adjlist(ListSorted,AdjSorted),
        append(AdjSorted,NoWList,NewAdj),
        set_adj(G,GOut,V,NewAdj).

make_wname_list(G,V,[H|T],[[[W,H]]|L],NoWList):-
        get_edge_weight(G,V,H,W),!,
        make_wname_list(G,V,T,L,NoWList).
make_wname_list(G,V,[H|T],List,[H|NoWList]):-
        \+ get_edge_weight(G,V,H,_),!,
        make_wname_list(G,V,T,List,NoWList).
make_wname_list(_,_,[],[],[]).

make_simple_adjlist([[_,Name]|T],[Name|T2]):-
        make_simple_adjlist(T,T2),!.
make_simple_adjlist([],[]).

sort_wname_list(L,R):-
        sort_wname_list1(L,[R]).
sort_wname_list1([],[[]]):-!.
sort_wname_list1([X],[X]):-!.
sort_wname_list1(L,R):-
        L = [_,_|_],!,
        sort_wname_list0(L,L1),
        sort_wname_list1(L1,R).
sort_wname_list0([A,B|T],[C|T2]):-
        sort_wname_merge(A,B,C),!,
        sort_wname_list0(T,T2).
sort_wname_list0([A],[A]):-!.
sort_wname_list0([],[]).

sort_wname_merge([H1|T1],[H2|T2],[H1|T]):-
        H1 = [W1,_],
        H2 = [W2,_],
        W1 < W2,!,
        sort_wname_merge(T1,[H2|T2],T).
sort_wname_merge([H1|T1],[H2|T2],[H2|T]):-
        H1 = [W1,_],
        H2 = [W2,_],
        W1 >= W2,!,
        sort_wname_merge([H1|T1],T2,T).
sort_wname_merge([],L,L):-!.
sort_wname_merge(L,[],L).

dfs_sort_adj(G,GOut):-
        Root = G.get(root),
        dfs_sort_adj_rec(G,GOut,Root,[],_).
dfs_sort_adj_rec(G,GOut,V,A,AOut):-
        sort_adjacency_list(G,G1,V,NewAdj),
        dfs_sort_adj_list(G1,GOut,NewAdj,[V|A],AOut).
dfs_sort_adj_list(G,GOut,[H|T],A,AOut):-
        nonmember(H,A),!,
        dfs_sort_adj_rec(G,G1,H,A,A1),
        dfs_sort_adj_list(G1,GOut,T,A1,AOut).
dfs_sort_adj_list(G,GOut,[H|T],A,AOut):-
        member(H,A),!,
        dfs_sort_adj_list(G,GOut,T,A,AOut).
dfs_sort_adj_list(G,G,[],A,A).

dfs_lowpt(G_in, G_out):-
        Root = G_in.get(root),
        add_vertex_property(G_in,G_1,dfnum,"D"),
        add_vertex_property(G_1,G_2,ancestor,"ancestor"),
        add_vertex_property(G_2,G_3,l1,"L1"),
        add_vertex_property(G_3,G_4,l2,"L2"),
        add_edge_property(G_4,G_5,w,"W"),
        dfs_lowpt_rec(G_5,G_out,Root,Root,1,_,_,_,[],_).
dfs_lowpt_rec(G_in,G_out,P,U,D_in,D_out,L1,L2,A_in,A_out):-
        set_dfnum(G_in,G_1,U,D_in),
        D_1 is D_in+1,
        get_adj(G_1,U,Adj),
        get_dfnum(G_1,P,D_P),
        dfs_lowpt_list(G_1,G_2,U,Adj,D_1,D_out,D_P,L1,D_in,L2,[U|A_in],A_out),
        set_l1(G_2,G_3,U,L1),
        set_l2(G_3,G_out,U,L2).
dfs_lowpt_list(G_in,G_out,U,[V|Vs],D_in,D_out,L1_in,L1_out,L2_in,L2_out,A_in,A_out):-
        nonmember(V,A_in),!,
        set_edge_type(G_in,G_1,U,V,tree-edge),
        set_edge_type(G_1,G_2,V,U,tree-edge),
        set_ancestor(G_2,G_3,V,U),
        dfs_lowpt_rec(G_3,G_4,U,V,D_in,D_1,L1_V,L2_V,A_in,A_1),
        min(L1_in,L1_V,L1_Curr),
        min_ne([L1_in,L1_V,L2_in,L2_V],L1_Curr,L2_Curr),
        (   D_in = L1_V
        ->  W is 2*L2_V
        ;   W is 2*L2_V + 1),
        set_edge_weight(G_4,G_5,U,V,W),
        dfs_lowpt_list(G_5,G_out,U,Vs,D_1,D_out,L1_Curr,L1_out,L2_Curr,L2_out,A_1,A_out).
dfs_lowpt_list(G_in,G_out,U,[V|Vs],D_in,D_out,L1_in,L1_out,L2_in,L2_out,A_in,A_out):-
        member(V,A_in),
        \+ get_ancestor(G_in,U,V),!,
        set_edge_type(G_in,G_1,U,V,frond),
        set_edge_type(G_1,G_2,U,V,frond),
        get_dfnum(G_1,V,D_V),
        get_dfnum(G_1,U,D_U),
        (   D_U < D_V
        ->  W is 2*D_V,
            set_edge_weight(G_2,G_3,U,V,W)
        ;   G_3 = G_2),
        min(L1_in,D_V,L1_Curr),
        min_ne([L1_in,D_V,L2_in],L1_Curr,L2_Curr),
        dfs_lowpt_list(G_3,G_out,U,Vs,D_in,D_out,L1_Curr,L1_out,L2_Curr,L2_out,A_in,A_out).
dfs_lowpt_list(G_in,G_out,U,[V|Vs],D_in,D_out,L1_in,L1_out,L2_in,L2_out,A_in,A_out):-
        member(V,A_in),
        get_ancestor(G_in,U,V),!,
        write("ancestor of "),write(U),write(": "),writeln(V),
        dfs_lowpt_list(G_in,G_out,U,Vs,D_in,D_out,L1_in,L1_out,L2_in,L2_out,A_in,A_out).
dfs_lowpt_list(G,G,_,[],D,D,L1,L1,L2,L2,A,A).



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

graph2([
        edge(1,a),
        edge(a,b),
        edge(b,u),

        edge(u,uv),
        edge(uv,y),
        edge(y,a),
        edge(y,b),
        edge(y,s),
        edge(s,a),

        edge(u,a),

        edge(u,x),
        edge(x,c),
        edge(c,1),

        edge(u,w),
        edge(u,w),
        edge(w,wt),
        edge(wt,u),
        edge(wt,b),

        edge(u,b)
       ]).

test1:-
        graph1(Edges),
        edges_to_dict(Edges,G0),
        dfs_fronds(G0,G2),
        dfs_dfnum(G2,G3),
        dfs_ancestor(G3,G4),
        dfs_t(G4,G5),
        dfs_l1l2(G5,G6),
        dfs_w(G6,G7),
%%        writeln(G7),
        dfs_sort_adj(G7,G8),
        writeln(G8),
        show_dot(G8,[dfnum,l1,l2,w]).

test2:-
        graph2(Edges),
        edges_to_dict(Edges,G_0),
        dfs_lowpt(G_0,G_1),
        writeln("dfs_lowpt: finished"),
        %%writeln(G_1),
        dfs_sort_adj(G_1,G_2),
        show_dot(G_2,[dfnum,l1,l2,w]).
