show_dot(DotString):-
        open('/tmp/swipl_g.dot',write,Out),
        write(Out,DotString),
        close(Out),
        shell('dot -Tpng /tmp/swipl_g.dot -o /tmp/swipl_g.png'),
        shell('feh /tmp/swipl_g.png').

map(F,[H|T],[H1|T1]):-
        call(F,H,H1),
        map(F,T,T1),!.
map(_,[],[]).

nonmember(_,[]).
nonmember(X,[H|T]):-
        X \= H,
        nonmember(X,T).

find_vertex(Root,Vertex,Result):-
        find_vertex_rec(Root,Vertex,[],Result).
find_vertex_rec(vertex(Name,List),Vertex,Acc,Result):-
        find_vertex_in_list(List,Vertex,[Name|Acc],Result).
find_vertex_in_list([edge(H)|_],H,_,H).
find_vertex_in_list([edge(H)|T],Vertex,Acc,Result):-
        H \= Vertex,
        H = vertex(Name,_),
        (   nonmember(Name,Acc),
            find_vertex_rec(H,Vertex,Acc,Result),!;
            find_vertex_in_list(T,Vertex,Acc,Result)),!.

test1:-
        A = vertex(a,[edge(B),edge(D)]),
        B = vertex(b,[edge(A),edge(C),edge(D)]),
        C = vertex(c,[edge(B)]),
        D = vertex(d,[edge(B),edge(A)]),
        simple_dot(A,DotString),
        show_dot(DotString).
        %% find_vertex(A,C,vertex(Name,_)),
        %% write(Name).

%% find unique edges of a graph
graph_edges(vertex(Name,List),Acc,Edges):-
        graph_edges_list(Name,List,Acc,Edges).
graph_edges_list(_,[],Acc,Acc):-!.
graph_edges_list(Name,[edge(H)|T],Acc,Edges):-
        H = vertex(Name2,_),
        (   nonmember(edge(Name,Name2),Acc),
            nonmember(edge(Name2,Name),Acc)
        ->  graph_edges(H,[edge(Name,Name2)|Acc],Acc1);
            Acc1 = Acc),
        graph_edges_list(Name,T,Acc1,Edges).

edge_to_dot(edge(A,B),R):-
        string_concat("    ", A, Str1),
        string_concat(Str1, " -- ", Str2),
        string_concat(Str2, B, Str3),
        string_concat(Str3, "\n", R).

edges_to_dot([],[]):-!.
edges_to_dot([H|T],[H1|T1]):-
        edge_to_dot(H,H1),
        edges_to_dot(T,T1).

%% create simple graph representation
simple_dot(Root,DotString):-
        graph_edges(Root,[],Edges),
        edges_to_dot(Edges,DotEdgeStrings),
        concat_string_list(DotEdgeStrings,Str1),
        string_concat("graph {\n", Str1, Str2),
        string_concat(Str2, "}\n", DotString).
concat_string_list([Str],Str):-!.
concat_string_list([Str|T],Result):-
        concat_string_list(T,Str1),
        string_concat(Str,Str1,Result).

%% adds a vertex as edge to the list of edges
add_edge_to_list([],V,[edge(V)]).
add_edge_to_list([edge(H)|T],V,[edge(V)|T]):-
        H = vertex(Name,_),
        V = vertex(Name,_),!.
add_edge_to_list([edge(H)|T],V,[edge(H)|T1]):-
        H = vertex(Name1,_),
        V = vertex(Name2,_),
        Name1 \= Name2,
        add_edge_to_list(T,V,T1).

%% search list of vertices, replace vertex by name in each list of each vertex
replace_vertex_in_lists([],_,[]).
replace_vertex_in_lists([H|T],V,[H1|T1]):-
        H = vertex(N,L),
        replace_vertex_in_list(L,V,L1),
        H1 = vertex(N,L1),
        replace_vertex_in_lists(T,V,T1).

%% search the list of edges in a vertex, replace vertex by name
replace_vertex_in_list([],_,[]):-!.
replace_vertex_in_list([edge(H)|T],V,[edge(V)|T]):-
        H = vertex(Name,_),
        V = vertex(Name,_),!.
replace_vertex_in_list([edge(H)|T],V,[edge(H)|T1]):-
        H = vertex(HName,_),
        V = vertex(VName,_),
        HName \= VName,
        replace_vertex_in_list(T,V,T1).

%% delete_vertex_return_list(GraphIn,NameIn,ListOut,GraphOut)
%% deletes vertex with name NameIn and returns its list and the modified graph
delete_vertex_return_list([],_,[],[]).
delete_vertex_return_list([H|T],N,L,T):-
        H = vertex(N,L),!.
delete_vertex_return_list([H|T],N,L,[H|T1]):-
        H = vertex(HN,_),
        N \= HN,!,
        delete_vertex_return_list(T,N,L,T1).

%% adds an edge to a graph.
%% first takes the vertices from the graph
%%  then, adds the edge to them
%%  then, prepends to graph and updates the references to point to new vertices
add_edge_to_graph(Graph0,Name1,Name2,GraphOut):-
        delete_vertex_return_list(Graph0,Name1,List1,Graph1),
        delete_vertex_return_list(Graph1,Name2,List2,Graph2),
        Vertex1 = vertex(Name1,List1),
        Vertex2 = vertex(Name2,List1),
        add_edge_to_list(List1,Vertex2,List1a),
        add_edge_to_list(List2,Vertex1,List2a),
        Vertex1a = vertex(Name1,List1a),
        Vertex2a = vertex(Name2,List2a),
        Graph3 = [Vertex1a,Vertex2a|Graph2],
        replace_vertex_in_lists(Graph3,Vertex1a,Graph4),
        replace_vertex_in_lists(Graph4,Vertex2a,GraphOut).

edges_to_graph(Edges,Vertices):-
        edges_to_graph_rec(Edges,[],Vertices).
edges_to_graph_rec([],A,A):-!.
edges_to_graph_rec([edge(N1,N2)|T],A,R):-
        add_edge_to_graph(A,N1,N2,A1),
        edges_to_graph_rec(T,A1,R).

test2:-
        Edges = [edge(a,d),edge(a,b),edge(b,c),edge(b,d), edge(c,d)],
        edges_to_graph(Edges,Vertices),
        Vertices = [First|_],
        simple_dot(First,DotString),
        show_dot(DotString).

dfs_node_count(Root,Count):-
        dfs_node_count_rec(Root,0,Count,[],_).
dfs_node_count_rec(vertex(N,_),C,C,A,A):-
        member(N,A),!.
dfs_node_count_rec(V,C,COut,A,AOut):-
        V = vertex(N,L),
        nonmember(N,A),!,
        C1 is C+1,
        dfs_node_count_list(L,C1,COut,[N|A],AOut).
dfs_node_count_list([],C,C,A,A).
dfs_node_count_list([edge(H)|T],C,COut,A,AOut):-
        dfs_node_count_rec(H,C,C1,A,A1),
        dfs_node_count_list(T,C1,COut,A1,AOut).

test3:-
        Edges = [edge(a,d),edge(a,b),edge(b,c),edge(c,e)],
        edges_to_graph(Edges,Vertices),
        Vertices = [Root|_],
        dfs_node_count(Root,Count),
        write(Count),
        simple_dot(Root,DotString),
        show_dot(DotString).

dfs_numbers(RootIn,RootOut):-
        dfs_number_rec(RootIn,0,_,[],RootOut).
dfs_numbers_rec(VertexIn,CountIn,CountOut,AccIn,AccOut,VertexOut):-
        VertexIn = vertex(Name,Properties,List),
        nonmember(Name,AccIn),
        P1 = [[d,CountIn]|Properties],
        C1 is CountIn+1,
        A1 = [Name|AccIn],
        dfs_numbers_list(List,C1,CountOut,A1,AccOut,ListOut),
        VertexOut = vertex(Name,P1,ListOut).
dfs_numbers_list(ListIn,ListOut,CountIn,CountOut,AccIn,AccOut):-
        ListIn = [edge(Vertex)|Tail],
        dfs_number_list(Tail,T1,CountIn,C1,AccIn,A1),
        (   dfs_numbers_rec(Vertex,C1,CountOut,A1,AccOut,V)
        ->  V = vertex(VName,VP,VList),
            add_to_list(VList,edge(P),V1List),
            V1 = vertex(VName,VP,V1List),
            add_to_list(T1,edge(V1),TailOut)
        ;   V = vertex(VName,VP,VList),
            add_to_list(VList,frond(P),V1List),
            V1 = vertex(VName,VP,V1List),
            add_to_list(T1,frond(V1),TailOut)),
        ListOut = TailOut.
dfs_numbers_list([],[],C,C,A,A).
