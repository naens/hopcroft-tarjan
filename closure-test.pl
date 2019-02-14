map(F,[H|T],[H1|T1]):-
        call(F,H,H1),
        map(F,T,T1),!.
map(_,[],[]).

proc1(N,In,Out):-
        Out is N*10 + In.

test:-
        ListIn = [1,2,3,4,5],
        map(proc1(3), ListIn, ListOut),
        write(ListOut).
