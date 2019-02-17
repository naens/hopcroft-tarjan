test:-
        A = dic{a:x,'q(b,r)':d},
        write(A),nl,
        Key1 = edge-a,
        Value1 = b-t,
        Key2 = q(b,r),
        Value2 = [a,b,c,d],
        term_to_atom(Key1,Key1atom),
        term_to_atom(Key2,Key2atom),
        B = A.put(Key1atom, Value1),
        C = B.put(Key2atom, Value2),
        write(C).
