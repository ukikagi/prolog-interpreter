male(kobo).
male(koji).
male(iwao).
female(miho).
female(sanae).
female(mine).

parent(kobo, koji).
parent(kobo, sanae).
parent(miho, koji).
parent(miho, sanae).
parent(sanae, iwao).
parent(sanae, mine).

father(X, Y) :- parent(X, Y), male(Y).
mother(X, Y) :- parent(X, Y), female(Y).

grandparent(X,Z) :- parent(X,Y), parent(Y,Z).

ancestor(X,Z) :- ancestor(Y,Z), parent(X,Y).
ancestor(X,Z) :- parent(X,Z).

sibling(X, Y) :- parent(X, Z), parent(Y, Z).
bloodrelative(X, Y) :- ancestor(X, Z), ancestor(Y, Z).

reverse(Xs, Ys) :- reverse(Xs, [], Ys).
reverse([], Ys, Ys).
reverse([X | Xs], Ys, Zs) :- reverse(Xs, [X | Ys], Zs).

append([], Ys, Ys).
append([X | Xs], Ys, [X | Zs]) :- append(Xs, Ys, Zs).

concat([], []).
concat([Xs | Xss], Ys) :- append(Xs, Zs, Ys), concat(Xss, Zs).

nat(z).
nat(s(N)) :- nat(N).

nat_list([]).
nat_list([N|X]) :- nat(N), nat_list(X).

equal(X, X).

add(X, z, X).
add(X, s(Y), s(Z)) :- add(X, Y, Z).

test :- q(X, X).
q(X, f(X)).

eq(a, b).
eq(c, b).
eq(X, Z) :- eq(X, Y), eq(Y, Z).
eq(X, Y) :- eq(Y, X).

choose([X | Xs], X, Xs).
choose([Y | Ys], X, [Y | Xs]) :- choose(Ys, X, Xs).

hamilton(Vs, Es) :- hamilton_sub(Vs, Es, PPP).

hamilton_sub([V], PPP, [V]).
hamilton_sub(Vs, Es, [V1, V2 | Path])
  :- choose(Vs, V1, Vs0), choose(Es, [V1, V2], PPP),
     hamilton_sub(Vs0, Es, [V2 | Path]).

