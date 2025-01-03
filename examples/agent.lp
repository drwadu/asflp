cell(1..5,1..5).

1 { agent(X,Y,0) : cell(X,Y) } 1.
1 { action(X) } 1 :- X=1..4.

agent(X,Y+1,1) :- action(1), agent(X,Y,0), Y<5.
agent(X,Y-1,1) :- action(2), agent(X,Y,0), Y<1.
agent(X+1,Y,1) :- action(3), agent(X,Y,0), X<5.
agent(X-1,Y,1) :- action(4), agent(X,Y,0), X>1.




