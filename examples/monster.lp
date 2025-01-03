1 { agent(X,Y,0) } 1 :- X=1..5, Y=1..5. 
1 { monster(X,Y,0) } 1 :- X=1..5, Y=1..5. 
1 { agent(X,Y,1) } 1 :- X=1..5, Y=1..5. 
1 { monster(X,Y,1) } 1 :- X=1..5, Y=1..5. 

action(up;down;left;right).
1 { action(A,0) : action(A) } 1.
1 { monster_action(A,0) : action(A) } 1.

%agent(X,Y+1,T) :- Y<5, action(up,T-1), agent(X,Y,T-1).
%agent(X,Y-1,T) :- Y>0, action(down,T-1), agent(X,Y,T-1).
%agent(X+1,Y,T) :- X<5, action(right,T-1), agent(X,Y,T-1).
%agent(X-1,Y,T) :- X>1, action(left,T-1), agent(X,Y,T-1).
%monster(X,Y+1,T) :- Y<5, monster_action(up,T-1), monster(X,Y,T-1).
%monster(X,Y-1,T) :- Y>0, monster_action(down,T-1), monster(X,Y,T-1).
%monster(X+1,Y,T) :- X<5, monster_action(right,T-1), monster(X,Y,T-1).
%monster(X-1,Y,T) :- X>1, monster_action(left,T-1), monster(X,Y,T-1).

agent(X,Y,T-1) :- Y<1, agent(X,Y+1,T), action(up,T-1).
agent(X,Y,T-1) :- Y>4, agent(X,Y-1,T), action(down,T-1).
agent(X,Y,T-1) :- X<1, agent(X+1,Y,T), action(right,T-1).
agent(X,Y,T-1) :- X>4, agent(X-1,Y,T), action(left,T-1).
monster(X,Y,T-1) :- Y<1, monster(X,Y+1,T), monster_action(up,T-1).
monster(X,Y,T-1) :- Y>4, monster(X,Y-1,T), monster_action(down,T-1).
monster(X,Y,T-1) :- X>1, monster(X+1,Y,T), monster_action(right,T-1).
monster(X,Y,T-1) :- X>4, monster(X-1,Y,T), monster_action(left,T-1).

hit(T) :- agent(X,Y,T), monster(X,Y,T).

%{ hit(T) } 1 :- T=0..1.
%:- agent(X,Y,T), monster(X,Y,T), not hit(T).

:- agent(X,Y,T), agent(X',Y',T), X!=X', Y!=Y'.
:- action(A,T), action(A',T), A!=A'.
:- monster_action(A,T), monster_action(A',T), A!=A'.
