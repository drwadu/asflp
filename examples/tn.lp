% { a;b;c } :- d
x :- d
a :- x, -x1 
x1 :- -a
b :- x, -x2
x2 :- -b
c :- x, -x3 
x3 :- -c
%d[1.0;1.0]
a[1.0;1.0]
%b[1.0;1.0]
ic[0.0;0.0]
ic :- a, b, -ic

%ic[0.0;0.0]
%a :- -b
%b :- -a
%c :- b, -d
%d :- b, -c
%ic :- b, -ic
