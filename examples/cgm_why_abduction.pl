% transmitter fell off
fell_off[1.0;1.0]
% worn 2-4 days out of 10
duration[0.2;0.4]
% was placed on arm
used(arm)[1.0;1.0]

misuse :- -used(arm)
negligence :- sports, used(stomach)
negligence :- sports, duration

fell_off :- sports, misuse
fell_off :- sports, -misuse
fell_off :- negligence, misuse
fell_off :- negligence, -misuse

% integrity constraint
ic[0.0;0.0]
ic :- -ic, used(arm), used(stomach)
