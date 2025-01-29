% transmitter fell off
fell_off[1.0;1.0]
% has been worn 1 day (10% of 10 days)
duration[0.1;0.1]
% was placed on arm
placed_on(arm)[1.0;1.0]

misuse :- -placed_on(arm)
negligence :- sports, placed_on(stomach)
negligence :- sports, duration

fell_off :- sports, misuse
fell_off :- sports, -misuse
fell_off :- negligence, misuse
fell_off :- negligence, -misuse

% integrity constraint
ic[0.0;0.0]
ic :- -ic, placed_on(arm), placed_on(stomach)
