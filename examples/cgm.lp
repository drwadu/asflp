%#define sports.
%#define negligent.
%#define duration.

used_stomach :- not used_arm.
used_arm :- not used_stomach.

% placing on stomach was considered misuse back in the day
misuse :- not used_stomach.

% using arm and doing sports risks losing transmitter
negligent :- sports, used_arm.

% long duration and doing sports risks losing transmitter
negligent :- sports, duration.

% known cases of fallen off transmitters
% model as domain/expert knowledge
fell_off :- sports, misuse.
fell_off :- sports, not misuse.
fell_off :- negligent, misuse.
fell_off :- negligent, not misuse.
