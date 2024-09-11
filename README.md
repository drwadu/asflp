## quickstart
```bash
sh build.sh 
# ...
./bin/asflp /examples/tiny.flp
[U] (0.4,0.7) a
[U] (0.3,0.6) b
[U] (0.0,0.6) c
[U] (0.0,0.6) d
```

### caveats
This prototypical implementation is quite basic and rather incomplete.

at the moment, pls:
- in n-ary predicates use `|` as the delimiter
- don't use `:-[l;u]` yet; `atom[l;u]` is fine tho
## framework
A program consists of rules of the form `atom_1 :-[l;u] atom_2, ..., atom_n`. Each rule is assigned a lower bound `l` and an upper bound `u`. 

A program contains one rule `atom :-[l;u]` for every atom
contained in the head or body of another rule. Bounds `l` and 
`u` are the input signals for the initial upward pass in the underlying logical neural network (LNN). 

An atom with no user specified input bounds is considered 
to be maximally uncertain. Accordingly, by default the bounds assigned to such atoms are `[0.0;1.0]`. 

```prolog
% syntactic sugar for rule `prediction(8):-[1.0;1.0]`
prediction(8)[1.0;1.0]  

% no provided bounds next to `:-` by default evaluate to `[0.0;1.0]`
prediction(9) :- digitL(6), digitR(3)

% we do not believe that 8=6+3
prediction(8) :-[0.0;0.0] digitL(6), digitR(3)
```
## solving
We construct the Cark's completion of the program and use it's AST as the program's underlying LNN. Note that the root neuron accordingly corresponds to the completion formula. The initial upward- and downwardpass are special.

First, in the initial upward pass, we input the bounds of atoms. 

Next, in the initial downward pass, we pass `[1.0;1.0]` as the root neuron's bounds through the LNN, to tighten the bounds of underlying neurons accordingly. This is to say that the completion formula shall be true. 
EXPERIMENTAL: What is more: instead of considering bounds after the initial upward pass, for the conjunction in the body of rules whose body is not empty, in the initial downward pass, we pass the specified bounds of these rules. (RATHER: in upward pass, different bounds per respective head/proof.)

Finally, we infer the final bounds, by repeatedly passing tightened bounds upwards and then downwards until bounds converge.

## build & usage
To build the tool run `sh build.sh`. Afterwards, you'll hopefully find the executable `asflp` in the directory `bin`. If not, it could be that you have to install [cabal](https://www.haskell.org/cabal/) first. 

Example programs can be found [here](https://github.com/drwadu/sflp/tree/main/prototype/examples). 

Always prepend either of the following flags before specifying the file path: 
* `-c` ... prints the underlying LNN
* `-s` ... prints the bounds of atoms after solving

