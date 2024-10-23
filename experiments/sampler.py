import random
from typing import List, Dict, Set


def tight_body(
    src: List[str], max_body_size: int, atoms: Dict[str, List[Set[str]]], atom: str
) -> List[str]:
    return frozenset(
        map(
            lambda l: l if random.randint(0, 1) else "-" + l,
            set(random.choices(xs, k=max_body_size,
            ))
        if (xs :=list(filter(lambda x: all([atom not in y for y in atoms[x]]), src))) else [],
            # random.choices(src, k=max_body_size),
        )
    )


def random_lp(
    atoms: List[str], max_body_size: int, max_rules_per_atom: int, tight: bool = True
) -> str:
    bodies = {a: [] for a in atoms}

    for i, a in enumerate(atoms):
        src = atoms[:i] + atoms[i + 1 :]
        n_rules = random.randint(1, max_rules_per_atom)
        if tight_body:
            bodies[a] = set([tight_body(src, max_body_size, bodies, a) for _ in range(n_rules)])
        else:
            return 

    return bodies

def random_values(atoms: List[str]) -> str:
    n = random.randint(0,len(atoms))
    for a in set(random.choices(atoms,k=n)):
        l = random.randint(0,100) 
        u = random.randint(l,100)
        print(f"{a}[{l/100};{u/100}]")


def render(bodies: Dict[str, List[Set[str]]]) -> None:
    for a, bs in bodies.items():
        for b in bs:
            if b:
                print(a, ":-", ",".join(list(b)))
        


if __name__ == "__main__":
    atoms = ["p", "q", "r"]
    random_values(atoms)
    render(random_lp(atoms, 2, 2))
