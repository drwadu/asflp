from ctypes import cdll, c_char_p
from dataclasses import dataclass
from typing import Any, Optional


@dataclass
class Flp:
    encoding: str
    lib: str
    has_ic: bool = False
    knowledge: Optional[dict[str, tuple[float, float]]] = None
    atoms: Optional[dict[str, tuple[float, float]]] = None

    def add_rule(self, head: str, body: list[str]) -> None:
        self.encoding = self.encoding + f"\n{head} :- {','.join(body)}"

    def add_integrity_constraint(self, forbidden_and: list[str]) -> None:
        self.encoding = self.encoding + f"\nic :- {','.join(forbidden_and)},-ic"
        self.has_ic = True

    def add_choice_rule(self, head: list[str], body: list[str]) -> None:
        b = ",".join(body)
        x = f"cr_aux_{hash(b)}"
        self.encoding = self.encoding + f"\n{x} :- {b}"
        for i, a in enumerate(head):
            xi = f"{x}_{i}"
            self.encoding = self.encoding + f"\n{a} :- {','.join([x,f"-{xi}"])}"
            self.encoding = self.encoding + f"\n{xi} :- -{a}"

    def infer(self, input: dict[str, float]) -> dict[str, float]:
        lib = cdll.LoadLibrary(self.lib)
        lib.hs_init(0, 0)
        xs = [f"{k}[{l};{u}]" for k, (l, u) in input.items()]
        if self.has_ic:
            xs += ["ic[0.0;0.0]"]
        flp = "\n".join(xs) + "\n" + self.encoding
        lib.forward.restype = c_char_p
        x = lib.forward(flp.encode("utf-8"))
        d = {
            y.split("[")[0]: (
                float(y.split("[")[1].split(";")[0]),
                float(y.split("[")[1].split(";")[1][:-1]),
            )
            for y in x.decode().split("~")
            if len(y) > 1
        }
        self.knowledge = d
        lib.hs_exit()
        return d

    def true(self) -> None:
        for f, (l, u) in self.knowledge.items():
            if (
                not "root" in f
                and not "ic" in f
                and not "OR" in f
                and not "proof" in f
                and not "cr_aux" in f
            ):
                if l + u == 2.0:
                    print(f, l, u)

    def false(self) -> None:
        for f, (l, u) in self.knowledge.items():
            if (
                not "root" in f
                and not "ic" in f
                and not "OR" in f
                and not "proof" in f
                and not "cr_aux" in f
            ):
                if l + u == 0.0:
                    print(f, l, u)

    def fuzzy(self) -> None:
        for f, (l, u) in self.knowledge.items():
            if (
                not "root" in f
                and not "ic" in f
                and not "OR" in f
                and not "proof" in f
                and not "cr_aux" in f
            ):
                if l == 0.0 and u == 1.0:
                    print(f, l, u)

    def model(self, quiet: bool = True) -> None:
        if not self.atoms:
            self.atoms = dict()
        if not quiet:
            for f, (l, u) in self.knowledge.items():
                if (
                    not "root" in f
                    and not "ic" in f
                    and not "OR" in f
                    and not "AND" in f
                    and not "proof" in f
                    and not f.startswith("-")
                    and not "cr_aux" in f
                ):
                    self.atoms[f] = (l,u)
                    if l == u:
                        print(round(l, 2), f)
                    else:
                        print(round(l, 2), round(u, 2), f)
        else:
            for f, (l, u) in self.knowledge.items():
                if (
                    not "root" in f
                    and not "ic" in f
                    and not "OR" in f
                    and not "AND" in f
                    and not "proof" in f
                    and not f.startswith("-")
                    and not "cr_aux" in f
                ):
                    self.atoms[f] = (l,u)

        print(self.atoms)

if __name__ == "__main__":
    lib = "../dist-newstyle/build/x86_64-linux/ghc-9.4.8/asflp-0.1.0.0/f/hsasflp/build/hsasflp/libhsasflp.so"
    #flp = Flp("a :- -b\nb :- -a", lib=lib)
    #print(flp.infer({"a": (0.2, 1.0), "b": (0.6, 1.0)}))
    #flp.add_rule("c", ["b", "-d"])
    ##print(flp.infer({"a": (0.2, 1.0), "b": (0.6, 1.0)}))
    #flp.add_rule("d", ["b", "-c"])
    #print(flp.infer({"a": (0.2, 1.0), "b": (0.6, 1.0)}))

    src = open("../examples/cgm_why_abduction.flp").read()
    flp = Flp(src, lib=lib)
    #for k, v in flp.infer({"duration": (0.1, 0.1), "used_stomach": (1.0, 1.0),"fell_off": (1.0, 1.0)}).items():
    for k, v in flp.infer({"duration": (0.9, 0.9), "sports": (1.0, 1.0),"fell_off": (1.0, 1.0)}).items():
        print(v, k)

    #src = open("../examples/mnist_sum.flp").read()
    #flp = Flp(src, lib=lib)
    #for k, v in flp.infer({"digitL": (0.2, 1.0)}).items():
    #    print(v, k)
