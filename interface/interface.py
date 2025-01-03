from ctypes import cdll, c_char_p
from dataclasses import dataclass


lib = cdll.LoadLibrary(
    "../dist-newstyle/build/x86_64-linux/ghc-9.4.8/asflp-0.1.0.0/f/blafoo/build/blafoo/libblafoo.so"
)

lib.hs_init()


@dataclass
class Flp:
    encoding: str

    def add_rule(self, head: str, body: list[str]) -> None:
        self.encoding = self.encoding + f"\n{head} :- {','.join(body)}"

    def infer(self, input: dict[str, float]) -> dict[str, float]:
        xs = [f"{k}[{l};{u}]" for k, (l, u) in input.items()]
        flp = "\n".join(xs) + "\n" + self.encoding
        lib.forward.restype = c_char_p
        x = lib.forward(flp.encode("utf-8"))
        d = {
            y.split("[")[0]: (
                float(y.split("[")[1].split(";")[0]),
                float(y.split("[")[1].split(";")[1][:-1]),
            )
            for y in x.decode().split(" ")
            if len(y) > 1
        }
        return d


lib.hs_exit()

if __name__ == "__main__":
    # flp = Flp("a :- -b\nb :- -a\nc :- b,-d\nd :- b,-c")
    flp = Flp("a :- -b\nb :- -a")
    print(flp.infer({"a": (0.2, 1.0), "b": (0.6, 1.0)}))
    flp.add_rule("c", ["b", "-d"])
    print(flp.infer({"a": (0.2, 1.0), "b": (0.6, 1.0)}))
    flp.add_rule("d", ["b", "-c"])
    print(flp.infer({"a": (0.2, 1.0), "b": (0.6, 1.0)}))
