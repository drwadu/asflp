from asflp import Flp


# lib = "../dist-newstyle/build/x86_64-linux/ghc-9.4.8/asflp-0.1.0.0/f/blafoo/build/blafoo/libblafoo.so"
# lib = "./libblafoo.so"
# print(lib)
lib = "../dist-newstyle/build/x86_64-linux/ghc-9.4.8/asflp-0.1.0.0/f/hsasflp/build/hsasflp/libhsasflp.so"

# flp = Flp("", lib)
# flp.add_rule("a", ["-b"])
# flp.add_rule("b", ["-a"])
# flp.add_rule("c", ["b", "-d"])
# flp.add_rule("d", ["b", "-c"])
## flp.add_integrity_constraint(["-c","-d"])
# print(flp.encoding)
# print()
# flp.infer({"a": (0.2, 0.8), "b": (0.6, 0.9)})
# flp.model()
#
# print('===')
flp = Flp("", lib)
# {a;b} :- not c, d
flp.add_choice_rule(["a", "b"], ["-c", "d"])
# :- not a, not b
flp.add_integrity_constraint(["-a", "-b"])
print(flp.encoding)
flp.infer({"a": (0.5, 0.8)})
flp.model()
