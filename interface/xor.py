import torch
from torch.nn.functional import one_hot

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


flp = Flp("")
flp.add_rule("y", ["f1", "-f2"])
flp.add_rule("y", ["-f1", "f2"])
# flp.add_rule("ny", ['f1','f2'])
# flp.add_rule("ny", ['-f1','-f2'])
# flp.add_rule("ny", ['-y'])
# flp.add_rule("y", ['-ny'])

# x0 = torch.zeros((4, 100))
# x_train = torch.tensor([
#    [0, 0],
#    [0, 1],
#    [1, 0],
#    [1, 1],
# ], dtype=torch.float)
# x_train = torch.cat([x_train, x0], dim=1)
# y_train = torch.tensor([0, 1, 1, 0], dtype=torch.long)
# y_train_1h = one_hot(y_train).to(torch.float)
#
# print(x_train)
# print(y_train_1h)
#
torch.manual_seed(42)
x_train = torch.FloatTensor(200, 2).uniform_(0.0, 1.0)
layers = [
    torch.nn.Linear(2, 10),
    torch.nn.ReLU(),
    torch.nn.Linear(10, 4),
    torch.nn.LeakyReLU(),
    torch.nn.Linear(4, 1),
    torch.nn.Sigmoid(),
    # torch.nn.Linear(4, 1),
]
model = torch.nn.Sequential(*layers)

lr = 1e-3
optimizer = torch.optim.AdamW(model.parameters(), lr=lr)
# optimizer = torch.optim.SGD(model.parameters(), lr=lr)
# loss_form = torch.nn.BCEWithLogitsLoss()
loss_form = torch.nn.MSELoss()
# loss_form = torch.nn.BCELoss()
# loss_form = torch.nn.L1Loss()
# loss_form = torch.nn.CrossEntropyLoss()
model.train()
losses = []
for epoch in range(5_001):
    optimizer.zero_grad()
    y_pred = model(x_train).squeeze(-1)
    lnn_pred = []
    for i, xs in enumerate(x_train):
        f1, f2 = float(float(xs[0]) > 0.5), float(float(xs[1]) > 0.5)
        lnn_output = flp.infer({"f1": (f1, f1), "f2": (f2, f2)})
        # print(lnn_output)
        lnn_pred.append(lnn_output["y"])
    y = torch.FloatTensor(list(map(lambda t: t[0], lnn_pred))).squeeze(-1)
    loss = loss_form(y_pred, y) + 0.001
    print(epoch, loss.item())
    losses += [loss.item()]
    loss.backward()
    optimizer.step()


x_test = torch.cat(
    (torch.tensor(
        [
            [0, 0],
            [0, 1],
            [1, 0],
            [1, 1],
            [0.2, 0.4],
            [0.7, 1],
            [0.8, 0],
            [0, 0.9],
        ],
        dtype=torch.float,
    ),
    torch.FloatTensor(10, 2).uniform_(0.1, 0.9)), 0
)

model.eval()
for i, xs in enumerate(x_test):
    f1, f2 = float(float(xs[0]) > 0.5), float(float(xs[1]) > 0.5)
    lnn_output = flp.infer({"f1": (f1, f1), "f2": (f2, f2)})
    ym = model(xs)
    yl = lnn_output["y"]
    print(xs, ym.item(), yl)

lib.hs_exit()
