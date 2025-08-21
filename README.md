# asflp
A prototypical fuzzy logic programming solver that performs inference on ([ground](https://www.cs.uni-potsdam.de/~torsten/Lehre/ASP/Folien/asp-handout.pdf#Outline3.11) & [tight](https://www.cs.uni-potsdam.de/~torsten/Lehre/ASP/Folien/asp-handout.pdf#Outline13.47)) input [answer set programs](https://www.cs.uni-potsdam.de/~torsten/Lehre/ASP/Folien/asp-handout.pdf#Outline3) using [logical neural networks](https://arxiv.org/pdf/2006.13155#subsection.4.3) (LNNs).

![demo gif made with VHS](https://vhs.charm.sh/vhs-1roR2jzOqWsqsE9vDeZkxt.gif)

In more detail, an answer set program is first being translated into its corresponding [Clark's completion](https://www.cs.uni-potsdam.de/~torsten/Lehre/ASP/Folien/asp-handout.pdf#Outline13), whose abstract syntax tree provides the LNN that will be used to perform inference. Inference is performed within the LNN by 
- first forwarding the truth bounds of atoms (leafs) using an upward pass; 
-  in order to tighten the truth bounds of atoms according to the assumption that the Clark's Completion of the input program is true, we then use a downward pass to propagate that the root node of the LNN has lower bound 1.0 and upper bound 1.0;
- finally, on the resulting LNN, we perform the [Upward–Downward algorithm](https://arxiv.org/pdf/2006.13155#subsection.4.3) until the truth bounds of all subformulae converge, which happens in a finite number of steps.

For now, this is a basic hobby project and by no means considered to be stable. For more details on certain intricacies, feel free to issue a question.

