# n-markov

This is kind of a generalisation of markov chains. In a markov chain, given a state, there is a certain probability of transitioning to another state. However, that means that a markov chain cannot (easily) capture context in its decisions, which, depending on the use case, can be useful.

In this program, the _n_ in the title stands for the amount of previous states a transition probability can look at. For example, if _n_ = 4, the probability of transitioning to another state can depend on the previous 4 states of the system.

This look-behind property makes contextual prediction tasks possible. It's likely to not perform as well as a recurrent neural network, but is a lot simpler to understand, both in terms of the code, but also the "brain" and decision-making of the system.
