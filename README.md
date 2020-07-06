# QuantumOption

This program is for use with the paper A Quantum Walk Model of Financial Options, by David Orrell.
See: https://papers.ssrn.com/sol3/Papers.cfm?abstract_id=3512481

The program can be used to compare call option prices calculated using the classical binomial model and the quantum walk model.

The Stock tab shows the probability density for the stock price. The classical model used is equivalent to the Jarrow-Rudd (JR) equal probability model. 
The unscaled random and quantum walks can also be plotted. Decoherence is set by adjusting a parameter as discussed in the paper.

The Options tab shows the option prices for a range of strike prices. Results can be compared with those from the Black-Scholes (BS) model as calculated 
using the derivmkts R package, and are shown listed in the Table tab.

The implied volatility tab shows the implied volatility of the quantum model, defined as the volatility which gives the same price in the classical model.

Note that the stock price in the quantum model is not the actual price, but rather the future possible price, as imagined by market participants. Decoherence 
reflects the partial collapse of this model, due for example to empirical observations of option prices. Adding decoherence therefore brings the quantum model 
closer to the classical model.
