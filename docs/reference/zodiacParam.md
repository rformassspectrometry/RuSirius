# Configuration fr re-ranking of Molecular Formula Annotation

This function configures the parameters for the re-ranking of the
previously computed molecular formula annotation in Sirius. This step is
quite computationally and memory demanding, it is advised to perform it
only if de novo structure annotation is used later on

ZODIAC uses the top X molecular formula candidates for each molecule
from SIRIUS to build a similarity network, and uses Bayesian statistics
to re-rank those candidates.

## Usage

``` r
zodiacParam(
  consideredCandidatesAt300Mz = 10,
  consideredCandidatesAt800Mz = 50,
  runInTwoSteps = TRUE,
  edgeFilterThreshold = TRUE,
  thresholdFilter = 0.95,
  minLocalCandidates = 1,
  minLocalConnections = 10,
  gibbsSamplerParameters = TRUE,
  iterations = 20000,
  burnInPeriod = 2000,
  numberOfMarkovChains = 10
)
```

## Arguments

- consideredCandidatesAt300Mz:

  An `integer(1)` specifying the maximum number of candidate molecular
  formulas considered by ZODIAC for compounds below 300 m/z. Default is
  `10`.

- consideredCandidatesAt800Mz:

  An `integer(1)` specifying the maximum number of candidate molecular
  formulas considered by ZODIAC for \\ compounds above 800 m/z. Default
  is `50`.

- runInTwoSteps:

  `logical` specifying whether ZODIAC uses a 2-step approach. First
  running 'good quality compounds' only, and afterwards including the
  remaining compounds. Default is `TRUE`.

- edgeFilterThreshold:

  `logical` value specifying whether ZODIAC uses an edge filter
  threshold. Default is `TRUE`.

- thresholdFilter:

  `numeric(1)` value specifying the threshold filter. Default is `0.95`.

- minLocalCandidates:

  `numeric(1)` value specifying the minimum number of local candidates.
  Default is `1`.

- minLocalConnections:

  `numeric(1)` value specifying the minimum number of local connections.
  Default is `10`.

- gibbsSamplerParameters:

  `logical` value specifying whether ZODIAC uses Gibbs sampler
  parameters. Default is `TRUE`.

- iterations:

  `numeric(1)` specifying the number of iterations. Default is `20000`.

- burnInPeriod:

  `numeric(1)` specifying the burn-in period. Default is `2000`.

- numberOfMarkovChains:

  `numeric(1)` specifying the number of Markov chains. Default is `10`.

## Value

An object of class `zodiacParam`.

## Note

For more information, see the Sirius
[documentation](https://v6.docs.sirius-ms.io/methods-background).

## References

reference

## Examples

``` r
# Example of setting up the parameter for Zodiac re-ranking
param <- zodiacParam(consideredCandidatesAt300Mz = 10,
                     consideredCandidatesAt800Mz = 50,
                     runInTwoSteps = TRUE,
                     edgeFilterThreshold = TRUE,
                     thresholdFilter = 0.95,
                     minLocalCandidates = 1,
                     minLocalConnections = 10,
                     gibbsSamplerParameters = TRUE,
                     iterations = 20000,
                     burnInPeriod = 2000,
                     numberOfMarkovChains = 10)
```
