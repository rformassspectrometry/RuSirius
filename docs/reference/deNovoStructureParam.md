# de novo structure annotation

This function to set up the parameter for de novo structure annotation
using the *MSNovelist* tool.

*MSNovelist* generates molecular structures de novo from MS/MS data -
without relying on any database. This makes it particularly useful for
analyzing poorly represented analyte classes and novel compounds, where
traditional database searches may fall short. However, it is not
intended to replace database searches altogether, as structural
elucidation of small molecules from MS/MS data remains a challenging
task, and identifying a structure without database candidates is even
more difficult.

## Usage

``` r
deNovoStructureParam(numberOfCandidateToPredict = 10)
```

## Arguments

- numberOfCandidateToPredict:

  `numeric`, number of structure candidates to be predicted by
  MsNovelist. Max Value `128`. Actual number of returned candidate might
  be lower du to duplicates being created. Default is `10`

## Value

An object of class `deNovoStructureParam`.

## Note

For more information, see the Sirius
[documentation](https://v6.docs.sirius-ms.io/methods-background).

## References

reference

## Examples

``` r
# Example of setting up the parameters for de novo structure annotation
param <- deNovoStructureParam(numberOfCandidateToPredict = 10)
```
