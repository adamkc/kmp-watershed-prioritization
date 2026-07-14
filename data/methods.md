## Overview and workflow

This tool scores and ranks subwatersheds from metrics and weights you choose. Begin with a prebuilt scenario, pick your own metrics, or upload a CSV. As you move the weight sliders, the map, table, and report update live.

To compare metrics measured in different units, the tool rescales each one onto a common 1-to-5 score. Your weights set the relative importance of each metric, and the weighted average of the scores is the composite score used for ranking. Subwatersheds are sorted by composite score, highest first. When two or more tie, they share the best of the tied positions.

## Validating your results

Scores are relative: a 5 means "high compared to the rest of the map," not a fixed passing grade, so changing the study area or the metric set will change the scores. Two tabs help you check the analysis before you rely on it.

### Diagnostics tab: data quality and scope

This tab surfaces data problems before they skew the results. It flags metrics that are mostly zeros, since their bins carry little discriminating information, and it lists any columns with missing values (those values are dropped from a metric's own average rather than filled with a guess, so a gap neither helps nor hurts the score). It also reports how many subwatersheds matched the boundaries, which is worth a glance: below roughly a dozen units, Jenks has too little to work with and the five-class scoring becomes unreliable.

### Sensitivity tab: Monte Carlo analysis

Because the weights are a judgment call, this tab tests how stable the rankings are. You set an uncertainty fraction (± %) and a number of iterations. On each iteration the tool multiplies every active weight by its own small random factor, recomputes the ranks, and after all the iterations reports each subwatershed's median rank and how much its rank moved. Weights you set to zero stay at zero, so only the weights you actually chose get perturbed. A tight cluster of ranks means the ranking is robust; a wide spread means it hinges on the exact weights you picked.

## Methodology

For readers who want the underlying math, here is how the tool standardizes and combines the data.

### 1. Scoring, binning, and Jenks breaks

Every metric is converted to an integer from 1 to 5 before anything is combined, and how the cut points are chosen depends on the data.

Continuous metrics use the R `classInt` package to compute Jenks natural breaks, which set class boundaries by minimizing the variance within groups and maximizing the variance between them. Because the breaks come from the data, they shift whenever the geographic scope changes.

Metrics with only a few distinct values (for example a 0/2/5 presence code) are rescaled linearly instead, to avoid unstable clustering on a handful of numbers.

Metrics that are mostly zeros use a fallback that separates the zero subwatersheds from the rest.

Whichever route a metric takes, its final bins are stretched across the full 1-to-5 range, so no metric contributes on a shrunken scale just because its values happened to form fewer natural groups.

### 2. Direction

Each metric is tagged by whether high or low values mean higher priority. For a positive metric, a higher raw value raises priority. For a negative metric, where lower is better, the bin scores are inverted, so a 2 becomes a 4 and a 5 becomes a 1. After that step a score of 5 always means the highest priority, and every later calculation can treat the metrics the same way.

### 3. Composite score

A subwatershed's composite is the weighted average of its direction-adjusted bin scores. Metrics with a weight of zero drop out. If a subwatershed is missing a metric, that metric is left out of its average and the remaining weights are rescaled to fill the gap, so missing data neither helps nor hurts the score. The composite stays on the same 1-to-5 scale as the scores that feed it.
