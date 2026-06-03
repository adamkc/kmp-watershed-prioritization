## How it works

You tell the tool what matters and how much, and it ranks the subwatersheds for you. Pick one of the prebuilt scenarios, assemble your own list of metrics, or upload a CSV. Then fine-tune how much each metric counts with the weight sliders. The result updates as you go and shows up three ways: a choropleth map, a sortable table you can download, and a written report.

The idea underneath is simple. Every metric, whatever its units, is scored from 1 (low) to 5 (high) so they can be compared on equal terms. Your weights combine those scores into a single composite score for each subwatershed, and the subwatersheds are ranked from highest to lowest. A subwatershed that scores well on the things you've prioritized rises to the top.

Two habits will keep you out of trouble. First, the scores are relative to the other subwatersheds in the same run, not absolute grades; read them as a comparison. Second, the ranking works best across the full study area, because drilling into a small sub-area leaves too few subwatersheds for the scoring to settle. When in doubt, check the Diagnostics tab, and use the Sensitivity tab to see whether your ranking survives a nudge to the weights.

If you want to know exactly how the scoring, weighting, and sensitivity analysis work, read on.

## More information

### Scoring and binning

Each metric is converted to an integer score from 1 to 5 before anything is combined. How the cut points are chosen depends on the metric. Continuous metrics with enough distinct values are binned by Jenks natural breaks (below). Metrics with only a few unique values, such as a 0 / 2 / 5 presence code, are rescaled linearly onto 1-5 instead of clustered, since clustering a handful of values is unstable. Metrics dominated by zeros get a fallback that separates the zero subwatersheds from the non-zero ones. In every case the resulting bins are stretched to span the full 1-to-5 range, so a metric's scores are comparable to every other metric's regardless of how many natural groups its data actually formed.

### Jenks natural breaks

Jenks natural breaks is a one-dimensional clustering method that places class boundaries to minimize the variance within classes while maximizing the variance between them (the "goodness of variance fit" criterion). In effect it searches for the cut points that make each group as internally similar as possible and the groups as distinct from each other as possible. The tool uses five classes by default, computed with the `classInt` package on the metric's values across whichever subwatersheds are currently in scope. Because the breaks are data-driven, they move when the set of subwatersheds changes, which is why selecting a sub-area re-bins everything (see Scope and data quality).

### Direction

Each metric is tagged positive or negative. Positive means a higher raw value should raise priority; negative means a lower raw value should. For negative metrics the bin scores are inverted (a score *b* becomes 6 - *b* under a five-class scheme), so that after this step a 5 always denotes high priority no matter the metric's native direction. Every downstream step can then treat all metrics identically.

### Composite score

A subwatershed's composite is the weighted arithmetic mean of its direction-adjusted bin scores, taken over the metrics with a non-zero weight. Metrics weighted at zero are dropped. If a subwatershed has no value for an included metric, that metric is omitted from its mean and the weights are renormalized over the metrics it does have, so missing data neither rewards nor penalizes it. The composite stays on the same 1-to-5 scale as its inputs.

### Ranking and ties

Subwatersheds are sorted by composite score in descending order. Tied scores take the same rank, assigned the lower (better) of the tied positions.

### Monte Carlo sensitivity

The sensitivity analysis tests how much the ranking depends on the exact weights. You set an uncertainty fraction *p* (the "± %" slider) and a number of draws *N*. On each draw, every active weight is multiplied by an independent factor drawn uniformly from [1 - *p*, 1 + *p*], and the composite scores and ranks are recomputed under those perturbed weights. Across all *N* draws the tool records each subwatershed's rank distribution and reports its median rank, an interquantile band, and the probability it lands in the top tier (top 10% by default). Weights set to zero stay at zero on every draw, so only the weights you actually chose are perturbed. At *p* = 0.5 the factor is Uniform(0.5, 1.5). A tight, high rank distribution means a subwatershed is robust to weighting choices; a wide one means its rank hinges on specific weights.

### Scope and data quality

Three issues, all surfaced in the Diagnostics tab. Missing values are excluded per metric rather than imputed. Zero-inflated metrics (mostly zeros) are flagged, because their bins carry little discriminating information and can behave erratically. And because Jenks breaks are recomputed over the in-scope subwatersheds, restricting the analysis to a small sub-area re-bins the data among just those few; below roughly a dozen units the five-class scheme becomes unstable, so the full study area is the default and most reliable scope.
