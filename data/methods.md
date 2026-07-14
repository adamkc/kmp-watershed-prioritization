## How it works

You tell the tool what matters and how much. It scores every subwatershed on the metrics you picked, weights those scores, and ranks the results. Start from a prebuilt scenario, build your own list of metrics, or upload a CSV. Move the weight sliders and the map, the table, and the report all update as you go.

The scoring puts every metric on the same footing. Whatever the original units, each one is binned to a score from 1 to 5, so a percentage and a species count can sit in the same average without one drowning out the other. Your weights decide how hard each metric pulls, and the weighted average of those scores is the composite that drives the ranking.

Two things are worth keeping in mind. The scores are relative: a 5 means "high compared to the other subwatersheds in this run," not a fixed grade, so the same subwatershed can score differently once you change the area or the metric set. And the ranking needs a decent number of subwatersheds to mean anything. Narrow it down to a handful and the binning has too little to work with. The Sensitivity tab shows whether a ranking survives a nudge to the weights, and the Diagnostics tab flags data problems before they mislead you.

The rest of this page is the detail.

## More information

### Scoring and binning

Every metric becomes an integer from 1 to 5 before anything is combined, and how the cut points are chosen depends on the data. A continuous metric with plenty of distinct values gets Jenks natural breaks (below). A metric with only a few values, say a 0/2/5 presence code, is stretched linearly onto 1 to 5 instead, because clustering three numbers just produces unstable breaks. A metric that is mostly zeros gets a fallback that separates the zero subwatersheds from the rest. Whichever route a metric takes, its final bins are stretched to cover the full 1-to-5 range, so no metric ends up contributing on a shrunken scale just because its values happened to form fewer natural groups.

### Jenks natural breaks

Jenks is a clustering method for a single variable. It sets the class boundaries to make each group as tight as possible while keeping the groups far apart, what the method calls the goodness-of-variance fit. The tool asks for five classes and computes them with the `classInt` package, using the metric's values across whatever subwatersheds are currently in scope. Because the breaks come out of the data, they move when the set of subwatersheds changes. That is why choosing a sub-area re-bins everything (see Scope and data quality).

### Direction

Some metrics are better when high, others when low, so each one carries a tag. Positive means a higher raw value should raise priority; negative means a lower one should. For a negative metric the bin scores are flipped, so a 2 becomes a 4 and a 5 becomes a 1 (a score *b* becomes 6 - *b* on the five-class scale). After that step a 5 always means high priority, and every later calculation can treat all the metrics the same way.

### Composite score

A subwatershed's composite is the weighted average of its direction-adjusted bin scores, taken over the metrics you gave a non-zero weight. Zero-weight metrics drop out. If a subwatershed is missing one of the included metrics, that metric is left out of its average and the remaining weights are rescaled to fill the gap, so missing data neither helps nor hurts it. The composite stays on the same 1-to-5 scale as the scores that feed it.

### Ranking and ties

Sort by composite score, highest first. Subwatersheds with the same score share a rank, and the shared rank is the better (lower) of the tied positions.

### Monte Carlo sensitivity

A ranking is only as good as the weights behind it, and those weights are a judgment call. This analysis asks how much the call matters. You set an uncertainty fraction *p* (the "± %" slider) and a number of draws *N*. On each draw, every active weight is multiplied by its own random factor drawn from a uniform range between 1 - *p* and 1 + *p*, and the composites and ranks are recomputed under those perturbed weights. At *p* = 0.5 the factor runs from 0.5 to 1.5. Across the *N* draws the tool records each subwatershed's spread of ranks and reports the median, the interquartile range, and how often it landed in the top tier (top 10% unless you change it). Weights you set to zero stay at zero on every draw, so only the choices you actually made get shaken. A subwatershed that holds a narrow band of high ranks is robust to the weighting. One whose rank swings widely is riding on the exact weights, and its position deserves more caution.

### Scope and data quality

Three things are worth watching, all of them on the Diagnostics tab. Missing values are dropped from a metric's own average rather than filled in with a guess. Metrics that are mostly zeros get flagged, because their bins carry little information and can behave erratically. And since Jenks recomputes over the subwatersheds in scope, narrowing to a small sub-area re-bins the data among just those few. Below roughly a dozen units the five-class scheme starts to fall apart, which is why the full study area is the default and the most reliable way to read the results.
