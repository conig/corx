# corx (development version)

- Upper-triangle tables now retain the last variable, and two-variable tables keep their dimensions. P-value rows align with the displayed coefficients, including when the correlation method is set explicitly or supplied through a variable. Triangular output requires matching source variables on both axes.

- Descriptive statistics no longer overwrite a correlation column with the same name. Plotting uses the stored caption and triangle, and MDS rejects asymmetric matrices even when the axis labels match.

- The papaja table method now accepts the generic's named `x` argument.

- Updated the help for exported functions and methods, including column selection, return values, and examples.

- Ordinary Pearson correlations now compute the two-sided p-value directly, and tables format each distinct rounded value only once.

- Column selectors now inspect the actual data, so `where(is.numeric)` works on mixed data. Renaming a variable labels the output without changing which correlations are computed or adjusted. Explicit `x` and `y` selections that match nothing now error instead of silently using the defaults.

- Pairs with no shared observations now return `NA` for the correlation and p-value and `0` for the sample size, without aborting the rest of the matrix (#13). Symmetric ordinary correlation matrices compute each pair only once.

- Overlapping row and column selections now adjust p-values once per distinct variable pair, excluding self-correlations and giving reversed pairs the same adjusted p-value. Ordinary correlations also avoid recomputing those reversed pairs.

# corx 1.0.7.3

- Corx now can identify haven labelled variables as numeric

# corx 1.0.7.2

- Fixed issue with incorrect class declaration

# corx 1.0.7.1

- Fixed issue where misspelling columns did not result in error
- Removed leading zeros for p-values in to_table
- P-values are now rounded consistently to three decimal places

# corx 1.0.7.0

- Removed magrittr pipe operator
- Added corx method for papaja::apa_table
- Now use cor.test for non-partial correlations
- Added ability to adjust p-values using stats::p.adjust
- Added new function "to_table" which provides additional options for corx tabulation (i.e., inclusion of p-values)
