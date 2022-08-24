# corx 1.0.7.0

* Removed magrittr pipe operator
* Added corx method for papaja::apa_table
* Now use cor.test for non-partial correlations
* Added ability to adjust p-values using stats::p.adjust
* Added new function "to_table" which provides additional options for corx tabulation (e.g., inclusion of p-values)
* Diagonals for partial adjusted matrices are now NA instead of 1
