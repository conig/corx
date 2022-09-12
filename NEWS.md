# corx 1.0.7.1

* Fixed issue where misspelling columns did not result in error
* Removed leading zeros for p-values in to_table
* P-values are now rounded consistently to three decimal places

# corx 1.0.7.0

* Removed magrittr pipe operator
* Added corx method for papaja::apa_table
* Now use cor.test for non-partial correlations
* Added ability to adjust p-values using stats::p.adjust
* Added new function "to_table" which provides additional options for corx tabulation (i.e., inclusion of p-values)
