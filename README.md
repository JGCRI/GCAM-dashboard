# EPPA Dashboard: An Interactive Dashboard for Exploring EPPA Scenario Data

The EPPA Dashboard is a scenario explorer for EPPA.  Its purpose is to
provide a way to give users a quick view of the data in a collection
of scenarios.  You can get a listing of the scenarios in a data set
and the queries available for each scenario, or available jointly for
a collection of scenarios.  You can plot the queries for a single
scenario, or you can plot the difference in output values between two scenarios.

## Installation

The easiest way to run the EPPA Dashboard is using the
[R Studio](https://www.rstudio.com/) IDE.  Download and install R
Studio.  Start a new session in R Studio, and install the `devtools`
package, if you don't have it already.  You can do that by entering:
```R
install.packages('devtools')
```
Next, install the GCAMdashboard
package:
```R
devtools::install_github('cypressf/GCAMdashboard')
```
This should install all of the required packages that the GCAM
Dashboard needs to operate for you automatically.

## Usage

To run the EPPA Dashboard, from the R command console enter
`GCAMdashboard::run()`.

### Uploading data

The EPPA Dashboard gets its data from the excel files created by the the EPPA model.
