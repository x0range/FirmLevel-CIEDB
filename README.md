# FirmLevel CIEDB

Software for parsing, computations, visualization, and fitting using data of the Chinese Industrial Enterprise DB

# Dependencies

The code Python 3 for parsing of the encoding and initial combination of the data frame and R for all further computation and analysis.

It relies on code that is part of the [Amadeus-Datawork code base](https://github.com/Orbis-Amadeus-Oxford/Amadeus-Datawork)

# Usage

```
$ python3 01_parse_encoding.py
$ python3 02_combine_dataframe.py
$ Rscript 03_R_computations.R
$ Rscript 04_Script_add_FirmType2.R
$ Rscript 05_Diagnostic_aggregated.R
$ Rscript 05_Diagnostic_by_year.R
$ Rscript 05_Fitting_Levy_AS_and_AEP.R
```
