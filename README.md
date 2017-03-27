[![Build Status](https://travis-ci.org/klangner/timeseries.svg?branch=master)](https://travis-ci.org/klangner/timeseries)
[![Hackage](https://img.shields.io/hackage/v/timeseries.svg)](https://hackage.haskell.org/package/timeseries)

# Welcome to Time Series library

Library for processing Time Series which are functions from Time to real value.


# Features

  * Basic functionality
    * [x] Slicing series
    * [x] Mapping over Series
    * [x] Folding Series
    * [ ] Integration
    * [x] Differentiation
    * [ ] Rolling window
    * [ ] Resampling and groupBy
  * Input/Output
    * [ ] Reading and writing from CSV file
  * Calculate statistics
    * [x] min, max
    * [x] mean, variance and standard deviation
  * Advanced functionality
    * [ ] Finding sessions (periods of activity)
  * Anomaly detection
    * [ ] Outliers


## Build

```bash
sbt test
```


# Join in!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/carldata/timeseries/issues).



# Redistributing

timeseries source code is distributed under the Apache-2.0 license.

**Contributions**

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
licensed as above, without any additional terms or conditions.
