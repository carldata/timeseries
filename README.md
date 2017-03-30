[![Build status](https://travis-ci.org/carldata/timeseries.svg?branch=master)](https://travis-ci.org/carldata/timeseries)
# Welcome to the Time Series library

Library for processing Time Series which are functions from Time to real value.


# Features

  * Basic functionality
    * [x] Slicing series
    * [x] Map, fold and filter
    * [x] Integration
    * [x] Differentiation
    * [ ] Rolling window
    * [ ] Resampling and groupBy
  * Calculate statistics
    * [x] min, max
    * [x] mean, variance and standard deviation
  * IO
    * [x] Read data from CSV string
  * Advanced functionality
    * [ ] Finding sessions (periods of activity)
    * [ ] Finding patterns
  * Anomaly detection
    * [ ] Outliers


## Build

```bash
sbt test
```

This library can also be compiled into the JavaScript

```bash
sbt fastOptJS
```

## Examples

Folder examples contains some examples how to use this library 


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
