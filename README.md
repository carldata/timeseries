# Welcome to the Time Series library

[![Build status](https://travis-ci.org/carldata/timeseries.svg?branch=master)](https://travis-ci.org/carldata/timeseries)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.github.carldata/timeseries_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.carldata/timeseries_2.12)

Library for processing Time Series.


## Quick start
 
 Add the following dependency to the build.sbt
 ```scala
 libraryDependencies += "io.github.carldata" %% "timeseries" % "0.6.2"
 ```

Running benchmarks
```bash
sbt -mem 4000 run
```

# Features

  * Basic functionality
    * [x] Slicing series
    * [x] Map, fold and filter
    * [x] Integration
    * [x] Differentiation
    * [x] groupBy
    * [x] Rolling window
    * [x] Resampling 
    * [x] join and merge
  * Calculate statistics
    * [x] min, max
    * [x] mean, variance and standard deviation
    * [x] covariance and correlation
    * [x] normalization
  * IO
    * [x] Read data to/from CSV string
  * Generators
    * [x] Constant series
    * [x] Random noise
    * [x] Random Walk
    * [x] periodic pattern
  * Metrics
    * [ ] MSE between 2 series
    * [ ] MAE between 2 series
  * ARIMA
    * [ ] Check is series is stationary
    * [ ] AR(p) - Autoregressive
    * [ ] I(d) - Integrate
    * [ ] MA(q) - Moving average
  * Advanced functionality
    * [x] Finding sessions (periods of activity)



# Join in!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/carl/timeseries/issues).



# Redistributing

timeseries source code is distributed under the Apache-2.0 license.

**Contributions**

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
licensed as above, without any additional terms or conditions.
