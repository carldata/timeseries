# Welcome to the Time Series library

[![Build status](https://travis-ci.org/carldata/timeseries.svg?branch=master)](https://travis-ci.org/carldata/timeseries)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.github.carldata/timeseries_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.carldata/timeseries_2.12)

Library for processing Time Series.


## Quick start
 
 Add the following dependency to the build.sbt
 ```scala
 libraryDependencies += "io.github.carldata" %% "timeseries" % "0.2.1"
 ```


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
