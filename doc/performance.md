# Performance notes

 * LocalDateTime 1M records ~= 70MB memory
 * Instant 1M records ~= 27MB memory
 * TimeSeries[Float] 1M records ~= 70MB memory
 * TimeSeries[Double] 1M records ~= 80MB memory
 
 
## Benchmark report 

```
1. Measure: map
1,000,000 points: 67.943919 ms.

2. Measure: groupBy
1,000,000 points: 135.562344 ms.

3. Measure: rollingWindow
100,000 points: 247.156599 ms.
1,000,000 points: 3027.655978 ms.

4. Measure: resample
100,000 points: 55.898174 ms.
1,000,000 points: 567.447768 ms.

5. Measure: integrateByTime
1,000,000 points: 143.223433 ms.

6. Measure: step (This will create 1M series as a output)
200,000 points: 496.940703 ms.

7. Measure: findSessions
1,000,000 points: 124.038921 ms.
```