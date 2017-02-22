# warp10r #

R client for executing Warpscript on a Warp10 instance

### Installation from source ###

Require R libraries: httr, RCurl, jsonlite, stringr

```bash
R CMD INSTALL warp10r
```

### Documentation ###

see `warp10r.pdf`


### First steps ###

Hello World

```R
library(warp10r)
postWarpscript("'Hello world' NOW", outputType="pretty", endpoint="http://localhost:8080/api/v0/exec")
```

```out
Status: 200
[
  1487669519106000,
  "Hello world"
]
```

Example with Geo Time Series

```R
postWarpscript(
  "NEWGTS 'randGTS' RENAME 1 10 <% 100 * RAND RAND NaN RAND ADDVALUE %> FOR
  NEWGTS 'nogeoTS' RENAME 2 11 <% 100 * NaN NaN NaN RAND ADDVALUE %> FOR",
  outputType="dataFrame",
  endpoint="http://localhost:8080/api/v0/exec")
```

```out
 Status: 200
   timestamp   nogeoTS  randGTS.lat randGTS.lon   randGTS
1        100        NA 0.5742330803 0.002447935 0.0542487
2        200 0.2812567 0.2160466975 0.203367937 0.2459928
3        300 0.5893098 0.6406101678 0.169097185 0.3079759
4        400 0.3849952 0.5910933623 0.391406296 0.5402910
5        500 0.7801280 0.2211455349 0.991288712 0.6953682
6        600 0.4600786 0.4256033711 0.853263745 0.1816866
7        700 0.6767966 0.9512155037 0.453630527 0.8362666
8        800 0.8403910 0.1712334808 0.650456389 0.4757557
9        900 0.1251300 0.4399808077 0.252384050 0.7146097
10      1000 0.1570902 0.0002819253 0.726212533 0.1683950
11      1100 0.6551525           NA          NA        NA
```

Permalink example

```R
permalink(
  "NEWGTS 1 1000 <% 'i' STORE $i 1 w * NaN NaN NaN $i PI 120 / * COS ADDVALUE %> FOR
  'Cosinus example' RENAME",
  plot=TRUE,
  endpoint="https://warp.cityzendata.net/api/v0/exec",
  quantum="https://home.cityzendata.net/quantum/")
```

[`[1] "https://home.cityzendata.net/quantum/#/plot/TkVXR1RTIDEgMTAwMCA8JSAnaScgU1RPUkUgJGkgMSB3ICogTmFOIE5hTiBOYU4gJGkgUEkgMTIwIC8gKiBDT1MgQUREVkFMVUUgJT4gRk9SCiAgJ0Nvc2ludXMgZXhhbXBsZScgUkVOQU1F/aHR0cHM6Ly93YXJwLmNpdHl6ZW5kYXRhLm5ldC9hcGkvdjAvZXhlYw==`](https://home.cityzendata.net/quantum/#/plot/TkVXR1RTIDEgMTAwMCA8JSAnaScgU1RPUkUgJGkgMSB3ICogTmFOIE5hTiBOYU4gJGkgUEkgMTIwIC8gKiBDT1MgQUREVkFMVUUgJT4gRk9SCiAgJ0Nvc2ludXMgZXhhbXBsZScgUkVOQU1F/aHR0cHM6Ly93YXJwLmNpdHl6ZW5kYXRhLm5ldC9hcGkvdjAvZXhlYw==)"
