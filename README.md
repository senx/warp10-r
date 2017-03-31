# warp10r #

R client for executing Warpscript on a Warp10 instance

### Installation from source ###

```bash
R CMD INSTALL warp10r
```

### Documentation ###

see `warp10r.pdf`


### First steps ###

Hello World

```R
library(warp10r)
postWarpscript("'Hello world' NOW", outputType="pretty", endpoint="https://warp.cityzendata.net/api/v0/exec")
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
options(scipen=20)
df <- postWarpscript(
  "NEWGTS 'randGTS' RENAME 1 10 <% h RAND RAND NaN RAND ADDVALUE %> FOR
  NEWGTS 'nogeoTS' RENAME 2 11 <% h NaN NaN NaN RAND ADDVALUE %> FOR",
  outputType="dataFrame",
  endpoint="https://warp.cityzendata.net/api/v0/exec")
df
```

```out
 Status: 200
     timestamp    nogeoTS randGTS.lat randGTS.lon    randGTS
1   3600000000         NA  0.87099507  0.93634240 0.09402443
2   7200000000 0.07420913  0.57297676  0.03031818 0.66382453
3  10800000000 0.96817580  0.47228877  0.93688396 0.83812560
4  14400000000 0.73406256  0.08157067  0.28171510 0.06737351
5  18000000000 0.46082755  0.05734425  0.96935579 0.87079701
6  21600000000 0.78599726  0.93150424  0.44859300 0.97788627
7  25200000000 0.46165074  0.74583285  0.74990348 0.10461472
8  28800000000 0.34320106  0.15692930  0.15190783 0.24054082
9  32400000000 0.95503016  0.02361383  0.42274112 0.98896688
10 36000000000 0.01279738  0.96326252  0.91678608 0.14959980
11 39600000000 0.83390927          NA          NA         NA
```

Push example

```R
pushWarp10(toGtsInputFormat(df), token="<myToken>", endpoint="http://<myEndpoint>/api/v0/update")
```

```out
 Status: 200
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

To convert timestamp in human readable date

```R
df$timestamp <- as.POSIXct(df$timestamp / 1000000, origin="1970-01-01")
```
