# warp10r #

R client for executing WarpScript on a Warp 10 instance

### Installation from source ###

Require R libraries: httr, RCurl, jsonlite, stringr, bitops, data.table

```bash
R CMD INSTALL warp10r
```

### Documentation ###

see `warp10r.pdf`


### First steps ###

Hello World

```R
library(warp10r)
postWarpscript("'Hello world' NOW", outputType="pretty", endpoint="https://warp.senx.io/api/v0/exec")
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
df <- postWarpscript( #expects a warpscript returning a list of GTS
  "[ NEWGTS 'randGTS' RENAME 1 10 <% h RAND RAND NaN RAND ADDVALUE %> FOR
  NEWGTS 'nogeoTS' RENAME 2 11 <% h NaN NaN NaN RAND ADDVALUE %> FOR ]",
  outputType="data.table",
  endpoint="https://warp.senx.io/api/v0/exec")
```

Returns a data.table for which the column `timestamps` is shared between all GTS.<br/>
Note that in case of multiple GTS, they need to have at most one value per timestamp.

```out
df
 Status: 200
     timestamps randGTS.lat randGTS.lon    randGTS    nogeoTS
 1:  3600000000   0.1360472   0.5646129 0.96810855         NA
 2:  7200000000   0.8854959   0.1332623 0.71743495 0.43692989
 3: 10800000000   0.1682685   0.1445078 0.79938432 0.68983094
 4: 14400000000   0.6101049   0.3162186 0.81602426 0.86114494
 5: 18000000000   0.1488110   0.4454609 0.08290809 0.83700514
 6: 21600000000   0.2036094   0.3059227 0.06738745 0.58656507
 7: 25200000000   0.9268167   0.8949560 0.48696337 0.09783341
 8: 28800000000   0.6331636   0.5532274 0.84180407 0.91651408
 9: 32400000000   0.9440473   0.2198834 0.91475390 0.29334647
10: 36000000000   0.8281828   0.7831793 0.66450939 0.73855863
11: 39600000000          NA          NA         NA 0.06546927
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
  endpoint="https://warp.senx.io/api/v0/exec",
  quantum="https://home.senx.io/quantum/")
```

[`[1] "https://home.senx.io/quantum/#/plot/TkVXR1RTIDEgMTAwMCA8JSAnaScgU1RPUkUgJGkgMSB3ICogTmFOIE5hTiBOYU4gJGkgUEkgMTIwIC8gKiBDT1MgQUREVkFMVUUgJT4gRk9SCiAgJ0Nvc2ludXMgZXhhbXBsZScgUkVOQU1F/aHR0cHM6Ly93YXJwLmNpdHl6ZW5kYXRhLm5ldC9hcGkvdjAvZXhlYw==`](https://home.senx.io/quantum/#/plot/TkVXR1RTIDEgMTAwMCA8JSAnaScgU1RPUkUgJGkgMSB3ICogTmFOIE5hTiBOYU4gJGkgUEkgMTIwIC8gKiBDT1MgQUREVkFMVUUgJT4gRk9SCiAgJ0Nvc2ludXMgZXhhbXBsZScgUkVOQU1F/aHR0cHM6Ly93YXJwLmNpdHl6ZW5kYXRhLm5ldC9hcGkvdjAvZXhlYw==)"

To convert timestamp in human readable date

```R
df$timestamp <- as.POSIXct(df$timestamp / 1000000, origin="1970-01-01")
```
