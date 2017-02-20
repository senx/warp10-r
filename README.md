# warpscriptr #

R client for executing Warpscript on a Warp10 instance

### Installation ###

```
#!bash

Rscript install.R
```

### First step ###


```
#!R

library(warpscriptr)
postWarpscript("'Hello world' NOW", outputType="pretty", endpoint="http://localhost:8080/api/v0/exec")
```


### Documentation ###

see `warpscriptr.pdf`