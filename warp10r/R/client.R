#' Extract Geo Time Series
#'
#' Extract all GTS from a JSON response and return them merged as a data.table. If too slow, consider using postWarpscript with outputType="data.table" instead.
#' @param response response body of a post request (a character vector or a json)
#' @param withLabels if TRUE, column names also include Labels. Default to FALSE
#' @return data.table
#' @export
#' @importFrom jsonlite minify fromJSON
#' @importFrom stringr str_match_all
#' @importFrom data.table data.table

extractGTS <- function(response, withLabels=FALSE){

  body = gsub('(\\W)NaN(\\W)', '\\1null\\2', response)
  body = minify(body)

  # we start by replacing labels {"key":"value"} as {key=value}
  headRegexp = '\"([la])\":\\{\"([^\"]*)\":\"([^\"]*)\"'
  tailRegexp = ',\"([^\"]*)\":\"([^\"]*)\"'

  body = gsub(headRegexp, '\"\\1\":{\\2=\\3', body)
  body = gsub(tailRegexp, ',\\1=\\2', body)

  # then we extract all gts within the json
  gtsRegexp = '\\{\"c\":\"([^\"]*)\",\"l\":(\\{[^\"]*\\}),\"a\":\\{[^\"]*\\},\"v\":(\\[[^\\}]*\\])\\}'
  caught = str_match_all(body, gtsRegexp)[[1]]

  # and build a data.table out of it
  dt <- data.table(timestamp=integer())
  if (length(caught) > 0) {
    for (gts_id in 1:nrow(caught)){
      values = fromJSON(caught[gts_id,4])
      if (length(values) != 0) {
        gtsColName = caught[gts_id,2]
        if (withLabels) {
          gtsColName = paste0(gtsColName,caught[gts_id,3])  
        }
        numCol = ncol(values)
        if (is.null(numCol)){
          stop('Datapoints of a same GTS must have geo stamps of same dimension')
        }
        if (numCol == 2){
          colnames(values) <- c("timestamp", gtsColName)
        }
        if (numCol == 3){
          colnames(values) <- c("timestamp", paste0(gtsColName,'.geo'), gtsColName)
        }
        if (numCol == 4){
          colnames(values) <- c("timestamp", paste0(gtsColName,'.lat'), paste0(gtsColName,'.lon'), gtsColName)
        }
        if (numCol == 5){
          colnames(values) <- c("timestamp", paste0(gtsColName,'.lat'), paste0(gtsColName,'.lon'), paste0(gtsColName,'.elev'), gtsColName)
        }        
        values <- data.table(values, check.names=FALSE)
        
        # convert if wrong type
        if (!(is.numeric(values$timestamp))) {
          convert <- function(x) {as.numeric(as.character(x))}
          values[, c(1:(numCol-1))] <- sapply(values[, c(1:(numCol-1))], convert)
          values[, numCol] <- as.character(values[, numCol])
        }

        # merge
        dt <- merge(dt, values, by="timestamp", all=TRUE)
      }
    }
  }

  return(dt)
}

#' Post Warpscript Code
#' 
#' Post warpscript code to a Warp 10 instance and retrieve response as character vector, json, named list or data frame.
#' @param warpscript code or file name ending with .mc2
#' @param outputType the type of the returned value. The supported types are "raw", "json", "pretty", "list" and "data.table". Default to "json". If outputType is "data.table", require the first level of the warpscript stack to be a list of GTS"
#' @param endpoint egress endpoint. Default to "http://localhost:8080/api/v0/exec"
#' @param withLabels if TRUE and if outputType is "data.table", column names also include Labels. Default to FALSE
#' @return character vector or json or named list or data frame
#' @export
#' @importFrom httr POST content headers
#' @importFrom jsonlite fromJSON minify prettify

postWarpscript <- function(warpscript, outputType="json", endpoint="http://localhost:8080/api/v0/exec", withLabels=FALSE){

  if (substr(warpscript, nchar(warpscript) - 3, nchar(warpscript)) == '.mc2'){
    warpscript <- readLines(warpscript, warn=FALSE)
  }

  # post request
  if (outputType == "data.table"){
    request <- POST(endpoint, body=paste0(warpscript, if (withLabels) preconverter_with_labels else preconverter))
  } else {
    request <- POST(endpoint, body=warpscript)
  }
  cat(paste0(" Status: ", request$status, "\n"))

  # retrieve body
  body <- content(request, "text", encoding="UTF-8")

  # check status and return error or parsed JSON
  if (request$status != 200) {

    # parse error message
    h <- headers(request)
    cat(paste0("ERROR line #", h[["X-Warp10-Error-Line"]], ": ", h[["X-Warp10-Error-Message"]], "\n"))
  
  } else {

    if (outputType == "raw"){
      return(body)
    }

    if (outputType == "dataFrame"){
      cat("outputType=\"dataFrame\" is deprecated. Consider using outputType=\"data.table\" instead")
      return(extractGTS(body, withLabels))
    }

    if (outputType == "data.table"){
      body <- gsub('(NaN|8888888888888888888|\"lviaezcdcdsqlzeuvnj\")', 'null', body)
      raw <- fromJSON(body, simplifyDataFrame=FALSE)
      dt <- data.table(raw[[2]])
      colnames(dt) <- raw[[1]]
      return(dt)
    }

    body <- gsub('(\\W)NaN(\\W)', '\\1null\\2', body)

    if (outputType == "json"){
      return(minify(body))
    }

    if (outputType == "pretty"){
      return(prettify(body, indent=2))
    }

    if (outputType == "list"){
      return(fromJSON(body, simplifyDataFrame=FALSE))
    }

    cat("ERROR: unrecognized outputType\n")
  }
}

#' Generate a Permalink
#' 
#' Generate a permalink to a quantum instance.
#' @param warpscript code or file name ending with .mc2
#' @param plot if TRUE, the generated link points to a plot. Default to FALSE
#' @param endpoint egress endpoint. Default to "http://localhost:8080/api/v0/exec"
#' @param quantum address of quantum instance. Default to "http://localhost:8090"
#' @return url
#' @export
#' @importFrom RCurl base64

permalink <- function(warpscript, plot=FALSE, endpoint="http://localhost:8080/api/v0/exec", quantum="http://localhost:8090"){

  if (substr(warpscript, nchar(warpscript) - 3, nchar(warpscript)) == '.mc2'){
    warpscript = readLines(warpscript, warn=FALSE)
  }

  url = quantum
  if (plot) {
    url = paste0(url, "#/plot/")
  } else {
    url = paste0(url, "#/warpscript/")
  }
  url = paste0(url, base64(warpscript), "/", base64(endpoint))

  return(url)
}


#' Push data points
#' 
#' Push data points to an ingress instance.
#' @param data data points in GTS input format as a character vector or a filename ending with .data or .gz
#' @param token write token
#' @param endpoint ingress endpoint. Default to "http://localhost:8080/api/v0/update"
#' @export
#' @importFrom httr POST content add_headers upload_file

pushWarp10 <- function(data, token, endpoint="http://localhost:8080/api/v0/update"){
  if ((substr(data, nchar(data) - 4, nchar(data)) == '.data') | (substr(data, nchar(data) - 2, nchar(data)) == '.gz')) {
    data = upload_file(data)
  }

  request = POST(endpoint, add_headers("X-Warp10-Token"=token), body=data)

  # print status
  cat(paste0(" Status: ", request$status, "\n"))
  if (request$status != 200) {
    cat(content(request, "text", encoding="UTF-8"))
  }
}

#' Convert a data frame
#' 
#' Convert a data frame into GTS input format.
#' @param dataFrame first column must be named "timestamp". Column names corresponding to GTS values must be in the <selector> form: "classname\{label1=value1,label2=value2,...\}" (or "classname"). Optional geo columns must be named "<selector>.lat", "<selector>.lon" or "<selector>.elev"
#' @return data points in GTS input format as a character vector
#' @export

toGtsInputFormat <- function(dataFrame){
  c <- colnames(dataFrame)

  if (c[1] != 'timestamp') {
    stop("First column must be named timestamp")
  }

  lat <- c[substr(c, nchar(c) - 3, nchar(c)) == '.lat']
  lon <- c[substr(c, nchar(c) - 3, nchar(c)) == '.lon']
  elev <- c[substr(c, nchar(c) - 4, nchar(c)) == '.elev']
  value <- c[(!(c %in% lat)) & (!(c %in% lon)) & (!(c %in% elev))]

  res <- ''
  for (name in value) {

    if (name == 'timestamp') {
      next
    }

    sub <- c('timestamp', name)

    nlat <- paste0(name, '.lat')
    nlon <- paste0(name, '.lon')
    nelev <- paste0(name, '.elev')

    if ((nelev %in% elev)) {
      sub <- c('timestamp', nelev, name)
    }

    if ((nlat %in% lat) & (nlon %in% lon)) {
      sub <- c('timestamp', nlat, nlon, name)
    }

    if ((nlat %in% lat) & (nlon %in% lon) & (nelev %in% elev)) {
      sub <- c('timestamp', nlat, nlon, nelev, name)
    }

    classname <- name
    if (substr(name, nchar(name), nchar(name)) != '}'){
      classname <- paste0(name, '{}')
    }

    subDf <- dataFrame[sub]
    first <- TRUE

    for (rowId in 1:nrow(subDf)) {
      if (is.na(subDf[name][rowId,1])) {
        next
      }

      if (ncol(subDf) == 2) {
        if (first) {
          res <- paste0(res, subDf[rowId, 1], '// ', classname, ' ', subDf[rowId, 2], '\n')
          first <- FALSE
        } else {
          res <- paste0(res, subDf[rowId, 1], '// ', subDf[rowId, 2], '\n')
        }        
      }

      if (ncol(subDf) == 3) {
        if (first) {
          res <- paste0(res, subDf[rowId, 1], '//', subDf[rowId,2], ' ', classname, ' ', subDf[rowId, 3], '\n')
          first <- FALSE
        } else {
          res <- paste0(res, subDf[rowId, 1], '//', subDf[rowId,2], ' ', subDf[rowId, 3], '\n')
        }        
      }

      if (ncol(subDf) == 4) {
        if (first) {
          res <- paste0(res, subDf[rowId, 1], '/', subDf[rowId,2], ':', subDf[rowId,3], '/ ', classname, ' ', subDf[rowId, 4], '\n')
          first <- FALSE
        } else {
          res <- paste0(res, subDf[rowId, 1], '/', subDf[rowId,2], ':', subDf[rowId,3], '/ ', subDf[rowId, 4], '\n')
        }
      }

      if (ncol(subDf) == 5) {
        if (first) {
          res <- paste0(res, subDf[rowId, 1], '/', subDf[rowId,2], ':', subDf[rowId,3], '/', subDf[rowId,4], ' ', classname, ' ', subDf[rowId, 5], '\n')
          first <- FALSE
        } else {
          res <- paste0(res, subDf[rowId, 1], '/', subDf[rowId,2], ':', subDf[rowId,3], '/', subDf[rowId,4], ' ', subDf[rowId, 5], '\n')
        }
      }

    }
  }

  res = gsub('(\\W)NA(\\W)', '\\1\\2', res)
  return(res)
}

preconverter = "
//
// Prepare List of Gts to be converted in R data.table object
//

// retrieve first level of the stack
'gtsList' STORE CLEAR

// check if it is a list of GTS
$gtsList <% TYPEOF 'LIST' != %> <% 'List of Gts must be on first level of the stack' MSGFAIL %> IFT
$gtsList <% <% TYPEOF 'GTS' != %> <% 'List of Gts must be on first level of the stack' MSGFAIL %> IFT %> FOREACH

// make tickbase GTS
$gtsList TICKS 'ticks' STORE
$ticks [] [] [] $ticks MAKEGTS 'baseGTS' STORE

// function: meta-data gathering
[] 'meta' STORE
<% $meta SWAP + 'meta' STORE %> 'colName' STORE

// function: check not all NaN
<% UNIQUE DUP SIZE 1 == SWAP 0 GET ISNaN && %> 'isAllNaN' STORE

// function: define NA equivalent
<% { 'DOUBLE' NaN 'STRING' 'lviaezcdcdsqlzeuvnj' 'LONG' 8888888888888888888 } SWAP GET %> 'NA' STORE

// data
$ticks 'timestamps' @colName
$gtsList
<%
  'gts' STORE
  $gts NAME 'name' STORE
  $gts VALUES 0 GET TYPEOF 'type' STORE
  [ $gts true mapper.replace 0 0 0 ] MAP
  'mask' STORE
  [ $mask [ $baseGTS ] [] op.negmask ] APPLY
  [ SWAP $type @NA mapper.replace 0 0 0 ] MAP 
  0 GET 'residualSeries' STORE
  [ $gts $residualSeries ] MERGE
  'gts' STORE
  $gts LOCATIONS 'lon' STORE 'lat' STORE
  <% $lat @isAllNaN ! %> <% $lat $name '.lat' + @colName %> IFT
  <% $lon @isAllNaN ! %> <% $lon $name '.lon' + @colName %> IFT
  $gts ELEVATIONS 'elev' STORE
  <% $elev @isAllNaN ! %> <% $elev $name '.elev' + @colName %> IFT
  $gts VALUES $name @colName
%>
FOREACH
DEPTH ->LIST ZIP
$meta
"

preconverter_with_labels = "
//
// Prepare List of Gts to be converted in R data.table object
//

// retrieve first level of the stack
'gtsList' STORE CLEAR

// check if it is a list of GTS
$gtsList <% TYPEOF 'LIST' != %> <% 'List of Gts must be on first level of the stack' MSGFAIL %> IFT
$gtsList <% <% TYPEOF 'GTS' != %> <% 'List of Gts must be on first level of the stack' MSGFAIL %> IFT %> FOREACH

// make tickbase GTS
$gtsList TICKS 'ticks' STORE
$ticks [] [] [] $ticks MAKEGTS 'baseGTS' STORE

// function: meta-data gathering
[] 'meta' STORE
<% $meta SWAP + 'meta' STORE %> 'colName' STORE

// function: check not all NaN
<% UNIQUE DUP SIZE 1 == SWAP 0 GET ISNaN && %> 'isAllNaN' STORE

// function: define NA equivalent
<% { 'DOUBLE' NaN 'STRING' 'lviaezcdcdsqlzeuvnj' 'LONG' 8888888888888888888 } SWAP GET %> 'NA' STORE

// data
$ticks 'timestamps' @colName
$gtsList
<%
  'gts' STORE
  $gts NAME $gts LABELS ->JSON + 'name' STORE
  $gts VALUES 0 GET TYPEOF 'type' STORE
  [ $gts true mapper.replace 0 0 0 ] MAP
  'mask' STORE
  [ $mask [ $baseGTS ] [] op.negmask ] APPLY
  [ SWAP $type @NA mapper.replace 0 0 0 ] MAP 
  0 GET 'residualSeries' STORE
  [ $gts $residualSeries ] MERGE
  'gts' STORE
  $gts LOCATIONS 'lon' STORE 'lat' STORE
  <% $lat @isAllNaN ! %> <% $lat $name '.lat' + @colName %> IFT
  <% $lon @isAllNaN ! %> <% $lon $name '.lon' + @colName %> IFT
  $gts ELEVATIONS 'elev' STORE
  <% $elev @isAllNaN ! %> <% $elev $name '.elev' + @colName %> IFT
  $gts VALUES $name @colName
%>
FOREACH
DEPTH ->LIST ZIP
$meta
"