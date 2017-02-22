#' Extract Geo Time Series
#'
#' Extract all GTS from a JSON response and return them merged as a data frame
#' @param response response body of a post request (a character vector or a json)
#' @param withLabels if TRUE, column names also include Labels. Default to FALSE
#' @return data frame
#' @export
#' @importFrom jsonlite minify fromJSON
#' @importFrom stringr str_match_all

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

  # and build a dataFrame out of it
  df <- data.frame(timestamp=integer())
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
        values <- data.frame(values, check.names=FALSE)
        df <- merge(df, values,by="timestamp", all=TRUE)
      }
    }
  }

  return(df)
}

#' Post Warpscript Code
#' 
#' Post warpscript code to a Warp 10 instance and retrieve response as charcter vector, json, named list or data frame.
#' @param warpscript code or file name ending with .mc2
#' @param outputType the type of the returned value. The supported types are "raw", "json", "pretty", "list" and "dataFrame". Default to "json". If outputType is "dataFrame", only GTS present in the response will be included in the returned data frame.
#' @param endpoint egress endpoint. Default to "http://localhost:8080/api/v0/exec"
#' @return character vector or json or named list or data frame
#' @export
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON minify prettify

postWarpscript <- function(warpscript, outputType="json", endpoint="http://localhost:8080/api/v0/exec"){

  if (substr(warpscript, nchar(warpscript) - 3, nchar(warpscript)) == '.mc2'){
    warpscript = readLines(warpscript, warn=FALSE)
  }

  # post request
  request = POST(endpoint, body=warpscript)
  cat(paste0(" Status: ", request$status, "\n"))

  # retrieve body
  body <- content(request, "text", encoding="UTF-8")

  # check status and return error or parsed JSON
  if (request$status != 200) {

    # parse error message
    body = strsplit(body,"\n")[[1]][9]
    n = nchar(body)
    msg = substring(body, 9, n - 10)
    cat(paste0(msg,"\n"))
  
  } else {

    if (outputType == "raw"){
      return(body)
    }

    if (outputType == "dataFrame"){
      return(extractGTS(body))
    }

    body = gsub('(\\W)NaN(\\W)', '\\1null\\2', body)

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
