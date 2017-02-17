library(httr)
library(jsonlite)
library(stringr)

postWarpscript <- function(warpscript, endpoint="http://localhost:8080/api/v0/exec", outputType="list", withLabels=FALSE){
  
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

    if (outputType == "body"){
      return(body)
    }

    if (outputType == "raw"){
      return(minify(body))
    }

    if (outputType == "pretty"){
      return(prettify(body, indent=2))
    }

    if (outputType == "list"){
      return(fromJSON(body, simplifyDataFrame=FALSE))
    }

    if (outputType == "dataFrame"){

      body=minify(body)

      # we start by replacing labels and attributes {"key":"value"} as {key=value}
      headRegexp = '\"([la])\":\\{\"([^\"]*)\":\"([^\"]*)\",'
      tailRegexp = ',\"([^\"]*)\":\"([^\"]*)\"'

      body = gsub(headRegexp, '\"\\1\":{\\2=\\3,', body)
      body = gsub(tailRegexp, ',\\1=\\2', body)

      # then we extract all gts within the json
      gtsRegexp = '\\{\"c\":\"([^\"]*)\",\"l\":(\\{[^\"]*\\}),\"a\":\\{[^\"]*\\},\"v\":(\\[[^\\}]*\\])\\}'
      caught = str_match_all(body, gtsRegexp)[[1]]

      # and build a dataFrame out of it
      df <- data.frame(timestamp=integer())
      for (gts_id in 1:nrow(caught)){
        values = fromJSON(caught[gts_id,4])
        gtsColName = caught[gts_id,2]
        if (withLabels) {
          gtsColName = paste0(gtsColName,caught[gts_id,3])  
        }
        colnames(values) <- c("timestamp", gtsColName)
        values <- data.frame(values)
        df <- merge(df, values,by="timestamp", all=TRUE)
      }

      return(df)
    }

    cat("ERROR: unrecognized outputType\n")
  }

}