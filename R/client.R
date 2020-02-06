# nolint start
#
#   Copyright 2018  SenX S.A.S.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#' Extract Geo Time Series
#'
#' Extract all GTS from a JSON response and return them merged as a data.table.
#' If too slow, consider using postWarpscript with outputType="data.table" instead.
#' @param response response body of a post request (a character vector or a json)
#' @param with_labels if TRUE, column names also include Labels. Default to FALSE
#' @return data.table
#' @export
#' @importFrom jsonlite minify fromJSON
#' @importFrom stringr str_match_all
#' @importFrom data.table data.table

extract_gts <- function(response, with_labels = FALSE) {

  body <- gsub("(\\W)NaN(\\W)", "\\1null\\2", response)
  body <- minify(body)

  # we start by replacing labels {"key":"value"} as {key=value}
  head_regexp <- "\"([la])\":\\{\"([^\"]*)\":\"([^\"]*)\""
  tail_regexp <- ",\"([^\"]*)\":\"([^\"]*)\""

  body <- gsub(head_regexp, "\"\\1\":{\\2=\\3", body)
  body <- gsub(tail_regexp, ",\\1=\\2", body)

  # then we extract all gts within the json
  gts_regexp <- "\\{\"c\":\"([^\"]*)\",\"l\":(\\{[^\"]*\\}),\"a\":\\{[^\"]*\\},\"v\":(\\[[^\\}]*\\])\\}"
  caught <- str_match_all(body, gts_regexp)[[1]]

  # and build a data.table out of it
  dt <- data.table(timestamp = integer())
  if (length(caught) > 0) {
    for (gts_id in seq_len(nrow(caught))) {
      values <- fromJSON(caught[gts_id, 4])
      if (length(values) != 0) {
        gts_colname <- caught[gts_id, 2]
        if (with_labels) {
          gts_colname <- paste0(gts_colname, caught[gts_id, 3])
        }
        num_col <- ncol(values)
        if (is.null(num_col)) {
          stop("Datapoints of a same GTS must have geo stamps of same dimension")
        }
        if (num_col == 2) {
          colnames(values) <- c("timestamp", gts_colname)
        }
        if (num_col == 3) {
          colnames(values) <- c("timestamp", paste0(gts_colname, ".geo"), gts_colname)
        }
        if (num_col == 4) {
          colnames(values) <- c("timestamp", paste0(gts_colname, ".lat"), paste0(gts_colname, ".lon"), gts_colname)
        }
        if (num_col == 5) {
          colnames(values) <- c("timestamp", paste0(gts_colname, ".lat"), paste0(gts_colname, ".lon"),
            paste0(gts_colname, ".elev"), gts_colname)
        }
        values <- data.table(values, check.names = FALSE)

        # convert if wrong type
        if (!(is.numeric(values$timestamp))) {
          convert <- function(x) as.numeric(as.character(x))
          values[, c(1:(num_col - 1))] <- sapply(values[, c(1:(num_col - 1))], convert)
          values[, num_col] <- as.character(values[, num_col])
        }

        # merge
        dt <- merge(dt, values, by = "timestamp", all = TRUE)
      }
    }
  }

  return(dt)
}

#' Post Warpscript Code
#'
#' Post warpscript code to a Warp 10 instance and retrieve response as character vector, json,
#' named list or data.table.
#' If outputType is "data.table", only the top of the stack is converted and it must be a list of GTS.
#' If this list is not a singleton, these GTS must have at most one value per timestamp.
#'
#' @param warpscript code or file name ending with .mc2
#' @param output_type the type of the returned value.
#'                   The supported types are "raw", "json", "pretty", "list" and "data.table".
#'                   Default to "json". Output type "dataFrame" is deprecated in favor of "data.table".
#' @param endpoint egress endpoint. Default to "http://localhost:8080/api/v0/exec"
#' @param with_labels if TRUE and if outputType is "data.table", column names also include Labels.
#'                   Default to FALSE
#' @return character vector or json or named list or data.table
#' @export
#' @importFrom httr POST content headers config
#' @importFrom jsonlite fromJSON minify prettify

post_warpscript <- function(warpscript, output_type = "json", endpoint = "http://localhost:8080/api/v0/exec",
                            with_labels = FALSE) {

  endpoint <- paste0(endpoint, "/exec")
  if (substr(warpscript, nchar(warpscript) - 3, nchar(warpscript)) == ".mc2") {
    warpscript <- readLines(warpscript, warn = FALSE)
  }

  # post request
  if (output_type == "data.table") {
    request <- POST(endpoint, body = paste0(warpscript, if (with_labels) preconverter_with_labels else preconverter))
  } else {
    request <- POST(endpoint, body = warpscript, config = config(ssl_verifypeer = FALSE))
  }

  # retrieve body
  body <- content(request, "text", encoding = "UTF-8")

  # check status and return error or parsed JSON
  if (request$status != 200) {

    # parse error message
    h <- headers(request) # nolint unused variable
    msg <- glue::glue(
      "Status: {request$status}",
      "ERRPR line #{h[['X-Warp10-Error-Line']]}: {h[['X-Warp10-Error-Message']]}"
    )
    stop(msg, call. = FALSE)
  } else {

    if (output_type == "raw") {
      return(body)
    }

    if (output_type == "dataFrame") {
      cat("outputType=\"dataFrame\" is deprecated. Consider using outputType=\"data.table\" instead")
      return(extract_gts(body, with_labels))
    }

    if (output_type == "data.table") {
      body <- gsub("(NaN|8888888888888888888|\\\"lviaezcdcdsqlzeuvnj\\\")", "null", body)
      raw <- fromJSON(body, simplifyDataFrame = FALSE)
      dt <- data.table(raw[[2]])
      colnames(dt) <- raw[[1]]
      return(dt)
    }

    body <- gsub("(\\W)NaN(\\W)", "\\1null\\2", body)

    if (output_type == "json") {
      return(minify(body))
    }

    if (output_type == "pretty") {
      return(prettify(body, indent = 2))
    }

    if (output_type == "list") {
      return(fromJSON(body, simplifyDataFrame = FALSE))
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

permalink <- function(warpscript, plot = FALSE, endpoint = "http://localhost:8080/api/v0/exec",
                      quantum = "http://localhost:8090") {

  if (substr(warpscript, nchar(warpscript) - 3, nchar(warpscript)) == ".mc2") {
    warpscript <- readLines(warpscript, warn = FALSE)
  }

  url <- quantum
  if (plot) {
    url <- paste0(url, "#/plot/")
  } else {
    url <- paste0(url, "#/warpscript/")
  }
  url <- paste0(url, base64(warpscript), "/", base64(endpoint))

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

push_warp10 <- function(data, token, endpoint = "http://localhost:8080/api/v0/update") {
  if ((substr(data, nchar(data) - 4, nchar(data)) == ".data") | (substr(data, nchar(data) - 2, nchar(data)) == ".gz")) {
    data <- upload_file(data)
  }

  request <- POST(endpoint, add_headers("X-Warp10-Token" = token), body = data)

  # print status
  cat(paste0(" Status: ", request$status, "\n"))
  if (request$status != 200) {
    cat(content(request, "text", encoding = "UTF-8"))
  }
}

#' Convert a data frame or data table
#'
#' Convert a data frame into GTS input format.
#' @param df first column must be named "timestamp".
#'                  Column names corresponding to GTS values must be in the <selector> form:
#'                  "classname\{label1=value1,label2=value2,...\}" (or "classname").
#'                  Optional geo columns must be named "<selector>.lat",
#'                  "<selector>.lon" or "<selector>.elev"
#' @return data points in GTS input format as a character vector
#' @export

to_gts_input_format <- function(df) {

  if (class(df)[1] == "data.table") {
    df <- as.data.frame(df)
  }

  if (class(df)[1] != "data.frame") {
    stop("toGtsInputFormat() requires a data.frame or data.table as argument.")
  }

  c <- colnames(df)

  if (c[1] != "timestamp") {
    stop("First column must be named timestamp")
  }

  lat <- c[substr(c, nchar(c) - 3, nchar(c)) == ".lat"]
  lon <- c[substr(c, nchar(c) - 3, nchar(c)) == ".lon"]
  elev <- c[substr(c, nchar(c) - 4, nchar(c)) == ".elev"]
  value <- c[(!(c %in% lat)) & (!(c %in% lon)) & (!(c %in% elev))]

  res <- ""
  for (name in value) {

    if (name == "timestamp") {
      next
    }

    sub <- c("timestamp", name)

    nlat <- paste0(name, ".lat")
    nlon <- paste0(name, ".lon")
    nelev <- paste0(name, ".elev")

    if ((nelev %in% elev)) {
      sub <- c("timestamp", nelev, name)
    }

    if ((nlat %in% lat) & (nlon %in% lon)) {
      sub <- c("timestamp", nlat, nlon, name)
    }

    if ((nlat %in% lat) & (nlon %in% lon) & (nelev %in% elev)) {
      sub <- c("timestamp", nlat, nlon, nelev, name)
    }

    classname <- name
    if (substr(name, nchar(name), nchar(name)) != "}") {
      classname <- paste0(name, "{}")
    }

    sub_df <- df[sub]
    first <- TRUE

    for (rowId in seq_len(nrow(sub_df))) {
      if (is.na(sub_df[name][rowId, 1])) {
        next
      }

      if (ncol(sub_df) == 2) {
        if (first) {
          res <- paste0(res, sub_df[rowId, 1], "// ", classname, " ", sub_df[rowId, 2], "\n")
          first <- FALSE
        } else {
          res <- paste0(res, sub_df[rowId, 1], "// ", sub_df[rowId, 2], "\n")
        }
      }

      if (ncol(sub_df) == 3) {
        if (first) {
          res <- paste0(res, sub_df[rowId, 1], "//", sub_df[rowId, 2], " ", classname, " ", sub_df[rowId, 3], "\n")
          first <- FALSE
        } else {
          res <- paste0(res, sub_df[rowId, 1], "//", sub_df[rowId, 2], " ", sub_df[rowId, 3], "\n")
        }
      }

      if (ncol(sub_df) == 4) {
        if (first) {
          res <- paste0(res, sub_df[rowId, 1], "/", sub_df[rowId, 2], ":", sub_df[rowId, 3], "/ ", classname, " ",
            sub_df[rowId, 4], "\n")
          first <- FALSE
        } else {
          res <- paste0(res, sub_df[rowId, 1], "/", sub_df[rowId, 2], ":", sub_df[rowId, 3], "/ ",
            sub_df[rowId, 4], "\n")
        }
      }

      if (ncol(sub_df) == 5) {
        if (first) {
          res <- paste0(res, sub_df[rowId, 1], "/", sub_df[rowId, 2], ":", sub_df[rowId, 3], "/",
            sub_df[rowId, 4], " ", classname, " ", sub_df[rowId, 5], "\n")
          first <- FALSE
        } else {
          res <- paste0(res, sub_df[rowId, 1], "/", sub_df[rowId, 2], ":", sub_df[rowId, 3], "/",
            sub_df[rowId, 4], " ", sub_df[rowId, 5], "\n")
        }
      }

    }
  }

  res <- gsub("(\\W)NA(\\W)", "\\1\\2", res)
  return(res)
}

preconverter <- "
//
// Prepare List of Gts to be converted in R data.table object
//

// retrieve first level of the stack
'gtsList' STORE CLEAR

// check if it is a list of GTS
$gtsList <% TYPEOF 'LIST' != %> <% 'List of Gts must be on first level of the stack' MSGFAIL %> IFT
$gtsList <% <% TYPEOF 'GTS' != %> <% 'List of Gts must be on first level of the stack' MSGFAIL %> IFT %> FOREACH

// use either TICKS or TICKLIST to make base of ticks
$gtsList <% DUP SIZE 1 == %> <% 0 GET TICKLIST %> <% TICKS %> IFTE 'ticks' STORE
$ticks [] [] [] $ticks MAKEGTS 'baseGTS' STORE

// function: meta-data gathering
[] 'meta' STORE
<% $meta SWAP + 'meta' STORE %> 'colName' STORE

// function: check not all NaN
<% UNIQUE DUP SIZE 1 == SWAP 0 GET ISNaN && %> 'isAllNaN' STORE

// function: define NA equivalent
<% { 'DOUBLE' NaN 'STRING' 'lviaezcdcdsqlzeuvnj' 'LONG' 8888888888888888888 } SWAP GET %> 'NA' STORE

// fill in timestamps
$ticks 'timestamp' @colName

// other columns
$gtsList
<%
  'gts' STORE
  $gts NAME 'name' STORE

  // Fill in missing values with @NA
  <% $gtsList SIZE 1 == ! %>
  <%
    $gts VALUES 0 GET TYPEOF 'type' STORE
    [ $gts true mapper.replace 0 0 0 ] MAP
    'mask' STORE
    [ $mask [ $baseGTS ] [] op.negmask ] APPLY
    [ SWAP $type @NA mapper.replace 0 0 0 ] MAP
    0 GET 'residualSeries' STORE
    [ $gts $residualSeries ] MERGE SORT
    'gts' STORE
  %>
  IFT

  // Fill in locations and values
  $gts LOCATIONS 'lon' STORE 'lat' STORE
  <% $lat @isAllNaN ! %> <% $lat $name '.lat' + @colName %> IFT
  <% $lon @isAllNaN ! %> <% $lon $name '.lon' + @colName %> IFT
  $gts ELEVATIONS 'elev' STORE
  <% $elev @isAllNaN ! %> <% $elev $name '.elev' + @colName %> IFT
  $gts VALUES $name @colName
%>
FOREACH

// TRY to ZIP data
DEPTH ->LIST
<% ZIP %>
<%
  $gtsList
  <%
    <% DUP SIZE SWAP DEDUP SIZE != %>
    <% 'When the list have more than one GTS, they cannot have more than one value per timestamp' MSGFAIL %>
    IFT
  %>
  FOREACH
  'Zipping data prior to data.table conversion failed.' MSGFAIL
%>
<% %>
TRY
$meta
"

preconverter_with_labels <- "
//
// Prepare List of Gts to be converted in R data.table object
//

// retrieve first level of the stack
'gtsList' STORE CLEAR

// check if it is a list of GTS
$gtsList <% TYPEOF 'LIST' != %> <% 'List of Gts must be on first level of the stack' MSGFAIL %> IFT
$gtsList <% <% TYPEOF 'GTS' != %> <% 'List of Gts must be on first level of the stack' MSGFAIL %> IFT %> FOREACH

// use either TICKS or TICKLIST to make base of ticks
$gtsList <% DUP SIZE 1 == %> <% 0 GET TICKLIST %> <% TICKS %> IFTE 'ticks' STORE
$ticks [] [] [] $ticks MAKEGTS 'baseGTS' STORE

// function: meta-data gathering
[] 'meta' STORE
<% $meta SWAP + 'meta' STORE %> 'colName' STORE

// function: check not all NaN
<% UNIQUE DUP SIZE 1 == SWAP 0 GET ISNaN && %> 'isAllNaN' STORE

// function: define NA equivalent
<% { 'DOUBLE' NaN 'STRING' 'lviaezcdcdsqlzeuvnj' 'LONG' 8888888888888888888 } SWAP GET %> 'NA' STORE

// data
$ticks 'timestamp' @colName
$gtsList
<%
  'gts' STORE
  $gts NAME $gts LABELS ->JSON + 'name' STORE
  $gts VALUES 0 GET TYPEOF 'type' STORE

  // Fill in missing values with @NA
  <% $gtsList SIZE 1 == ! %>
  <%
    $gts VALUES 0 GET TYPEOF 'type' STORE
    [ $gts true mapper.replace 0 0 0 ] MAP
    'mask' STORE
    [ $mask [ $baseGTS ] [] op.negmask ] APPLY
    [ SWAP $type @NA mapper.replace 0 0 0 ] MAP
    0 GET 'residualSeries' STORE
    [ $gts $residualSeries ] MERGE SORT
    'gts' STORE
  %>
  IFT

  // Fill in locations and values
  $gts LOCATIONS 'lon' STORE 'lat' STORE
  <% $lat @isAllNaN ! %> <% $lat $name '.lat' + @colName %> IFT
  <% $lon @isAllNaN ! %> <% $lon $name '.lon' + @colName %> IFT
  $gts ELEVATIONS 'elev' STORE
  <% $elev @isAllNaN ! %> <% $elev $name '.elev' + @colName %> IFT
  $gts VALUES $name @colName
%>
FOREACH
// TRY to ZIP data
DEPTH ->LIST
<% ZIP %>
<%
  $gtsList
  <%
    <% DUP SIZE SWAP DEDUP SIZE != %>
    <% 'When the list have more than one GTS, they cannot have more than one value per timestamp' MSGFAIL %>
    IFT
  %>
  FOREACH
  'Zipping data prior to data.table conversion failed.' MSGFAIL
%>
<% %>
TRY
$meta
"

# nolint end
