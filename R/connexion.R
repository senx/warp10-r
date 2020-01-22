#' Create Connection
#'
#' Create a connection to a Warp 10 database.
#' The connexion is an R6 object with few informations
#'
#' @inheritParams documentation
#'
#' @export
#'
wrp_connect <- function(endpoint = get_endpoint(), token = get_token()) {
  connect$new(endpoint = endpoint, token = token)
}

connect <- R6::R6Class(
  classname = "warp10",
  public    = list(
    initialize = function(endpoint, token) {
      assert_endpoint(endpoint)
      private$endpoint <- endpoint
      private$token    <- token
      if (!is.null(token)) {
        self$set_script(glue::glue("'{token}' 'token' STORE\n"))
      }
    },
    add_stack = function(return, consume = list()) {
      stack <- private$stack
      call  <- gsub("\\(.*\\)", "", deparse(sys.call(-1)))
      if (length(consume) > 0 && length(stack) == 0) {
        msg <- glue::glue("{call} requires {length(consume)} object in the stack but none are provided.")
        stop(msg, call. = FALSE)
      }
      for (object in consume) {
        n        <- length(stack)
        last     <- stack[[n]]
        objects  <- strsplit(object, "|", fixed = TRUE)[[1]]
        if (!last %in% objects) {
          msg <- glue::glue("{call} requires `{toString(objects)}` but `{last}` on the stack.")
          stop(msg, call. = FALSE)
        }
        stack[[n]] <- NULL
      }
      private$stack <- append(stack, return)
    },
    clear_script = function() {
      private$script <- ""
      private$stack <- list()
      if (!is.null(private$token)) {
        self$set_script(glue::glue("'{private$token}' 'token' STORE\n"))
      }
    },
    get_endpoint = function() {
      private$endpoint
    },
    get_script = function() {
      private$script
    },
    get_stack = function() {
      private$stack
    },
    get_token = function() {
      private$token
    },
    print = function() {
      stack    <- private$stack
      endpoint <- private$endpoint
      token    <- !is.null(private$token)
      msg      <- glue::glue(
        "Warp10 connexion:",
        "  - endpoint: {endpoint}",
        "  - token:    {dplyr::if_else(token, 'available', 'not available')}",
        "  - stack:    {dplyr::if_else(length(stack) == 0, 'empty', toString(stack))}",
        .sep = "\n"
      )
      cat(msg)
    },
    set_script = function(script) {
      private$script <- paste(private$script, script, paste = "\n")
    }
  ),
  private = list(
    script     = "",
    endpoint   = NULL,
    stack      = list(),
    token      = NULL
  )
)
