#' Create Connection
#'
#' Create a connection to a Warp 10 database.
#' The connexion is an R6 object with few informations
#'
#' @inheritParams documentation
#'
#' @export
#'
wrp_connect <- function(endpoint = get_endpoint()) {
  connect$new(endpoint = endpoint)
}

connect <- R6::R6Class(
  classname = "warp10",
  public    = list(
    initialize = function(endpoint) {
      assert_endpoint(endpoint)
      private$endpoint <- endpoint
    },
    clear_script = function() {
      private$script <- ""
      return(self)
    },
    get_endpoint = function() {
      private$endpoint
    },
    get_script = function() {
      private$script
    },
    print = function() {
      cat(private$script)
    },
    set_script = function(script) {
      private$script     <- paste(private$script, script, paste = "\n")
      private$stack_size <- private$stack_size + 1
      return(self)
    }
  ),
  private = list(
    script     = "",
    endpoint   = NULL,
    stack_size = 0
  )
)
