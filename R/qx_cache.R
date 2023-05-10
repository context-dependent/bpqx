#' Produce a path in the bpqx data cache
#' @param ...
#'   additional path nodes
#' @param .p
#'   cache root, automatically configured by use_bpqx
qx_cache_path <- function(..., .p = NULL) {

}

#' Produce the path to the root folder of the qx_cache.
#' Prompt setup with use_bpqx if missing.
#' Prompt vpn connection if set up but doesn't exist.
qx_cache_root <- function() {

}

qx_conf <- function() {

}

.timeout <- function(func, ..., seconds = 1, default = NULL) {
    tryCatch(
        {
            setTimeLimit(elapsed = seconds, transient = TRUE)
            func(...)
        },
        error = function(e) {
            default
        }
    )
}
