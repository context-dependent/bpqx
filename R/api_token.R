#' Retrieve Qualtrics API token
api_token <- function() {
    if (token_cache_exists()) {
        readLines("~/.bpqx-auth")[1]
    } else {
        stop(
            "Cannot find token cache file at ~/.bpqx-auth\n",
            "Please run `save_api_token()`\n"
        )
    }
}

#' Stores Qualtrics API token in "~/.bpqx-auth"
#' @param .t
#'   A string which overrides interactive prompt.
#'   strictly for automated testing.
#' @export
save_api_token <- function(.t = NULL) {
    if (is.null(.t)) {
        p <- c(
            "Paste your Qualtrics API Token in the field below.",
            "Visit https://api.qualtrics.com/ZG9jOjg3NjYzMg-api-key-authentication",
            "if you need help finding it.",
            "Qualtrics API Token > "
        )
        t <- getPass::getPass(
            msg = paste0(p, collapse = "\n"),
            forcemask = TRUE
        )
    } else {
        t <- .t
    }

    if (token_cache_exists()) {
        file.remove("~/.bpqx-auth")
    }

    writeLines(t, "~/.bpqx-auth")
}


#' Check if "~/.bpqx-auth" exists
token_cache_exists <- function() {
    file.exists("~/.bpqx-auth")
}
