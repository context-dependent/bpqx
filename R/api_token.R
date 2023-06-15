#' Retrieve Qualtrics API token
#' @param .f
#'   String pointing to the location of the credentials file
api_token <- function(.f = "~/.bpqx-auth") {
    if (token_cache_exists(.f)) {
        yaml::read_yaml(.f)$api_token
    } else {
        rlang::abort(
            message = c(
                glue::glue("Cannot find token cache file at {.f}\n"),
                "Please run `save_api_token()`\n"
            )
        )
    }
}

#' Retrieve Data Center
data_center <- function(.f = "~/.bpqx-auth") {
    if (token_cache_exists(.f)) {
        yaml::read_yaml(.f)$data_center
    } else {
        stop(
            "Cannot find token cache file at ~/.bpqx-auth\n",
            "Please run `save_api_token()` to register your token\n"
        )
    }
}

#' Stores Qualtrics API token in "~/.bpqx-auth"
#' @param data_center
#'   A string specifying the data center to use
#' @param .t
#'   A string which overrides interactive prompt.
#'   strictly for automated testing.
#' @export
save_api_token <- function(
    data_center = "blueprintade.yul1",
    .t = NULL,
    .f = "~/.bpqx-auth") {
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

    if (token_cache_exists(.f)) {
        file.remove(.f)
    }

    yaml::write_yaml(
        list(
            api_token = t,
            data_center = data_center
        ),
        .f
    )
}


#' Check if "~/.bpqx-auth" exists
token_cache_exists <- function(.f = "~/.bpqx-auth") {
    file.exists(.f)
}
