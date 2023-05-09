#' Retrieve Qualtrics API token
api_token <- function() {
    if (token_cache_exists()) {
        yaml::read_yaml("~/.bpqx-auth")$api_token
    } else {
        stop(
            "Cannot find token cache file at ~/.bpqx-auth\n",
            "Please run `save_api_token()`\n"
        )
    }
}

#' Retrieve Data Center
data_center <- function() {
    if (token_cache_exists()) {
        yaml::read_yaml("~/.bpqx-auth")$data_center
    } else {
        stop(
            "Cannot find token cache file at ~/.bpqx-auth\n",
            "Please run `save_api_token()`\n"
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
save_api_token <- function(data_center = "blueprintade.yul1", .t = NULL) {
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

    yaml::write_yaml(
        list(
            api_token = t,
            data_center = data_center
        ),
        "~/.bpqx-auth"
    )
}


#' Check if "~/.bpqx-auth" exists
token_cache_exists <- function() {
    file.exists("~/.bpqx-auth")
}
