#' rlookup
#'
#' Recoding variables in a data frame through a value map
#'
#' @name rlookup-package
#' @docType package
"_PACKAGE"


#' Initialize a value map from a data frame
#'
#'
#' @param .data a data frame
#' @param ... column names, tidy
#' @import dplyr purrr
#' @export
make_lookup <- function(.data, ...){
        dots <- quos(...)
        unique_values <-   .data %>%
                select(!!!dots) %>%
                map(unique)
        namez <- names(unique_values) # colnames

        lookup <- data.frame()
        for (i in seq_along(unique_values)) {
                single <- data.frame(namez[i], unique_values[i], NA)
                names(single) <- c("col_name", "old_value", "new_value")
                lookup <- rbind(lookup, single)
        }

        return(lookup %>% transmute_all(as.character) %>% as_tibble())
}


#' Write value map to a file
#'
#' @param lookup a value map
#' @param path a character string
#' @param overwrite overwriting existing file from \code{path}
#' @param edit invoke \code{edit_lookup} before writing to file
#' @import dplyr readr
#' @export
write_lookup <- function(lookup, path, overwrite = FALSE, edit = FALSE) {
        its_there <- file.exists(path)

        if (its_there & overwrite == FALSE) { # join file with new lookup
                lookup0 <- read_lookup(path)
                lookup <- lookup %>%
                        select(-new_value) %>%
                        left_join(lookup0, by = c("col_name", "old_value"))
        }

        if (edit) {
                edit_lookup(lookup)
        }


        write_csv(lookup, path, na = "", append = FALSE)
        cat("Written at ", path)
}

#' Read value map from file
#'
#' @param path a path pointing to a value map
#' @import readr
#' @export
read_lookup <- function(path){suppressMessages({
        read_csv(path, col_names = TRUE, col_types = cols(.default = "c"))
})}


#' Edit value map
#'
#' @param lookup a value map, or a path pointing to a file containing such
#' @param quiet commit the modification without confirmation?
#' @import dplyr readr utils
#' @export
edit_lookup <- function(lookup, quiet = FALSE) {

        if (is.character(lookup)) { # read from file, if so deviced
                path <- lookup
                lookup <- read_lookup(path)
                message(cat("Editing a file...\n"))
        }

        edited_lookup <- edit(lookup)
        made_changes <- !isTRUE(all_equal(lookup, edited_lookup))

        if (made_changes) { # accept or discard changes
                if (quiet) {
                        lookup <- edited_lookup
                } else if (!quiet) {
                        user_selection <- menu(c("Accept", "Discard"), title = "You've made changes:")
                        if (user_selection == 1) {
                                lookup <- edited_lookup
                        } else {
                                made_changes <- FALSE
                        }
                }
        }

        lookup <- as_tibble(lookup)

        if (exists("path")) { # write to file, if so deviced
                if (made_changes) {
                        write_csv(lookup, path, na = "", append = FALSE)
                        message(cat("\nChanges saved at ", path, "\n"))
                }
        }

        invisible(lookup)
}

#' Use value map to replace values in a data frame
#'
#' @param .data a data frame
#' @param lookup a value map, or a path pointing to a file containing such
#' @param mark out of old and new, mark which column name? choose from: c("old", "new", "both")
#' @param drop_old keep only the new column after recoding
#' @import dplyr
#' @export
use_lookup <- function(.data, lookup, mark = c("old", "new", "both"), drop_old = TRUE) {
        if (is.character(lookup)) { # read from file, if so deviced
                path <- lookup
                lookup <- read_lookup(path)
                cat("Using a file...\n")
        }

        if (all(mark == c("old", "new", "both"))) {    # if mark is not specifid, skip mark
                mark <- ""
        }

        if (any(mark %in% c("old", "new", "both"))) { # if mark is specified, skip drop old
                drop_old <- FALSE
        }

        var_names <- unique(lookup$col_name)

        res <- .data %>% mutate_at(vars(var_names), .funs = as.character) # coerce target vars to character

        for (var_name in var_names) {  # loop over variables, join new coding to data
                sub_lookup <- lookup[lookup$col_name == var_name, c(2,3)]

                var_name_old <- paste0(var_name, "_old_")
                var_name_new <- paste0(var_name, "_new_")

                names(sub_lookup) <- c(var_name,
                                      var_name_new)

                res <- res %>% left_join(sub_lookup, by = var_name)

                if (mark == "both") {
                        names(res)[names(res) == var_name] <- var_name_old
                } else if (mark == "old") {
                        names(res)[names(res) == var_name] <- var_name_old
                        names(res)[names(res) == var_name_new] <- var_name
                }

                if (drop_old) {
                        res[var_name] <- NULL
                        names(res)[names(res) == var_name_new] <- var_name
                }
        }

        as_tibble(res)

}


#' Batch modify a value map
#'
#' @param lookup a value map
#' @param .fun a function to be applied to old_value
#' @param ... a vector containing col_name
#' @import dplyr readr
#' @export
modify_lookup <- function(lookup, .fun, ...){
        if (is.character(lookup)) { # read from file, if so deviced
                path <- lookup
                lookup <- read_lookup(path)
                message(cat("Modifying a file...\n"))
        }

        dots <- paste(match.call(expand.dots = FALSE)$...)
        if (length(dots) == 0) dots <- unique(lookup$col_name)

        fun <- as_mapper(.fun)

        sub_lookup <- lookup %>%
                filter(col_name %in% dots) %>%
                mutate(new_value = fun(old_value))

        lookup <- lookup %>%
                anti_join(sub_lookup, by = c("col_name", "old_value")) %>%
                bind_rows(sub_lookup)


        if (exists("path")) { # write to file, if so deviced
                write_csv(lookup, path, na = "", append = FALSE)
        }
        invisible(lookup)
}

