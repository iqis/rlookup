#' jollyrogr
#'
#' Recoding variables in a data frame through a value map
#'
#' @name jollyrogr-package
#' @docType package
"_PACKAGE"


#' Initialize a value map from a data frame
#'
#'
#' @param .data a data frame
#' @param ... column names, tidy
#' @import dplyr purrr
#' @export
cover <- function(.data, ...){
        dots <- quos(...)
        unique_values <-   .data %>%
                select(!!!dots) %>%
                map(unique)
        namez <- names(unique_values) # colnames

        cover <- data.frame()
        for (i in seq_along(unique_values)) {
                single <- data.frame(namez[i], unique_values[i], NA)
                names(single) <- c("col_name", "old_value", "new_value")
                cover <- rbind(cover, single)
        }

        return(cover %>% transmute_all(as.character) %>% as_tibble())
}


#' Write value map to a file
#'
#' @param cover a value map
#' @param path a character string
#' @param overwrite overwriting existing file from \code{path}
#' @param edit invoke \code{edit_cover} before writing to file
#' @import dplyr readr
#' @export
write_cover <- function(cover, path, overwrite = FALSE, edit = FALSE) {
        its_there <- file.exists(path)

        if (its_there & overwrite == FALSE) { # join file with new cover
                cover0 <- read_cover(path)
                cover <- cover %>%
                        select(-new_value) %>%
                        left_join(cover0, by = c("col_name", "old_value"))
        }

        if (edit) {
                edit_cover(cover)
        }


        write_csv(cover, path, na = "", append = FALSE)
        cat("Written at ", path)
}

#' Read value map from file
#'
#' @param path a path pointing to a value map
#' @import readr
#' @export
read_cover <- function(path){suppressMessages({
        read_csv(path, col_names = TRUE, col_types = cols(.default = "c"))
})}


#' Edit value map
#'
#' @param cover a value map, or a path pointing to a file containing such
#' @param quiet commit the modification without confirmation?
#' @import dplyr readr utils
#' @export
edit_cover <- function(cover, quiet = FALSE) {

        if (is.character(cover)) { # read from file, if so deviced
                path <- cover
                cover <- read_cover(path)
                message(cat("Editing a file...\n"))
        }

        edited_cover <- edit(cover)
        made_changes <- !isTRUE(all_equal(cover, edited_cover))

        if (made_changes) { # accept or discard changes
                if (quiet) {
                        cover <- edited_cover
                } else if (!quiet) {
                        user_selection <- menu(c("Accept", "Discard"), title = "You've made changes:")
                        if (user_selection == 1) {
                                cover <- edited_cover
                        } else {
                                made_changes <- FALSE
                        }
                }
        }

        cover <- as_tibble(cover)

        if (exists("path")) { # write to file, if so deviced
                if (made_changes) {
                        write_csv(cover, path, na = "", append = FALSE)
                        message(cat("\nChanges saved at ", path, "\n"))
                }
        }

        invisible(cover)
}

#' Use value map to replace values in a data frame
#'
#' @param .data a data frame
#' @param cover a value map, or a path pointing to a file containing such
#' @param mark out of old and new, mark which column name? choose from: c("old", "new", "both")
#' @param drop_old keep only the new column after recoding
#' @import dplyr
#' @export
use_cover <- function(.data, cover, mark = c("old", "new", "both"), drop_old = TRUE) {
        if (is.character(cover)) { # read from file, if so deviced
                path <- cover
                cover <- read_cover(path)
                cat("Using a file...\n")
        }

        if (all(mark == c("old", "new", "both"))) {    # if mark is not specifid, skip mark
                mark <- ""
        }

        if (any(mark %in% c("old", "new", "both"))) { # if mark is specified, skip drop old
                drop_old <- FALSE
        }

        var_names <- unique(cover$col_name)

        res <- .data %>% mutate_at(vars(var_names), .funs = as.character) # coerce target vars to character

        for (var_name in var_names) {  # loop over variables, join new coding to data
                sub_cover <- cover[cover$col_name == var_name, c(2,3)]

                var_name_old <- paste0(var_name, "_old_")
                var_name_new <- paste0(var_name, "_new_")

                names(sub_cover) <- c(var_name,
                                      var_name_new)

                res <- res %>% left_join(sub_cover, by = var_name)

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
#' @param cover a value map
#' @param .fun a function to be applied to old_value
#' @param ... a vector containing col_name
#' @import dplyr readr
#' @export
modify_cover <- function(cover, .fun, ...){
        if (is.character(cover)) { # read from file, if so deviced
                path <- cover
                cover <- read_cover(path)
                message(cat("Modifying a file...\n"))
        }

        dots <- paste(match.call(expand.dots = FALSE)$...)
        if (length(dots) == 0) dots <- unique(cover$col_name)

        fun <- as_mapper(.fun)

        sub_cover <- cover %>%
                filter(col_name %in% dots) %>%
                mutate(new_value = fun(old_value))

        cover <- cover %>%
                anti_join(sub_cover, by = c("col_name", "old_value")) %>%
                bind_rows(sub_cover)


        if (exists("path")) { # write to file, if so deviced
                write_csv(cover, path, na = "", append = FALSE)
        }
        invisible(cover)
}

