# Personal functions intended for interactive use

#' Quit
#'
#' Quit without save prompt
#' @param save save the current session? yes/no
#' @param ... passed to `quit`
#' @return exits R
#' @export
q <- function(save = "no", ...) {
    quit(save = save, ...)
}

#' Split a vector by index
#'
#' Split a vector into sections of equal length
#' @param x vector to split.
#' @param n section length.
#' @return list of vector sections
#' @export
section <- function(x, n = 50L) {
    split(x, cut(seq_along(x), n, labels = FALSE))
}

#' Not in
#'
#' Opposite of `%in%`
#' @param x the values to be matched.
#' @param y the values to be matched against.
#' @return logical vector
#' @export
"%ni%" <- function(x, y) {
    Negate("%in%")(x, y)
}

#' Pass
#'
#' Do nothing
#' @param ... ignored
#' @return invisible
#' @export
pass <- function(...) {
    invisible()
}

#' Maybe do monad
#'
#' Use operator to evaluate functions from left to right, breaking and
#' returning NA if any function fails or returns NA.
#' @param f left hand function
#' @param g right hand function
#' @export
"%|%" <- function(f, g) {
    maybe <- function(x) {
        if (class(x) == "try-error") NA else x
    }

    x = try(eval(f), silent = TRUE)
    x = maybe(x)

    if (is.na(x))
        return(NA)

    g = match.call(expand.dots = FALSE)$g

    if (length(g) == 4 & class(g) == "call") {
        # anonymous function

        y = try(eval(g)(x), silent = TRUE)
        ret = maybe(y)
    } else if (length(g) == 1 & class(g) == "name") {
        # curried function

        y = try(eval(g)(x), silent = TRUE)
        ret = maybe(y)
    } else {
        # length(g) == 2 && class(g) == "call"
        # function with additional parameters
        # e.g. sum(10)

        parsed = as.list(g)
        parsed = append(parsed, x, after = 1)
        # add x as the first argument to g
        g = as.call(parsed)
        y = try(eval(g), silent = TRUE)
        ret = maybe(y)
    }

    return(ret)
}

#' Remove rows with NA
#'
#' Remove all rows that have on or more NA values
#' @param x data.frame to be cleaned
#' @return data.frame with zero or more rows removed
#' @export
rm_na_rows <- function(x) {
    x[!rowSums(is.na(x)), ]
}

#' Check `x` is a valid value
#'
#' Similar to the `not` keyword in python. not(NA) will evaluate to FALSE,
#' whereas not(NULL) evaluates to TRUE.
#' @param x value
#' @return logical
#' @export
not <- function(x) UseMethod('not')

not.default <- function(x) {
    !length(x)
}

not.data.frame <- function(x) {
    !nrow(x)
}

#' Check if `x` is null, NA or NaN
#'
#' Check for undesirable elements
#' @param x object
#' @return logical vector
#' @export
is.mal <- function(x) {
    null <- is.null(x)
    x[null] <- TRUE
    x[!null] <- is.na(x[!null]) | is.nan(x[!null])
    x
}

#' Does `x` have names?
#'
#' Check if `x` is named
#' @param x object that supports `names` method
#' @return logical
#' @export
named <- function(x) {
    if (length(names(x))) TRUE else FALSE
}

#' Length
#'
#' Length of `x` if `x` is a vector, number of characters if `x` is a string
#' @param x vector
#' @return integer indicating the number of characters in x if x is a
#' single-element character vector, otherwise the number of elements in x
#' @export
len <- function(x) {
    if (is.character(x) && length(x) == 1)
        nchar(x)
    else
        length(x)
}

#' Last `n` elements in `x`
#'
#' Last `n` elements in `x`
#' @param x list or vector
#' @param n number of elements to take from the end
#' @return last n elements in x
#' @export
last <- function(x, n = 1L) {
    tail(x, n)
}

#' Plot kill
#'
#' Shut down all open plots
#' @return invisible
#' @export
pk <- function() {
    while (TRUE) {
        isplot <- try({ dev.off() }, silent = TRUE)
        if (is.null(isplot) || class(isplot) == 'try-error') break
    }

    invisible()
}

#' Inspect global namespace or package namespace
#'
#' If called without an argument, return the names in the current scope.
#' If called with a package name, return the names in the package scope.
#' @param package package name to inspect
#' @return vector of function and method names in the namespace
#' @export
dir <- function(package) {
    if (missing(package)) {
        ls(envir = .GlobalEnv)
    } else {
        package <- as.character(substitute(package))
        ls(paste0("package:", package))
    }
}

#' Change factors to characters
#'
#' Change factors to characters
#'
#' @param x object to change factors into characters in
#' @export
#' @examples
#' df <- data.frame(a = letters[1:5], x = 1:5, y = LETTERS[1:5], stringsAsFactors = TRUE)
#' str(df)
#' df <- unfactor(df)
#' str(df)
# Modified from:
# https://github.com/Dasonk/Dmisc/blob/master/R/unfactor.R
unfactor <- function(x){
    id <- vapply(x, is.factor, logical(1))
    x[id] <- lapply(x[id], as.character)
    x
}

#' Rename object names
#'
#' Match and rename object names with new names.
#' @param x object to rename
#' @param old names to match on
#' @param newn new names, matched to `old` by position
#' @param value list with `old` and `new` containing the names to match on and
#' names to be matched to, by position
#' @examples
#' x <- data.frame(foo = 1:3, bar = letters[1:3])
#' names(x) <- rename(x, "bar", "baz")
#' names(x)
#' # [1] "foo" "baz"
#' rename(x) <- list(old = "baz", new = "bar")
#' names(x)
#' # [1] "foo" "bar"
#' @export
rename <- function(x, old, newn) {
    n <- names(x)
    n[ match(old, n) ] <- newn
    return(n)
}

#' @rdname rename
#' @export
"rename<-" <- function(x, value) {
    old <- value$old
    newn <- value$new
    names(x) <- rename(x, old, newn)
    return(x)
}

