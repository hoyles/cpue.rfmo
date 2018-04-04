#' Sort unique.
#'
#' A helper function sort(unique()).
#' @param x Input vector.
#' @return Sorted unique values.
#'
su <- function(x) sort(unique(x))

#' Calulate remainder.
#'
#' A helper function to calculate the remainder, x - floor(x).
#' @param x Input vector.
#' @return Remainder.
#'
revtrunc <- function(x) {
    x - floor(x)
}


#' Calulate mode of a vector.
#'
#' A helper function to calculate the mode (most common value) of a vector.
#' @param x Input vector.
#' @return Mode.
#'
Mode <- function(x) {
    # from http://stackoverflow.com/questions/2547402/standard - library - function - in - r - for - finding - the - mode
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

#' Calulate number of unique values in a vector.
#'
#' A helper function to calulate the number of unique values in a vector, length(unique()).
#' @param x Input vector.
#' @return Number of unique values.
#'
dimu <- function(x) length(unique(x))

#' Calulate number of unique values in a vector.
#'
#' A helper function to calulate the number of unique values in a vector, length(unique()).
#' @param x Input vector.
#' @return Number of unique values.
#'
lu <- function(x) length(unique(x))

#' Turn a factor into a numeric.
#'
#' A helper function to turn a vector of factors into numeric variables.
#' @param x Input vector.
#' @return Numeric vector.
#'
defactor <- function(x) as.numeric(as.character(x))

#' Calulate number of unique values in a vector.
#'
#' A helper function to calulate the number of unique values in a vector, length(unique()).
#' @param x Input vector.
#' @return Number of unique values.
#'
lu <- function(x) length(unique(x))

#' Expand a grid across a data frame.
#'
#' @return Expanded grid.
#'
expand.grid.df <- function(...) Reduce(function(...) merge(..., by = NULL), list(...))


#' count proportion of zeroes in a vector.
#'
#' A helper function to calulate the proportion of zero values in a vector.
#' @param a Input vector.
#' @return Proportion of zeroes.
#'
countzero <- function(a) {
    z <- sum(a == 0)
    tot <- length(a)
    pz <- z/tot
    return(pz)
}

#' List objects in the workspace and their characteristics.
#'
#' List objects in the workspace and their characteristics.
#' @param pos Search level.
#' @param pattern Pattern.
#' @param order.by Sort the output by this characteristic.
#' @param decreasing Sort descending if TRUE.
#' @param head Report the first n objects if head is TRUE.
#' @param n Report the first n objects if head is TRUE.
#' @return Output to console.
#'
ls.objects <- function(pos = 1, pattern, order.by, decreasing = FALSE, head = FALSE, n = 5) {
    napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing = decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

#' List objects in the workspace and their characteristics.
#'
#' List objects in the workspace and their characteristics.
#' @param order.by Sort the output by this characteristic.
#' @param decreasing Sort descending if TRUE.
#' @param head Report the first n objects if head is TRUE.
#' @param n Report the first n objects if head is TRUE.
#' @return Output to console.
#'
lsos <- function(..., n = 10) {
    ls.objects(..., order.by = "Size", decreasing = TRUE, head = TRUE, n = n)
}

