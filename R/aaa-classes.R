#'An S4 class to represent l1tf output
#'
#'@slot summary.df A numeric dataframe
#'@slot series.df A numeric dataframe
#'@slot prop.na A length-one numeric vector
#'
#'@export
setClass("l1tf_obj",
        slots = list(summary.df = "data.frame",
                    series.df = "data.frame",
                    prop.na = "numeric"))

