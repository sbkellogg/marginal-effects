## ----echo = FALSE, message = FALSE, warning = FALSE---------------------------
library("data.table")
library("tinytable")

dat <- "https://raw.githubusercontent.com/vincentarelbundock/marginaleffects/main/data-raw/supported_models.csv"
##dat <- "data-raw/supported_models.csv"
supported_models <- fread(dat, colClasses = rep("character", 10))

tmp <- supported_models
for (i in nrow(tmp):2) {
    if (tmp$Package[i] == tmp$Package[i - 1]) {
        tmp$Package[i] <- ""
    }
}

colnames(tmp) <- c("Package",
                   "Function",
                   "dY/dX",
                   "SE",
                   "dY/dX ",
                   "SE ",
                   "dY/dX  ",
                   "SE  ",
                   "dY/dX   ",
                   "SE   ")

## remove "supported" columns which feel useless since everything is supported
tmp[[3]] <- tmp[[4]] <- NULL

## colors & icons
f <- function(x) fcase(x == TRUE, "green", x == FALSE, "red", default = "gray")
idx <- lapply(tmp, f)

for (i in 3:ncol(tmp)) {
    tmp[[i]] <- as.character(tmp[[i]])
    tmp[[i]] <- fcase(tmp[[i]] == "TRUE", "✓",
                      tmp[[i]] == "FALSE", "✖",
                      tmp[[i]] == "U", "U",
                      default = "")
}


## ----echo = FALSE, message = FALSE, warning = FALSE---------------------------
library(tinytable)

tab <- tt(tmp)
tab <- style_tt(tab, j = 1:2, align = "l")
tab <- style_tt(tab, j = 3:8, align = "c")
# for (j in 3:ncol(tmp)) {
#     tab <- style_tt(tab, j = i, color = idx[[i]])
# }
tab <- group_tt(tab, j = list("Supported by marginaleffects" = 1:2, "Stata" = 3:4, "margins" = 5:6, "emtrends" = 7:8))
tab <- group_tt(tab, j = list(" " = 1:2, "Numerical equivalence" = 3:8), level = 2)
tab

