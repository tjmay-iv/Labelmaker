
#' Generate Variable and Value Labels
#'
#' @param x Data set
#' @param out File Path for R Script
#' @param max_labels Maximum number of distinct value labels to be generated for a single variable
#' @param max_cases Maximum number of distinct values within a variable for said variable to be considered
#' @param fill Should the function create labels for values that are between values from the data set but are not themselves found
#' @param fill.limits Boundaries for fill
#'
#' @return R Script
#' @export
#'
#' @examples

make_labs <- function(x, out = "", max_labels = 30, max_cases = 30, fill = F, fill.limits = c(-1,100)) {
  ### Variable Labels
  variable <- c()
  label <- c()
  for (i in 1:ncol(x)) {
    variable <- c(variable, colnames(x)[i])
    if (!is.null(labelled::var_label(x[[i]]))) {
      label <- c(label, labelled::var_label(x[[i]]))
    } else {
      label <- c(label, "VARIABLE_LABEL")
    }
  }

  x.labels <- c()
  for (i in 1:length(variable)) {
    if (i != length(variable)) {
      x.labels <- c(x.labels, paste(
        "\n\t",
        variable[i], " = ", '"',label[i], '"',",", sep = ""))
    } else {
      x.labels <- c(x.labels, paste(
        "\n\t",
        variable[i], " = ", '"',label[i], '"', sep = ""))
    }
  }

  ### Value Labels
  label.maker.syntax <- c()
  label.maker.complete <- c()
  for (i in 1:ncol(x)) {
    label.maker.variablevalues <- c()

    if (is.character(x[[i]]) & is.null(labelled::val_labels(x[[i]]))) {
      next
    } else {
      labs.values <- unique(c(unique(labelled::val_labels(x[[i]])), unique(x[[i]])))
      if (fill == T) {
        if (fill.limits[1] > fill.limits[2]) {
          stop("fill.limits must be ordered lowest value first")
        }
        labs.values.range <- range(labs.values[which(labs.values > fill.limits[1] & labs.values < fill.limits[2])])
        labs.values.fill <- labs.values.range[1]:labs.values.range[2]
        labs.values <- unique(c(labs.values, labs.values.fill))
      }
      labs.values <- labs.values[order(labs.values)]
      if (length(labs.values) >= max_cases) {
        next
      } else
        if (!all(is.na(labs.values))) {
          labs.values <- labs.values[!is.na(labs.values)]
        }
      if (length(labs.values) > max_labels) {
        labs.values <- labs.values[1:max_labels]
      }
      label.maker.name <- paste('\n\t',colnames(x[i]), " = c(", sep = "")

      for (o in 1:length(labs.values)) {
        if (o != length(labs.values)) {
          label.maker.value <- paste(
            '\n\t\t "',ifelse (!is.null(labelled::val_label(x[[i]], labs.values[o])),
                               labelled::val_label(x[[i]], labs.values[o]), "VAL_LABEL"),  '" = ',
            ifelse (is.character(labs.values[o]) , paste('"',labs.values[o],'"', sep = ""), labs.values[o]),
            ",", sep = ""
          )
        } else {
          label.maker.value <- paste(
            '\n\t\t "',ifelse (!is.null(labelled::val_label(x[[i]], labs.values[o])),
                               labelled::val_label(x[[i]], labs.values[o]), "VAL_LABEL"),  '" = ',
            ifelse (is.character(labs.values[o]) , paste('"',labs.values[o],'"', sep = ""), labs.values[o]),
            "\n\t),", sep = ""
          )
        }

        label.maker.variablevalues <- c(label.maker.variablevalues, label.maker.value)
      }
      label.maker.complete <- c(label.maker.name, label.maker.variablevalues)
      label.maker.syntax <- c(label.maker.syntax, label.maker.complete)
    }
  }
  label.maker.syntax[length(label.maker.syntax)] <- stringr::str_replace(label.maker.syntax[length(label.maker.syntax)], ",$", ")")

  cat("data <- data %>% \nset_variable_labels(", x.labels, "\n\t) %>% ",
      "\nset_value_labels(", label.maker.syntax , sep = "",
      file = out )
}


