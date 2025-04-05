#' Gets the current time in the given time zone.
#'
#' @description This function returns the current time in the specified
#' time zone.
#'
#' @param tz The time zone to get the current time in.
#' @return The current time in the given time zone.
#'
#'
#' @export
#'
#' @examples
#' get_current_time("America/New_York")
get_current_time <- function(tz = "UTC") {
  format(Sys.time(), tz = tz, usetz = TRUE)
}

#' Run R code from a string
#'
#' @description This function takes a string of R code, evaluates it, and
#' captures the output.
#'
#' @param code_string A string containing R code to be executed.
#'
#' @return A list containing the output and the result of the evaluated code.
#'
#' @importFrom utils capture.output
#'
#' @export
#'
#' @examples
#' res <- run_r_string("sum(1:10)")
#' cat("Input:", paste(res$output, collapse="\n"), "\n")
#' cat("Result:", res$result, "\n")
run_r_string <- function(code_string) {
  output <- capture.output(result <- eval(parse(text = code_string)))
  list(
    output = output,  # console output
    result = result   # code return value
  )
}

#' Get help text from a string
#'
#' @description This function takes a string of help text, parses it, and
#' returns the help text.
#'
#' @param help_string A string containing help text to be parsed.
#'
#' @return A string containing the help text.
#'
#' @importFrom utils help capture.output
#' @importFrom tools Rd2txt
#'
#' @export
#'
#' @examples
#' res <- get_help_text("print")
#' cat(res)
get_help_text <- function(help_string) {
  # handle the ?
  if (substr(help_string, 1, 1) == "?") {
    help_string <- substr(help_string, 2, nchar(help_string))
  }

  # handle package::function format
  if (grepl("::", help_string)) {
    parts <- strsplit(help_string, "::")[[1]]
    pkg_name <- parts[1]
    function_name <- parts[2]

    # ensure the package is loaded
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      return(paste("Package", pkg_name, "is not installed"))
    }

    # use help function correctly
    h <- eval(substitute(help(topic, package = pkg),
                       list(topic = function_name, pkg = pkg_name)))
  } else {
    h <- help(help_string)
  }

  # if found help document
  if (length(h) > 0) {
    # Use capture.output to get the text directly from help instead of
    # using .getHelpFile
    tmp <- tempfile()
    on.exit(file.remove(tmp), add = TRUE)

    # Write help output directly to file
    sink(tmp)
    print(h)
    sink()

    # Read the help text
    help_text <- paste(readLines(tmp), collapse = "\n")

    # If the above method doesn't provide full help text, try alternative
    if (nchar(help_text) < 100) {  # Assume minimal help would be longer
      help_text <- paste(capture.output(print(h, help_type = "text")),
                         collapse = "\n")
    }

    return(help_text)
  } else {
    return(paste("No help found for", help_string))
  }
}


#' Inspect an object
#'
#' @description This function takes an object and returns a detailed summary of
#'  its structure and properties.
#'
#' @param x The object to inspect.
#' @param name The name of the object.
#'
#' @importFrom methods slotNames showClass
#' @importFrom utils capture.output find help str
#'
#' @export
#'
#' @examples
#' a = c(1,2,3)
#' res <- inspect_object(a)
#' cat(res)
inspect_object <- function(x, name = deparse(substitute(x))) {
  cat(" Inspecting:", name, "\n")

  cat(" Basic Info\n")
  cat("  Class     :", paste(class(x), collapse = ", "), "\n")
  cat("  Type      :", typeof(x), "\n")
  cat("  Mode      :", mode(x), "\n")
  cat("  Length    :", length(x), "\n")
  cat("  Attributes:", paste(names(attributes(x)), collapse = ", "), "\n")
  cat("  Names     :", paste(names(x), collapse = ", "), "\n\n")

  dims <- dim(x)
  if (!is.null(dims)) {
    cat("  Dimensions:", paste(dims, collapse = " x "), "\n\n")
  }

  cat(" Structure\n")
  str(x)
  cat("\n")

  cat(" Preview (head)\n")
  try(print(utils::head(x)), silent = TRUE)
  cat("\n")

  cat(" Summary\n")
  try(print(summary(x)), silent = TRUE)
  cat("\n")

  if (isS4(x)) {
    cat("  S4 Object Detected\n")
    cat("  Slots     :", paste(slotNames(x), collapse = ", "), "\n")
    cat("  Class Def :", capture.output(showClass(class(x)))[1], "\n\n")
  }

  if ("R6" %in% class(x)) {
    cat(" R6 Object Detected\n")
    cat(" Methods   :", paste(names(x), collapse = ", "), "\n\n")
  }

  cat(" Source Info\n")
  env <- try(environment(x), silent = TRUE)
  if (!inherits(env, "try-error") && !is.null(env)) {
    cat("  Environment:", environmentName(env), "\n")
    cat("  Objects    :", paste(ls(env), collapse = ", "), "\n")
  }

  found <- try(find(name), silent = TRUE)
  if (!inherits(found, "try-error") && length(found) > 0) {
    cat("  Found In   :", paste(found, collapse = ", "), "\n")
  }
}
