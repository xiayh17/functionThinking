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
#' @param verbose Logical, whether to print detailed information to console. Default is TRUE.
#'
#' @importFrom methods slotNames showClass
#' @importFrom utils capture.output find help str
#'
#' @return Prints a comprehensive analysis of the provided object to the console, including 
#' its class, type, structure, dimensions, attributes, and additional details for 
#' special object types like S4 or R6. No return value, called for side effects.
#'
#' @export
#'
#' @examples
#' a = c(1,2,3)
#' res <- inspect_object(a)
#' 
#' # Silent mode
#' inspect_object(a, verbose = FALSE)
inspect_object <- function(x, name = deparse(substitute(x)), verbose = TRUE) {
  # Create output as a character vector
  output <- character()
  
  output <- c(output, paste(" Inspecting:", name))
  
  output <- c(output, " Basic Info")
  output <- c(output, paste("  Class     :", paste(class(x), collapse = ", ")))
  output <- c(output, paste("  Type      :", typeof(x)))
  output <- c(output, paste("  Mode      :", mode(x)))
  output <- c(output, paste("  Length    :", length(x)))
  output <- c(output, paste("  Attributes:", paste(names(attributes(x)), collapse = ", ")))
  output <- c(output, paste("  Names     :", paste(names(x), collapse = ", ")))
  output <- c(output, "")
  
  dims <- dim(x)
  if (!is.null(dims)) {
    output <- c(output, paste("  Dimensions:", paste(dims, collapse = " x ")))
    output <- c(output, "")
  }
  
  output <- c(output, " Structure")
  struct_output <- capture.output(str(x))
  output <- c(output, struct_output)
  output <- c(output, "")
  
  output <- c(output, " Preview (head)")
  preview <- try(capture.output(print(utils::head(x))), silent = TRUE)
  if (!inherits(preview, "try-error")) {
    output <- c(output, preview)
  }
  output <- c(output, "")
  
  output <- c(output, " Summary")
  summary_output <- try(capture.output(print(summary(x))), silent = TRUE)
  if (!inherits(summary_output, "try-error")) {
    output <- c(output, summary_output)
  }
  output <- c(output, "")
  
  if (isS4(x)) {
    output <- c(output, "  S4 Object Detected")
    output <- c(output, paste("  Slots     :", paste(slotNames(x), collapse = ", ")))
    output <- c(output, paste("  Class Def :", capture.output(showClass(class(x)))[1]))
    output <- c(output, "")
  }
  
  if ("R6" %in% class(x)) {
    output <- c(output, " R6 Object Detected")
    output <- c(output, paste(" Methods   :", paste(names(x), collapse = ", ")))
    output <- c(output, "")
  }
  
  output <- c(output, " Source Info")
  env <- try(environment(x), silent = TRUE)
  if (!inherits(env, "try-error") && !is.null(env)) {
    output <- c(output, paste("  Environment:", environmentName(env)))
    output <- c(output, paste("  Objects    :", paste(ls(env), collapse = ", ")))
  }
  
  found <- try(find(name), silent = TRUE)
  if (!inherits(found, "try-error") && length(found) > 0) {
    output <- c(output, paste("  Found In   :", paste(found, collapse = ", ")))
  }
  
  # Only print if verbose is TRUE
  if (verbose) {
    message(paste(output, collapse = "\n"))
  }
  
  # Return the output invisibly
  invisible(output)
}
