#' A function make function think
#'
#' @description
#' This function captures and evaluates an R expression while providing
#' analysis and insights about the function call, its arguments, and results
#' through an AI assistant (DeepSeek chat API).
#'
#' @param expr An unevaluated R expression, typically a function call.
#' @param chat A chat object for AI interaction. If NULL, a warning is shown and
#'  no analysis is performed.
#'             Use the \code{create_fun_chat()} function to create a proper chat
#'              object.
#' @param system_prompt A string of system prompt for the AI assistant.
#'                      If NULL (default), a default will be set.
#' @param more_require A string of more requirements for the AI assistant if
#'                     need, default NULL.
#'
#' @details
#' The function performs several analysis steps:
#' \itemize{
#'   \item Captures the unevaluated expression
#'   \item Extracts function name and arguments (if applicable)
#'   \item Uses AI to analyze the function call
#'   \item Checks the function and its parameters
#'   \item Examines the result of the expression
#' }
#'
#' The AI assistant can save analysis results in various formats (csv, json,
#' rds, png, pdf)
#' based on the task requirements.
#'
#' @return Invisibly returns the result of evaluating the expression.
#'
#' @import ellmer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First create a chat object
#' my_chat <- ellmer::chat_openai()
#'
#' # Analyze a function call with the chat object
#' fun.thinking(mean(1:10), chat = my_chat)
#'
#' # Analyze with custom chat settings
#' custom_chat <- create_fun_chat(
#'   system_prompt = "Custom analysis instructions",
#'   model = "alternative-model"
#' )
#' fun.thinking(mean(1:10), chat = custom_chat)
#'
#' # Analyze an object
#' x <- data.frame(a = 1:5, b = letters[1:5])
#' fun.thinking(x, chat = my_chat)
#' }
#'
fun.thinking <- function(expr, chat = NULL, system_prompt = NULL,
                         more_require = NULL) {

  # Use default system prompt if none provided
  if (is.null(system_prompt)) {
    system_prompt <- 'you are the wisdom brain inner a function, need to think
                     about the function and variables, please check all the
                     elements, will automatically check the function name,
                     arguments, and run the correct version code.
                     make things right, you will never include the R class in
                     your answer,
                     you can save everything in rds file, and return the file
                     path,
                     you can choose proper file format, like csv, json, rds,
    png, pdf by task purpose. you can storage the code in script file and
    R class in Global Environment using proper variable name if possible run
    the corrected code. you dont need to ask for the behavior, just do what you
    need to do.'
    chat$set_system_prompt(system_prompt)
  }

  if (!is.null(more_require)) {
    original_pro <- chat$get_system_prompt()
    chat$set_system_prompt(paste0(original_pro))
  }

  # Register tools with the chat object
  # Time
  chat$register_tool(tool(
    get_current_time,
    "Gets the current time in the given time zone.",
    tz = type_string(
      "The time zone to get the current time in. Defaults to `\"UTC\"`.",
      required = FALSE
    )
  ))

  # coding
  chat$register_tool(tool(
    run_r_string,
    "Run R code from a string.",
    code_string = type_string(
      "A string containing R code to be executed.",
      required = TRUE
    )
  ))

  # documents
  chat$register_tool(tool(
    get_help_text,
    "Get help information for a function or package.",
    help_string = type_string(
      "The name of the function or package to get help for. Can be in the form `package:function`.",
      required = TRUE
    )
  ))

  # object
  chat$register_tool(tool(
    inspect_object,
    "Inspect an object and return its structure.",
    object = type_string(
      "The object to inspect. Can be any R object.",
      required = TRUE
    )
  ))


  # Capture the unevaluated expression
  call_expr <- substitute(expr)

  # Get function name and arguments
  if(is.call(call_expr)) {
    fun_name <- deparse(call_expr[[1]])
    arg_values <- as.list(call_expr)[-1]
    arg_names <- names(arg_values)

    # Construct description of arguments and function
    args_desc <- paste(mapply(function(n, v) {
      if(n == "") {
        deparse(v)
      } else {
        paste0(n, " = ", deparse(v))
      }
    }, arg_names, arg_values), collapse = ", ")

    # Evaluate the expression to get the result
    result <- eval(call_expr, parent.frame())
  } else {
    # If it's a single variable or already a function object
    fun_name <- "Inspect Object"
    args_desc <- deparse(call_expr)
    result <- eval(call_expr, parent.frame())
  }

  # If no chat object is provided, issue a warning
  if (is.null(chat)) {
    warning("No chat object provided. Please create one using create_fun_chat()
             function from the ellmer package.")
    return(invisible(result))
  }

  ## Build message for AI analysis
  msg_parts <- character()
  
  ## Check the expression
  msg_parts <- c(msg_parts, paste0("Here is the function call: ", deparse(call_expr)))

  ## Check functions if applicable
  if(is.call(call_expr)) {
    msg_parts <- c(msg_parts, paste0("Here is the function: ", fun_name))
    ## Check arguments
    msg_parts <- c(msg_parts, paste0("Here is arguments: ", args_desc))
  }

  ## Check results
  msg_parts <- c(msg_parts, paste0("Here is result: ", deparse(substitute(result))))
  
  ## Combine message parts with proper line breaks
  final_message <- paste(msg_parts, collapse = "\n")
  
  ## Send to AI assistant
  chat$chat(final_message, "run the corrected code if possible, and save the result")

  # Return the result
  invisible(result)

}
