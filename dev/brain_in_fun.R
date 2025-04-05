library(ellmer)

#' Gets the current time in the given time zone.
#'
#' @description This function returns the current time in the specified time zone.
#'
#' @param tz The time zone to get the current time in.
#' @return The current time in the given time zone.
get_current_time <- function(tz = "UTC") {
  format(Sys.time(), tz = tz, usetz = TRUE)
}

#' Run R code from a string
#'
#' @description This function takes a string of R code, evaluates it, and captures the output.
#'
#' @param code_string A string containing R code to be executed.
#'
#' @return A list containing the output and the result of the evaluated code.
run_r_string <- function(code_string) {
  output <- capture.output(result <- eval(parse(text = code_string)))
  list(
    output = output,  # 控制台输出
    result = result   # 代码返回值
  )
}

# 示例
# res <- run_r_string("sum(1:10)")
# cat("输出:", paste(res$output, collapse="\n"), "\n")
# cat("结果:", res$result, "\n")

get_help_text <- function(help_string) {
  # 处理带问号的情况
  if (substr(help_string, 1, 1) == "?") {
    help_string <- substr(help_string, 2, nchar(help_string))
  }

  # 处理 package::function 格式
  if (grepl("::", help_string)) {
    parts <- strsplit(help_string, "::")[[1]]
    pkg_name <- parts[1]
    function_name <- parts[2]

    # 确保包已加载
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      return(paste("Package", pkg_name, "is not installed"))
    }

    # 正确使用 help 函数
    h <- eval(substitute(help(topic, package = pkg),
                       list(topic = function_name, pkg = pkg_name)))
  } else {
    h <- help(help_string)
  }

  # 如果找到帮助文档
  if (length(h) > 0) {
    # 解析为 Rd 对象
    rd <- utils:::.getHelpFile(h)

    # 转换为文本
    tmp <- tempfile()
    tools::Rd2txt(rd, out = tmp)
    help_text <- paste(readLines(tmp), collapse = "\n")
    file.remove(tmp)

    return(help_text)
  } else {
    return(paste("No help found for", help_string))
  }
}

# Example usage
# result <- get_help_text("chat_deepseek")
# cat(result)

inspect_object <- function(x, name = deparse(substitute(x))) {
  cat("🔍 Inspecting:", name, "\n")
  cat("────────────────────────────────────────────\n")

  cat("🧠 Basic Info\n")
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

  cat("🔍 Structure\n")
  str(x)
  cat("\n")

  cat("🧪 Preview (head)\n")
  try(print(utils::head(x)), silent = TRUE)
  cat("\n")

  cat("📊 Summary\n")
  try(print(summary(x)), silent = TRUE)
  cat("\n")

  if (isS4(x)) {
    cat("🏗️  S4 Object Detected\n")
    cat("  Slots     :", paste(slotNames(x), collapse = ", "), "\n")
    cat("  Class Def :", capture.output(showClass(class(x)))[1], "\n\n")
  }

  if ("R6" %in% class(x)) {
    cat("🔧 R6 Object Detected\n")
    cat("  Methods   :", paste(names(x), collapse = ", "), "\n\n")
  }

  cat("📦 Source Info\n")
  env <- try(environment(x), silent = TRUE)
  if (!inherits(env, "try-error") && !is.null(env)) {
    cat("  Environment:", environmentName(env), "\n")
    cat("  Objects    :", paste(ls(env), collapse = ", "), "\n")
  }

  # 尝试查找所在位置
  found <- try(find(name), silent = TRUE)
  if (!inherits(found, "try-error") && length(found) > 0) {
    cat("  Found In   :", paste(found, collapse = ", "), "\n")
  }

  cat("────────────────────────────────────────────\n\n")
}

# inspect_object(lm)
# inspect_object(mtcars)
# inspect_object(lm(mpg ~ wt, data = mtcars))


chat <- chat_deepseek(
  system_prompt = 'you are the wisdom barin inner a function, need to think
                   about the function and variables, please check all the elements,
                   make thinks right, you will never inclue the R class in your answer,
                   you can save everything in rds file, and return the file path,
                   you can choose proper file format, like csv, json, rds, pnd, pdf by task purpose
                   you can storage the code in script and R class in Global Environment using proper variable name.',
  base_url = Sys.getenv("base_url"),
  api_key = Sys.getenv("api_key"),
  model = Sys.getenv("model"),
  seed = NULL,
  api_args = list(),
  echo = NULL
)

chat$register_tool(tool(
  get_current_time,
  "Gets the current time in the given time zone.",
  tz = type_string(
    "The time zone to get the current time in. Defaults to `\"UTC\"`.",
    required = FALSE
  )
))

chat$register_tool(tool(
  run_r_string,
  "Run R code from a string.",
  code_string = type_string(
    "A string containing R code to be executed.",
    required = TRUE
  )
))

chat$register_tool(tool(
  get_help_text,
  "Get help information for a function or package.",
  help_string = type_string(
    "The name of the function or package to get help for. Can be in the form `package:function`.",
    required = TRUE
  )
))

chat$register_tool(tool(
  inspect_object,
  "Inspect an object and return its structure.",
  object = type_string(
    "The object to inspect. Can be any R object.",
    required = TRUE
  )
))

chat$chat("use ggplot2 package to make visulaztion of `mtcars` dataset.")

chat$chat("add some colorful elements to the plot, like color, shape, size, etc.
          and save the plot as a pdf file, please check the file path and return it.")

chat$chat("more plots, like scatter plot, line plot, bar plot, etc.
          and save the plots as a png file, please check the file path and return it. also you can storage the code and R class in Global Environment.")


fun_thinking <- function(expr) {
  # 捕获未求值的表达式
  call_expr <- substitute(expr)

  # 获取函数名和参数
  if(is.call(call_expr)) {
    fun_name <- deparse(call_expr[[1]])
    arg_values <- as.list(call_expr)[-1]
    arg_names <- names(arg_values)

    # 构建参数和函数的描述
    args_desc <- paste(mapply(function(n, v) {
      if(n == "") {
        deparse(v)
      } else {
        paste0(n, " = ", deparse(v))
      }
    }, arg_names, arg_values), collapse = ", ")

    # 求值表达式获取结果
    result <- eval(call_expr, parent.frame())
  } else {
    # 如果是单个变量或已经是函数对象
    fun_name <- "查看对象"
    args_desc <- deparse(call_expr)
    result <- eval(call_expr, parent.frame())
  }

  ## chat
  chat <- chat_deepseek(
    system_prompt = 'you are the wisdom brain inner a function, need to think
                     about the function and variables, please check all the elements,
                     make things right, you will never include the R class in your answer,
                     you can save everything in rds file, and return the file path,
                     you can choose proper file format, like csv, json, rds, png, pdf by task purpose',
    base_url = Sys.getenv("base_url"),
    api_key = Sys.getenv("api_key"),
    model = Sys.getenv("model"),
    seed = NULL,
    api_args = list(),
    echo = NULL
  )

  ## check the expression
  chat$chat(paste0("分析函数调用: ", deparse(call_expr)))

  ## check functions if applicable
  if(is.call(call_expr)) {
    chat$chat(paste0("检查函数: ", fun_name))

    ## check arguments
    chat$chat(paste0("检查参数: ", args_desc))
  }

  ## check results
  chat$chat(paste0("检查结果: ", deparse(substitute(result))))

  # 返回结果
  invisible(result)
}

# Example usage
# 旧方式
# fun.thinking(mtcars, mean)

# 新方式
fun.thinking(mean(mtcars$mpg))
fun.thinking(lm(mpg ~ wt + hp, data = mtcars))
fun.thinking(ggplot(mtcars) + geom_point(aes(x = wt, y = mpg)))
