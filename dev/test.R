library(ellmer)
library(functionThinking)

chat <- chat_deepseek(
  base_url = Sys.getenv("base_url"),
  api_key = Sys.getenv("api_key"),
  model = Sys.getenv("model"),
  seed = NULL,
  api_args = list(),
  echo = NULL
)

fun.thinking(
  (
    a = mean("一到十")
   ),
  chat = chat)

fun.thinking(
  (
    "use ggplot2 package to make visulaztion of `mtcars` dataset."
  ),
  chat = chat)


fun.thinking(
  (
    "画出mtcars数据中，马力与油耗的关系图"
  ),
  chat = chat
)
