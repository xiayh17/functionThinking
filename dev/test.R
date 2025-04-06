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
