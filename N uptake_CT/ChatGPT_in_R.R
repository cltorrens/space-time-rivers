##### ChatGPT in R

# Install packages
# install.packages(c("httr", "stringr"))

# Load Libraries
library(httr)
library(stringr)

# save my API as an R object
chatGPT_API <- "add mine here"

# FUNCTION to ask a question to ChatGPT AND save and clean the answer
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo-0301",
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}

