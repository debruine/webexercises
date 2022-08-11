# learnr-style functions

#' LearnR-style quiz
#'
#' @param ... one or more questions (see webexercises::question)
#' @param caption Optional quiz caption
#'
#' @return html
#' @export
quiz <- function(..., caption = "Quiz") {
  questions <- paste0(c(...), collapse = "</li><li>")
  html <- sprintf("<div class='webex-quiz'><div class='webex-quiz-title'>%s</div><ol><li>%s</li></ol></div>", 
          caption, questions)
  
  cat(html)
}

#' LearnR-style question
#'
#' @param text The question text
#' @param ... Answers (see webexercises::answer)
#' @param type The input type (only single for now)
#' @param correct (not used yet)
#' @param incorrect (not used yet)
#' @param try_again (not used yet)
#' @param message (not used yet)
#' @param post_message (not used yet)
#' @param loading (not used yet)
#' @param submit_button (not used yet)
#' @param try_again_button (not used yet)
#' @param allow_retry (not used yet)
#' @param random_answer_order Whether to randomise the order of the answers
#' @param options options to pass onto webexercises widget function
#'
#' @return html for the webexercises question
#' @export
#'
#' @examples
#' question("Is R fun?",
#'          answer("Yes", TRUE),
#'          answer("No"),
#'          random_answer_order = TRUE)
question <- function(text, ..., 
                     type = c("auto", "single", "multiple", "learnr_radio", "learnr_checkbox", "learnr_text"),
                     correct = "Correct!",
                     incorrect = "Incorrect",
                     try_again = incorrect,
                     message = NULL,
                     post_message = NULL,
                     loading = c("**Loading:** ", text, "<br/><br/><br/>"),
                     submit_button = "Submit Answer",
                     try_again_button = "Try Again",
                     allow_retry = FALSE,
                     random_answer_order = FALSE,
                     options = list()
) {
  # get options, randomise, and count answers
  opts <- c(...)
  if (random_answer_order) opts <- sample(opts)
  ix <- length(which(names(opts) == "answer"))
  if (ix == 0) stop("There must be at least one correct answer")
  maxchar <- max(sapply(opts, nchar))
  
  # check/guess type
  type <- match.arg(type)
  if (type == "auto") {
    if (length(opts) == 1) {
      type <- "learnr_text"
    } else if (maxchar < 50) {
      if (ix == 1) {
        type <- "single"
      } else if (ix > 1) {
        type == "multiple"
      }
    } else {
      if (ix == 1) {
        type <- "learnr_radio"
      } else if (ix > 1) {
        type == "learnr_checkbox"
      }
    }
  }
  
  if (type == "single") {
    input <- mcq(opts)
  } else if (type == "learnr_radio") {
    input <- longmcq(opts)
  } else if (type == "learnr_text") {
    options$answer <- unname(opts)
    input <- do.call(fitb, options)
  } else {
    stop("webexercises does not implement the ", type, " question type yet")
  }
  
  sprintf("<div class='webex-question'><span class='webex-question-text'>%s</span>%s</div>", text, input)
}



#' LearnR-style answer
#'
#' @param text The answer text
#' @param correct Whether this as a/the correct answer
#' @param message (Not used)
#'
#' @return named vectot
#' @export
#'
answer <- function(text, correct = FALSE, message = NULL) {
  opt <- text
  if (correct) {
    names(opt) <- "answer"
  } else {
    names(opt) <- "x"
  }
  opt
}
