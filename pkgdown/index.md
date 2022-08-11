# webexercises

<!-- rmarkdown v1 -->

<img src="logo.png" style="float:right; max-width:280px; width: 25%;" />




The goal of `{webexercises}` is to enable instructors to easily create interactive web pages that students can use in self-guided learning. Although `{webexercises}` has fewer features than RStudio's [learnr](https://rstudio.github.io/learnr/) package, it is more lightweight: whereas `{learnr}` tutorials must be either hosted on a shiny server or run locally, `{webexercises}` creates standalone HTML files that require only a JavaScript-enabled browser. It is also extremely simple to use.

## Installation

You can install `{webexercises}` from CRAN using:


```r
install.packages("webexercises")
```

You can install the development version from [GitHub](https://github.com/PsyTeachR/webexercises) with:


```r
devtools::install_github("psyteachr/webexercises")
```

## Creating interactive widgets with inline code

The webexercises package provides functions that create HTML widgets using [inline R code](https://github.com/rstudio/cheatsheets/raw/master/rmarkdown-2.0.pdf).  These functions are:

| function                | widget         | description                    |
|:------------------------|:---------------|:-------------------------------|
| `fitb()`                | text box       | fill-in-the-blank question     |
| `mcq()`                 | pull-down menu | multiple choice question       |
| `torf()`                | pull-down menu | TRUE or FALSE question         |
| `longmcq()`             | radio buttons  | MCQs with long answers         |
| `hide()` and `unhide()` | button         | solution revealed when clicked |
| `total_correct()`       | text           | updating total correct         |

The appearance of the text box and pull-down menu widgets changes when users enter the correct answer. Answers can be either static or dynamic (i.e., specified using R code). Widget styles can be changed using `style_widgets()`.

Examples are provided in the **Web Exercises** R Markdown template. To create a file from the webexercises template in RStudio, click `File -> New File... -> RMarkdown` and in the dialog box that appears, select `From Template` and choose `Web Exercises`.

Alternatively (or if you're not using RStudio) use:


```r
rmarkdown::draft("exercises.Rmd", "webexercises", "webexercises")
```

Knit the file to HTML to see how it works. **Note: The widgets only function in a JavaScript-enabled browser.**

These functions are optimised to be used with inline r code, but you can also use them in code chunks by setting the chunk option `results = 'asis'` and using `cat()` to display the result of the widget. 


```r
# echo = FALSE, results = 'asis'
opts <- c("install.package", 
            "install.packages", 
            answer = "library", 
            "libraries")

q1 <- mcq(opts)

cat("What function loads a package that is already on your computer?", q1)
```

What function loads a package that is already on your computer? <select class='webex-select'><option value='blank'></option><option value=''>install.package</option><option value=''>install.packages</option><option value='answer'>library</option><option value=''>libraries</option></select>


### Total Correct 

The function `total_correct()` displays a running total of correct responses. Change the `elem` argument to display in a different style (e.g., `h2` or `h3` for header styles). If you're comfortable with css styles or classes, you can add them with the `args` argument. 


```r
total_correct(elem = "h3", args = "style='color:#003366;'")
```

<h3 style='color:#003366;' id="webex-total_correct"></h3>



### Fill-In-The-Blanks

Create fill-in-the-blank questions using `fitb()`, providing the answer as the first argument.


```r
fitb(4)
```

- 2 + 2 is <input class='webex-solveme nospaces' size='1' data-answer='["4"]'/>

You can also create these questions dynamically, using variables from your R session (e.g., in a hidden code chunk).


```r
x <- sample(2:8, 1)
```


```r
fitb(x)
```

- The square root of 9 is: <input class='webex-solveme nospaces' size='1' data-answer='["3"]'/>



The blanks are case-sensitive; if you don't care about case, use the argument `ignore_case = TRUE`.


```r
fitb("E", ignore_case = TRUE)
```

- What is the letter after D? <input class='webex-solveme nospaces ignorecase' size='1' data-answer='["E"]'/>



If you want to ignore differences in whitespace use, use the argument `ignore_ws = TRUE` (which is the default) and include spaces in your answer anywhere they could be acceptable.


```r
fitb(c("library( tidyverse )", "library( \"tidyverse\" )", "library( 'tidyverse' )"), ignore_ws = TRUE, width = "20")
```

- How do you load the tidyverse package? <input class='webex-solveme nospaces' size='20' data-answer='["library( tidyverse )","library( \"tidyverse\" )","library( &apos;tidyverse&apos; )"]'/>

You can set more than one possible correct answer by setting the answers as a vector.


```r
fitb(c("A", "E", "I", "O" , "U"), ignore_case = TRUE)
```

- Type a vowel: <input class='webex-solveme nospaces ignorecase' size='1' data-answer='["A","E","I","O","U"]'/>

You can use regular expressions to test answers against more complex rules.


```r
fitb("^[a-zA-Z]{3}$", width = 3, regex = TRUE)
```

- Type any 3 letters: <input class='webex-solveme nospaces regex' size='3' data-answer='["^[a-zA-Z]{3}$"]'/>

### Multiple Choice

Set up a multiple-choice drop-down menu using `mcq()`.


```r
mcq(c("tidyr", "dplyr", answer = "readr", "ggplot2"))
```

- What package helps you load CSV files? <select class='webex-select'><option value='blank'></option><option value=''>tidyr</option><option value=''>dplyr</option><option value='answer'>readr</option><option value=''>ggplot2</option></select>
- "Never gonna give you up, never gonna: <select class='webex-select'><option value='blank'></option><option value=''>let you go</option><option value=''>turn you down</option><option value=''>run away</option><option value='answer'>let you down</option></select>"
- "I <select class='webex-select'><option value='blank'></option><option value='answer'>bless the rains</option><option value=''>guess it rains</option><option value=''>sense the rain</option></select> down in Africa" -Toto

### True or False

Make quick true/false questions with `torf()`.


```r
torf(TRUE)
torf(FALSE)
```

- True or False? You can permute values in a vector using `sample()`. <select class='webex-select'><option value='blank'></option><option value='answer'>TRUE</option><option value=''>FALSE</option></select>

### Longer MCQs

When your answers are very long, sometimes a drop-down select box gets formatted oddly. You can use `longmcq()` to deal with this. Since the answers are long, It's probably best to set up the options inside an R chunk with `echo=FALSE`. 


```r
opts_p <- c(
   "the probability that the null hypothesis is true",
   answer = "the probability of the observed, or more extreme, data, under the assumption that the null-hypothesis is true",
   "the probability of making an error in your conclusion"
)
```


```r
longmcq(opts_p)
```

**What is a p-value?**

<div class='webex-radiogroup' id='radio_BHKZEXSCQF'><label><input type="radio" autocomplete="off" name="radio_BHKZEXSCQF" value=""></input> <span>the probability that the null hypothesis is true</span></label><label><input type="radio" autocomplete="off" name="radio_BHKZEXSCQF" value="answer"></input> <span>the probability of the observed, or more extreme, data, under the assumption that the null-hypothesis is true</span></label><label><input type="radio" autocomplete="off" name="radio_BHKZEXSCQF" value=""></input> <span>the probability of making an error in your conclusion</span></label></div>


**What is true about a 95% confidence interval of the mean?**



<div class='webex-radiogroup' id='radio_EETFWANYAL'><label><input type="radio" autocomplete="off" name="radio_EETFWANYAL" value=""></input> <span>there is a 95% probability that the true mean lies within this range</span></label><label><input type="radio" autocomplete="off" name="radio_EETFWANYAL" value="answer"></input> <span>if you repeated the process many times, 95% of intervals calculated in this way contain the true mean</span></label><label><input type="radio" autocomplete="off" name="radio_EETFWANYAL" value=""></input> <span>95% of the data fall within this range</span></label></div>


### Hidden solutions and hints

You can fence off a solution area that will be hidden behind a button using `hide()` before the solution and `unhide()` after, each as inline R code.  Pass the text you want to appear on the button to the `hide()` function.


```r
hide("Click here to see the solution")
unhide()
```

If the solution is an RMarkdown code chunk, instead of using `hide()` and `unhide()`, you can set the `webex.hide` chunk option to TRUE, or set it to the string you wish to display on the button.

How do you load tidyverse?


<div class='webex-solution'><button>Click here to see the solution</button>



```r
library(tidyverse)
```


</div>


## Bookdown

You can add webexercises to a bookdown project or start a new bookdown project using `add_to_bookdown()`.


```r
# create a new book
# use default includes and scripts directories (include and R)
add_to_bookdown(bookdown_dir = "demo_bs4",
                output_format = "bs4_book",
                render = TRUE)

add_to_bookdown(bookdown_dir = "demo_git",
                output_format = "gitbook",
                render = TRUE)

add_to_bookdown(bookdown_dir = "demo_html",
                output_format = "html_book",
                render = TRUE)

add_to_bookdown(bookdown_dir = "demo_tufte",
                output_format = "tufte_html_book",
                render = TRUE)

# update an existing book with custom include and script directories
add_to_bookdown(bookdown_dir = ".",
                include_dir = "www",
                script_dir = "scripts",
                output_format = "gitbook")
```

## Learnr syntax

You can use learnr syntax to set up webexercises, too. 


```r
quiz(caption = "Learnr Syntax Examples",
  # fitb: a single true answer
  question("2 + 2 is",
           answer(4, TRUE)),
  # multi-answer: set type explicitly
  question("Type a vowel:",
           type = "learnr_text",
           answer("A", TRUE),
           answer("E", TRUE),
           answer("I", TRUE),
           answer("O", TRUE),
           answer("U", TRUE),
           options = list(ignore_case = TRUE)),
  # mcq: short answers (charlength <= 50)
  question("Never gonna give you up, never gonna:",
           answer("let you go"), 
           answer("turn you down"), 
           answer("run away"), 
           answer("let you down", TRUE),
           random_answer_order = TRUE),
  # longmcq: longer answers (charlength > 50)
  question("What is a p-value?",
           answer("the probability that the null hypothesis is true"),
           answer("the probability of the observed, or more extreme, data, under the assumption that the null-hypothesis is true", TRUE),
           answer("the probability of making an error in your conclusion"))
)
```

<div class='webex-quiz'><div class='webex-quiz-title'>Learnr Syntax Examples</div><ol><li><div class='webex-question'><span class='webex-question-text'>2 + 2 is</span><input class='webex-solveme nospaces' size='1' data-answer='["4"]'/></div></li><li><div class='webex-question'><span class='webex-question-text'>Type a vowel:</span><input class='webex-solveme nospaces ignorecase' size='1' data-answer='["A","E","I","O","U"]'/></div></li><li><div class='webex-question'><span class='webex-question-text'>Never gonna give you up, never gonna:</span><select class='webex-select'><option value='blank'></option><option value='x'>let you go</option><option value='x'>turn you down</option><option value='x'>run away</option><option value='answer'>let you down</option></select></div></li><li><div class='webex-question'><span class='webex-question-text'>What is a p-value?</span><div class='webex-radiogroup' id='radio_EXVKZCERZD'><label><input type="radio" autocomplete="off" name="radio_EXVKZCERZD" value="x"></input> <span>the probability that the null hypothesis is true</span></label><label><input type="radio" autocomplete="off" name="radio_EXVKZCERZD" value="answer"></input> <span>the probability of the observed, or more extreme, data, under the assumption that the null-hypothesis is true</span></label><label><input type="radio" autocomplete="off" name="radio_EXVKZCERZD" value="x"></input> <span>the probability of making an error in your conclusion</span></label></div>
</div></li></ol></div>



<a rel="license" href="https://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.

<script>

/* update total correct if #webex-total_correct exists */
update_total_correct = function() {
  console.log("webex: update total_correct");

  if (t = document.getElementById("webex-total_correct")) {
    var correct = document.getElementsByClassName("webex-correct").length;
    var solvemes = document.getElementsByClassName("webex-solveme").length;
    var radiogroups = document.getElementsByClassName("webex-radiogroup").length;
    var selects = document.getElementsByClassName("webex-select").length;
    
    t.innerHTML = correct + " of " + (solvemes + radiogroups + selects) + " correct";
  }
}

/* webex-solution button toggling function */
b_func = function() {
  console.log("webex: toggle hide");
  
  var cl = this.parentElement.classList;
  if (cl.contains('open')) {
    cl.remove("open");
  } else {
    cl.add("open");
  }
}

/* function for checking solveme answers */
solveme_func = function(e) {
  console.log("webex: check solveme");

  var real_answers = JSON.parse(this.dataset.answer);
  var my_answer = this.value;
  var cl = this.classList;
  if (cl.contains("ignorecase")) {
    my_answer = my_answer.toLowerCase();
  }
  if (cl.contains("nospaces")) {
    my_answer = my_answer.replace(/ /g, "")
  }

  if (my_answer == "") {
    cl.remove("webex-correct");
    cl.remove("webex-incorrect");
  } else if (real_answers.includes(my_answer)) {
    cl.add("webex-correct");
    cl.remove("webex-incorrect");
  } else {
    cl.add("webex-incorrect");
    cl.remove("webex-correct");
  }

  // match numeric answers within a specified tolerance
  if(this.dataset.tol > 0){
    var tol = JSON.parse(this.dataset.tol);
    var matches = real_answers.map(x => Math.abs(x - my_answer) < tol)
    if (matches.reduce((a, b) => a + b, 0) > 0) {
      cl.add("webex-correct");
    } else {
      cl.remove("webex-correct");
    }
  }

  // added regex bit
  if (cl.contains("regex")){
    answer_regex = RegExp(real_answers.join("|"))
    if (answer_regex.test(my_answer)) {
      cl.add("webex-correct");
    }
  }

  update_total_correct();
}

/* function for checking select answers */
select_func = function(e) {
  console.log("webex: check select");
  
  var cl = this.classList
  
  /* add style */
  cl.remove("webex-incorrect");
  cl.remove("webex-correct");
  if (this.value == "answer") {
    cl.add("webex-correct");
  } else if (this.value != "blank") {
    cl.add("webex-incorrect");
  }
  
  update_total_correct();
}

/* function for checking radiogroups answers */
radiogroups_func = function(e) {
  console.log("webex: check radiogroups");

  var checked_button = document.querySelector('input[name=' + this.id + ']:checked');
  var cl = checked_button.parentElement.classList;
  var labels = checked_button.parentElement.parentElement.children;
  
  /* get rid of styles */
  for (i = 0; i < labels.length; i++) {
    labels[i].classList.remove("webex-incorrect");
    labels[i].classList.remove("webex-correct");
  }
  
  /* add style */
  if (checked_button.value == "answer") {
    cl.add("webex-correct");
  } else {
    cl.add("webex-incorrect");
  }
  
  update_total_correct();
}

window.onload = function() {
  console.log("onload");
  /* set up solution buttons */
  var buttons = document.getElementsByTagName("button");

  for (var i = 0; i < buttons.length; i++) {
    if (buttons[i].parentElement.classList.contains('webex-solution')) {
      buttons[i].onclick = b_func;
    }
  }

  /* set up webex-solveme inputs */
  var solveme = document.getElementsByClassName("webex-solveme");

  for (var i = 0; i < solveme.length; i++) {
    /* make sure input boxes don't auto-anything */
    solveme[i].setAttribute("autocomplete","off");
    solveme[i].setAttribute("autocorrect", "off");
    solveme[i].setAttribute("autocapitalize", "off");
    solveme[i].setAttribute("spellcheck", "false");
    solveme[i].value = "";

    /* adjust answer for ignorecase or nospaces */
    var cl = solveme[i].classList;
    var real_answer = solveme[i].dataset.answer;
    if (cl.contains("ignorecase")) {
      real_answer = real_answer.toLowerCase();
    }
    if (cl.contains("nospaces")) {
      real_answer = real_answer.replace(/ /g, "");
    }
    solveme[i].dataset.answer = real_answer;

    /* attach checking function */
    solveme[i].onkeyup = solveme_func;
    solveme[i].onchange = solveme_func;
  }
  
  /* set up radiogroups */
  var radiogroups = document.getElementsByClassName("webex-radiogroup");
  for (var i = 0; i < radiogroups.length; i++) {
    radiogroups[i].onchange = radiogroups_func;
  }
  
  /* set up selects */
  var selects = document.getElementsByClassName("webex-select");
  for (var i = 0; i < selects.length; i++) {
    selects[i].onchange = select_func;
  }

  update_total_correct();
}

</script>

