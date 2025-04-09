# This contains a suite of functions for adding elements to 
# RMarkdown documents in an html tab.

# The code chunk must be located under a heading where
# {.tabset} is at the end of the heading.

# Assumes that standard_flextable() is available in the RmD.

# If child_doc = T, cat will write to a temporary child document
# Otherwise, the knitted document will be inserted directly into
# the document. Note that the chunk options need to include 
# results = 'asis' for this to work.



require(knitr)
require(tidyverse)
require(rlang)
require(glue)
require(ggplot2)


#### child_doc_init() ####

# initialise a list for storing child doc filenames
child_doc_init <- function (filelist = child_docs) {
  # <<- makes it a global variable
  assign(deparse(substitute(filelist),
                 backtick = TRUE),
         list(),
         envir = globalenv()
  )
  
}

#### child_doc_add() ####

# Add the child_doc markdown to a file and store the filename in the 
# list of child_docs. 
child_doc_add <- function(markdown,
                          filelist = child_docs,
                          new_list = FALSE) {

  if(!exists(deparse(substitute(filelist),
                     backtick = TRUE)) |
     new_list) {
    child_doc_init()
    # cat("Child-doc filelist created\n\n")
  }
  
  # generate child_doc filename
  child_doc_filename <- paste0(
    tempdir(),
    "/",
    deparse(substitute(filelist),
            backtick = TRUE),
    "_",
    length(filelist) + 1,
    ".RmD"
  )
  
  assign(deparse(substitute(filelist),
                 backtick = TRUE),
         append(
           filelist, 
           child_doc_filename),
         envir = globalenv()
  )
  
  cat(
    markdown,
    file = child_doc_filename,
    sep = "\n"
  )
}

#### dataframe_tab() ####

# Creates a tab with a standard flextable of a dataframe
# flextable options is to pass arguments to standard_flextable()

# flextable_extras is to append extra flextable functions after 
# standard_flextable (e.g., flextable::bg()). These should be supplied
# as a text string. Do not supply the first pipe symbol.
# E.g., "flextable::bg(bg = 'orange', j = 'STDELS', 
#                       i = ~ STDELS == 'Y', part = 'body') %>%
#       flextable::bg(bg = 'orange', j = 'STDUCC', 
#                       i = ~ STDUCC == 'Y', part = 'body')"
# It could also be a character vector with separate functions. The pipe
# between elements will be supplied by the function.
# E.g., c("flextable::bg(bg = 'orange', j = 'STDELS', 
#                       i = ~ STDELS == 'Y', part = 'body')",
#       "flextable::bg(bg = 'orange', j = 'STDUCC', 
#                       i = ~ STDUCC == 'Y', part = 'body')"        
#       )


dataframe_tab <- function(dataframe,
                          tab_text = "Data Table",
                          heading_level = 2,
                          unnumbered = TRUE,
                          unlisted = TRUE,
                          flextable_options = "",
                          flextable_extras = "",
                          flextable_save_df, # a dataframe to save the flextable
                          child_doc = TRUE,
                          new_list = FALSE,
                          # the following line fixes a strange bug
                          # (along with the opts_knit() line in the 
                          # function)
                          knit_dir = getwd()) {
  options(knitr.duplicate.label = "allow")
  
  opts_knit$set(output.dir = knit_dir)
  
  flextable_expression <- 
      paste0(
        stringr::str_flatten(deparse(substitute(dataframe),
                                     backtick = TRUE)
                             ),
        " %>% standard_flextable(",
        flextable_options,
        ") ",
        {
          if (stringr::str_flatten(flextable_extras) == "") {
            paste0("")
          } else {
            paste0(" %>% ",
                   paste0(flextable_extras, collapse = " %>% ")
            )
          }
        }
      )
  
  if (hasArg(flextable_save_df))
  {
    eval(
      parse_expr(
        paste0(
          stringr::str_flatten(deparse(substitute(flextable_save_df),
                                       backtick = TRUE)),
          " <<- ",
          flextable_expression
        )
      )
    )
  }
  
  knit_text = c(
    paste0(
      paste0(rep("#", heading_level), collapse = ""),
      " ",
      tab_text,
      if (!(unnumbered == F &
            unlisted == F)) {
        paste0(" {",
               if (unnumbered == T & 
                   unlisted == T) {
                 paste0(".unnumbered .unlisted")
               } else if (unnumbered == T) {
                 paste0(".unnumbered")
               } else if (unlisted == T) {
                 paste0(".unlisted")
               } else {paste0("")},
               " }",
               collapse = ""
        )
      } else {
        paste0("")
      },
      "\n\n",
      "```{r datatable-",
      # chunk labels need to be unique, so we will
      # append the current datetime with seconds to 3 decimal places
      format(Sys.time(), "%Y%m%d%H%M%OS3"),
      "}",
      "\n\n",
      # {
      #   if (hasArg(flextable_save_df))
      #    {
      #     paste0(
      #       stringr::str_flatten(deparse(substitute(flextable_save_df))),
      #       " <- "
      #     )
      #   } 
      # },
      # stringr::str_flatten(deparse(substitute(dataframe))),
      # " %>% standard_flextable(",
      # flextable_options,
      # ") ",
      # {
      #   if (stringr::str_flatten(flextable_extras) == "") {
      #     paste0("")
      #   } else {
      #     paste0(" %>% ",
      #            paste0(flextable_extras, collapse = " %>% ")
      #     )
      #   }
      # },
      # "\n\n",
      # {
      #   if (hasArg(flextable_save_df)) {
      #     paste0(
      #       stringr::str_flatten(deparse(substitute(flextable_save_df)))
      #     )
      #   }
      # },
      flextable_expression,
      "\n\n",
      "```",
      "\n\n"
      
    )
  )
  
  if (!child_doc) {
    cat(
      knitr::knit_child(
        text = knit_text,
        quiet = TRUE)
    ) 
  } else {
    child_doc_add(
      knit_text,
      new_list = new_list
    ) 
  }
}

#### sql_tab() ####
 
# Creates a tab with a formatted SQL query
  # Tested with queries created with glue::glue_sql().
  # May not work with queries created some other way.

sql_tab <- function(query,
                    tab_text = "SQL Query",
                    heading_level = 2,
                    unnumbered = TRUE,
                    unlisted = TRUE,
                    connect = connect,
                    child_doc = TRUE,
                    new_list = FALSE,
                    knit_dir = getwd(),
                    # Normally, the SQL code is sent as text.
                    # When it is very long, this causes errors.
                    # Set use_object_name = T to use the environment
                    # object name instead (e.g., "query").
                    # Note that the object name will need to be in
                    # quotes
                    use_object_name = F,
                    # sqlparseR::sql_format()
                    # parameters
                    sql_format_params = list(
                      reindent = TRUE,
                      use_space_around_operators = TRUE,
                      wrap_after = 40, 
                      indent_after_first = TRUE,
                      strip_comments = FALSE
                      )
                    ){
  
  options(knitr.duplicate.label = "allow")
    # the following line fixes a strange bug
  opts_knit$set(output.dir = knit_dir)
  
  # create a heading and render the SQL code
  knit_text = c(
    paste0(
      paste0(rep("#", heading_level), collapse = ""),
      " ",
      tab_text,
      if (!(unnumbered == F &
            unlisted == F)) {
        paste0(" {",
               if (unnumbered == T & 
                   unlisted == T) {
                 paste0(".unnumbered .unlisted")
               } else if (unnumbered == T) {
                 paste0(".unnumbered")
               } else if (unlisted == T) {
                 paste0(".unlisted")
               } else {paste0("")},
               " }",
               collapse = ""
        )
      } else {
        paste0("")
      },
      "\n\n",
      "```{glue_sql check-query-",
      # chunk labels need to be unique, so we will
      # append the current datetime, with seconds to 3 decimal places
      format(Sys.time(), "%Y%m%d%H%M%OS3"),
      ", eval = FALSE, class.source = 'fold-show', ",
      "code = sqlparseR::sql_format(",
      # check if the text string in query matches an object
      # if so, we will treat it as an object, otherwise as a text string
      {
        if(use_object_name){
          paste0(query)
        } else {
          paste0("\"", query, "\"", collapse = "")
        }
      },
      ", ",
      paste(
        names(sql_format_params), 
        " = ", 
        sql_format_params,
        collapse = ", "
        ),
      ") %>% ",
      # These lines are to clean up the rendering of SQL comments.
      # sqlparseR generally does a good job, but it doesn't always
      # manage to put comments on their own line. This code ensures
      # that each comment is on a separate line.
      
      # It looks messy because we need to escape special characters
      # for both cat and str_replace_all.
      
      # The output of cat for line 1 is:
      #   str_replace_all("\\/\\*", "\\\n\\/\\*")
      
      # str_replace_all needs special characters to be double-escaped,
      # so search string is "/*" and the replacement is "\n/*".
      # It replaces any instance of "/*" with a line break followed
      # by "/*".
      
      "str_replace_all(\"\\\\/\\\\*\", \"\\\\\\n\\\\/\\\\*\") %>% ",
      
      # Line 2 replaces any instance of "*/" with "*/" followed by a
      # line break.
      
      "str_replace_all(\"\\\\*\\\\/\", \"\\\\*\\\\/\\\\\\n\") %>% ",
      
      # We end up with some comments that are separated from the
      # code by two line breaks instead of one.
      # Line 3 replaces any double line breaks with a single one.
      
      # The output of cat for line 3 is:
      #   str_replace_all("\\\n *?\\\n", "\\\n")
      # i.e., find all instances of "\n\n" and replace with "\n"
      # " *?" means that there can be any number of spaces between
      # the two \n.
      
      "str_replace_all(\"\\\\\\n *?\\\\\\n\", \"\\\\\\n\"), ",
      "connection = connect}",
      "\n\n",
      "```",
      "\n\n"
    )
  )
  
  if (!child_doc) {
    cat(
      knitr::knit_child(
        text = knit_text,
        quiet = TRUE)
    ) 
  } else {
    child_doc_add(
      knit_text,
      new_list = new_list
    )  
  }
}

#### plot_tab() ####

# Creates a tab containing the plot object (plot_obj) provided as argument

plot_tab <- function(plot_obj,
                     tab_text = "Plot",
                     figure_caption,
                     heading_level = 2,
                     unnumbered = TRUE,
                     unlisted = TRUE,
                     child_doc = TRUE,
                     new_list = FALSE,
                     knit_dir = getwd()) {
  
  options(knitr.duplicate.label = "allow")
  # the following line fixes a strange bug
  opts_knit$set(output.dir = knit_dir)
  
  # create a heading and place the plot
  knit_text = c(
    paste0(
      paste0(rep("#", heading_level), collapse = ""),
      " ",
      tab_text,
      if (!(unnumbered == F &
            unlisted == F)) {
        paste0(" {",
               if (unnumbered == T & 
                   unlisted == T) {
                 paste0(".unnumbered .unlisted")
               } else if (unnumbered == T) {
                 paste0(".unnumbered")
               } else if (unlisted == T) {
                 paste0(".unlisted")
               } else {paste0("")},
               " }",
               collapse = ""
        )
      } else {
        paste0("")
      },
      "\n\n",
      "```{R plot-",
      # chunk labels need to be unique, so we will
      # append the current datetime with seconds to 3 decimal placesOS3
      format(Sys.time(), "%Y%m%d%H%M%OS3"),
      if (hasArg(figure_caption)) {
        glue(", fig.cap = '**{figure_caption}**', fig.align='center'")
      },
      "}",
      "\n\n",
      "library(ggplot2)",
      "\n",
      deparse1(substitute(plot_obj), backtick = TRUE, collapse = ""),
      "\n\n",
      "```",
      "\n\n"
    )
  )

  if (!child_doc) {
    cat(
      knitr::knit_child(
        text = knit_text,
        quiet = TRUE)
    ) 
  } else {
    child_doc_add(
      knit_text,
      new_list = new_list
    )  
  }
}

#### plot_tab2() ####

# Creates a tab with a plot generated from a plotting function (plot_fn) using
# the required inputs (plot_fn_args), both provided as argument

plot_tab2 <- function(plot_fn,  # function to generate plot object / plot code
                      plot_fn_args,  # arguments of plot_fn in list format
                      tab_text = "Plot",
                      figure_caption,
                      heading_level = 2,
                      unnumbered = TRUE,
                      unlisted = TRUE,
                      child_doc = TRUE,
                      new_list = FALSE,
                      knit_dir = getwd()) {
  
  options(knitr.duplicate.label = "allow")
  # the following line fixes a strange bug
  opts_knit$set(output.dir = knit_dir)
  
  # create a heading and place the plot
  knit_text = c(
    paste0(
      paste0(rep("#", heading_level), collapse = ""),
      " ",
      tab_text,
      if (!(unnumbered == F &
            unlisted == F)) {
        paste0(" {",
               if (unnumbered == T & 
                   unlisted == T) {
                 paste0(".unnumbered .unlisted")
               } else if (unnumbered == T) {
                 paste0(".unnumbered")
               } else if (unlisted == T) {
                 paste0(".unlisted")
               } else {paste0("")},
               " }",
               collapse = ""
        )
      } else {
        paste0("")
      },
      "\n\n",
      "```{R plot-",
      # chunk labels need to be unique, so we will
      # append the current datetime with seconds to 3 decimal placesOS3
      format(Sys.time(), "%Y%m%d%H%M%OS3"),
      if (hasArg(figure_caption)) {
        glue(", fig.cap = '**{figure_caption}**', fig.align='center'")
      },
      ", cache=TRUE}",
      "\n\n",
      "library(ggplot2)",
      "\n",
      do.call(plot_fn, plot_fn_args),
      "\n\n",
      "```",
      "\n\n"
    )
  )
  
  if (!child_doc) {
    cat(
      knitr::knit_child(
        text = knit_text,
        quiet = TRUE)
    ) 
  } else {
    child_doc_add(
      knit_text,
      new_list = new_list
    )  
  }
}

#### plot_tab3() ####

# Creates a tab with a plot imported from a saved image file, e.g. png

plot_tab3 <- function(plot_dir,  # directory path to saved plot image
                      tab_text = "Plot",
                      figure_caption,
                      heading_level = 2,
                      unnumbered = TRUE,
                      unlisted = TRUE,
                      child_doc = TRUE,
                      new_list = FALSE,
                      knit_dir = getwd()) {
  
  options(knitr.duplicate.label = "allow")
  # the following line fixes a strange bug
  opts_knit$set(output.dir = knit_dir)
  
  # create a heading and place the plot
  knit_text = c(
    paste0(
      paste0(rep("#", heading_level), collapse = ""),
      " ",
      tab_text,
      if (!(unnumbered == F &
            unlisted == F)) {
        paste0(" {",
               if (unnumbered == T & 
                   unlisted == T) {
                 paste0(".unnumbered .unlisted")
               } else if (unnumbered == T) {
                 paste0(".unnumbered")
               } else if (unlisted == T) {
                 paste0(".unlisted")
               } else {paste0("")},
               " }",
               collapse = ""
        )
      } else {
        paste0("")
      },
      "\n\n",
      "```{R plot-",
      # chunk labels need to be unique, so we will
      # append the current datetime with seconds to 3 decimal placesOS3
      format(Sys.time(), "%Y%m%d%H%M%OS3"),
      if (hasArg(figure_caption)) {
        glue(", fig.cap = '**{figure_caption}**', fig.align='center'")
      },
      ", cache=TRUE}",
      "\n\n",
      glue("knitr::include_graphics('{plot_dir}')"),
      "\n\n",
      "```",
      "\n\n"
    )
  )
  
  if (!child_doc) {
    cat(
      knitr::knit_child(
        text = knit_text,
        quiet = TRUE)
    ) 
  } else {
    child_doc_add(
      knit_text,
      new_list = new_list
    )  
  }
}

#### tally_tab() ####

# Creates a tab with a flextable of a dataframe
# where the columns have been grouped by grouping_columns
# and the rows are tallied by group. The column with the count
# is moved to beginning.

# dataframe_name is a text string corresponding to the name of the
# dataframe in the Global Environment. It is not possible to use the
# function in a pipe, but code to modify a dataframe could be provided
# as text. For example, "paste0(dataframe_name, " %>% select(column_name)")".

tally_tab <- function (dataframe_name,
                       grouping_columns,
                       tab_text = "Tally",
                       heading_level = 2,
                       unnumbered = TRUE, 
                       unlisted = TRUE,
                       flextable_options = "",
                       child_doc = TRUE,
                       new_list = FALSE,
                       knit_dir = getwd()) {
  
  options(knitr.duplicate.label = "allow")
  # the following line fixes a strange bug
  opts_knit$set(output.dir = knit_dir)
  
  # create a heading and render table
  knit_text = c(
    paste0(
      paste0(rep("#", heading_level), collapse = ""),
      " ",
      tab_text,
      if (!(unnumbered == F &
            unlisted == F)) {
        paste0(" {",
               if (unnumbered == T & 
                   unlisted == T) {
                 paste0(".unnumbered .unlisted")
               } else if (unnumbered == T) {
                 paste0(".unnumbered")
               } else if (unlisted == T) {
                 paste0(".unlisted")
               } else {paste0("")},
               " }",
               collapse = ""
            )
      } else {
        paste0("")
      },
      "\n\n",
      "```{r tally-",
      # chunk labels need to be unique, so we will
      # append the current datetime with seconds to 3 decimal places
      format(Sys.time(), "%Y%m%d%H%M%OS3"),
      "}",
      "\n\n",
      dataframe_name,
      " %>%",
      "\n",
      "group_by(",
      paste0(grouping_columns, collapse = ", "),
      ") %>%",
      "\n",
      "tally() %>% select(n, everything()) %>% ",
      "\n",
      "arrange(desc(n)) %>%",
      "\n",
      "standard_flextable(",
      flextable_options,
      ")",
      "\n\n",
      "```",
      "\n\n"
    )
  )
  
  if (!child_doc) {
    cat(
      knitr::knit_child(
        text = knit_text,
        quiet = TRUE)
    ) 
  } else {
    child_doc_add(
      knit_text,
      new_list = new_list
    ) 
  }
}