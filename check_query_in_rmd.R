# This function is for checking a SQL query prior to using GROUP BY,
# e.g., when counting up the number of students in a course.

# It is for use in RMarkdown with the html output type.

# It requires a code chunk to be inserted into the main RMarkdown
# document after the one where this function is called. 

#```{r, child=child}
#```

# The variable "child" is the location of the child RmD file that is
# created by this function. 

# That needs to be declared before this function is executed.



# It creates new heading (at a specified heading level) with the .tabset flag,
# then two unnumbered sections at the heading level below the main one.
# The subheadings will appear in tabs in the final html document.

# Main heading, "Build SQL Query", displays the SQL query provided as a
# function parameter. The intention is for this to include a row limit,
# e.g., 1000 rows.

# The first tab is a data table for the dataframe provided as a function
# parameter - i.e., the sample of data extracted by the query.

# The second tab is a table with a tally of the data when grouped by all
# the columns. If the query has generated extra rows for a table column
# that was not included in the SELECT statement, then there will be more
# than 1 row per group.

# By default, we will use a temporary file for the child doc.
# This can be overridden in the function parameters.

require(knitr)


sql_tab <- function(
    query,
    heading_level = 2,
    unnumbered = T,
    connect = connect) {

  options(knitr.duplicate.label = "allow")

  # create a heading and render the SQL code
  cat(
    knitr::knit_child(
      text = c(
        paste0(
          paste0(rep("#", heading_level), collapse = ""),
          " SQL Query",
          if (unnumbered == T) {
            paste0(" {.unnumbered}")
          } else {paste0("")},
          "\n\n",
          " \n",
          "```{glue_sql check-query-",
          # chunk labels need to be unique, so we will
          # append the current datetime
          format(Sys.time(), "%Y%m%d%H%M%S"),
          ", eval = FALSE, class.source = 'fold-show', ",
          "code = sqlparseR::sql_format(",
          substitute(query),
          ", reindent = TRUE, ",
          "use_space_around_operators = TRUE, ",
          "wrap_after = 40, ",
          "indent_after_first = TRUE, ",
          "strip_comments = FALSE) %>% ",
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
          #   str_replace_all("\\\n\\\n", "\\\n")
          # i.e., find all instances of "\n\n" and replace with "\n"
          
          "str_replace_all(\"\\\\\\n\\\\\\n\", \"\\\\\\n\"), ",
          "connection = connect}\n\n",
          "```\n\n"
        )
      ),
      quiet = TRUE
    )
  )

}

tally_tab <- function (
    dataframe_name,
    grouping_columns, 
    heading_level = 2,
    unnumbered = T) {
   
   options(knitr.duplicate.label = "allow")
   
   # create a heading and render table
   cat(
     knitr::knit_child(
       text = c(
         paste0(
           paste0(rep("#", heading_level), collapse = ""),
           " Tally",
           if (unnumbered == T) {
             paste0(" {.unnumbered}")
           } else {paste0("")},
           "\n\n",
           " \n",
           "```{r tally-", 
           # chunk labels need to be unique, so we will
           # append the current datetime
           format(Sys.time(), "%Y%m%d%H%M%S"),
           "}\n",
           "\n\n",
           dataframe_name,
           " %>%",
           "\n\n",
           "group_by(",
           paste0(grouping_columns, collapse = ", "),
           ") %>%",
           "\n\n",
           "tally() %>% select(n, everything()) %>% ",
           "\n\n",
           "arrange(desc(n)) %>%",
           "\n\n",
           "standard_flextable(",
           ")",
           "\n\n",
           "```",
           "\n\n"
         )
       ), 
       quiet = TRUE
     )
   )
   
 }
#     
# )

# walk(names(db_selected),
#      ~ 
#        cat(
#          paste0(
#            "\n\n",
#            "## ", .x, " {.unnumbered}",
#            "\n\n",
#            "```{r ", .x, "-db-table}\n",
#            "\n\n",
#            "standard_flextable(db_selected[[\"",.x,"\"]] %>%\n",
#            "select(-TABLE.COLUMN) %>%\n",
#            "rename(\"SCHEMA\" = \"TABLE_SCHEMA\"), ",
#            "\"",.x, "\")",
#            "\n\n",
#            "```"
#          ),
#          file = child,
#          append = TRUE
#        )
# )