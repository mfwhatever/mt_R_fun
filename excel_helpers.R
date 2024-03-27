# 2024/02/14

# A suite of functions for formatting Excel workbooks using openxlsx2
require(openxlsx2)
require(tidyverse)


# ################################### #
#### Create Accompanying Notes tab ####
# ################################### #

add_accompanying_notes <- function (
    workbook,
    worksheet_name = "Accompanying notes",
    notes_vector = notes_for_client,
    column_width = 100
) {
  workbook <- workbook$clone() 
  
  # Add worksheet
  workbook$add_worksheet(
    worksheet_name
  )
  
  # Add a column heading (= worksheet_name)
  workbook$add_data(
    worksheet_name,
    worksheet_name,
    dims = "A1"
  )
  
  # Column Heading: centre-align cells
  workbook$add_cell_style(
    worksheet_name,
    dims="A1",
    horizontal = "center"
  )
  
  # Column Heading: bold
  workbook$add_font(
    worksheet_name,
    dims="A1",
    bold = "1"
  ) 
    
    # Add the notes
    workbook$add_data(
      worksheet_name,
      notes_vector,
      # add each note in the vector directly
      # underneath the column heading
      dims = wb_dims(
        x = notes_for_client, 
        cols = 1, from_row = 2, 
        select = "data" )
    ) 
    
    # First Column: Wrap text
    workbook$add_cell_style(
      worksheet_name,
      dims=wb_dims(
        x = c(
          # Add a dummy entry to the beginning of the vector for the
          # column heading to get the correct dimensions
          "Column Heading",
          notes_for_client
        ), 
        cols = 1, from_row = 1, 
        select = "x" ),
      wrap_text = "1"
    ) 
    
    # First Column: Set width 
    
    workbook$set_col_widths(
      worksheet_name,
      cols = c(1),
      widths = c(column_width)
    )
  
  workbook  
}


# ############################## #
#### Add Data Table Worksheet ####
# ############################## #

add_data_tables <- function (
    workbook,
    worksheet_name = "Untitled",
    dataframe,
    na.strings = na_strings(),
    # Optional list of data frame 
    # column names that need to have 
    # wrap text enabled. 
    # Format:
    # column_name = column_width
    # e.g., list(school_name = 40, 
    #            school_id = 30)
    # Column width unit is characters.  
    wrap_columns = list(), 
    freeze_rows = NULL,    # Number of rows to freeze from the top
    freeze_columns = NULL, # Number of columns to free from the left
    width_factor = 6  # Percentage amount by which to increase auto-calculated
                      # column widths. Necessary because the width algorithm
                      # does not take into account bold font.
                      # Set to 0 for no change.
) {

  workbook <- workbook$clone() 
  
  # Add worksheet
  workbook$add_worksheet(
    worksheet_name
  )
  
  # Add data

  workbook$add_data(
    worksheet_name, 
    dataframe,
    na.strings = na.strings
  )
  
  # First row bold
  workbook$add_font(
    worksheet_name, 
    dims=wb_dims(x = dataframe, select = "col_names"),
    bold = "single"
  ) 
  
  # Set column widths to auto. 
  workbook$set_col_widths(
    worksheet_name, 
    cols = 1:length(names(dataframe)),
    widths = "auto"
  )
  # Adjust column widths for bold font (workaround)
  # Not ideal as it uses private functions that may not be
  # available in future versions of the library
  # ...and it's a monstrosity
  workbook$set_col_widths(
    worksheet_name, 
    cols = 1:length(names(dataframe)),
    widths = as.character(
      as.numeric(
        workbook$worksheets[[
          workbook$.__enclos_env__$private$get_sheet_index(worksheet_name)
        ]]$unfold_cols()$width) * (1 + width_factor/100))
  )
  
  # Wrap text and override column width for selected columns
  if (length(wrap_columns) > 0) {
    
    # check that the column names match the dataframe
    if (!any(names(wrap_columns) %in% names(dataframe))) {
      mismatches = names(wrap_columns)[!any(names(wrap_columns) %in% names(dataframe))]
      stop(paste0(
        "\nThe following wrap column names do not match a dataframe column:\n\n\t",
        paste0(mismatches, collapse = "\n\t")
        )
      )
    }
    
    iwalk(wrap_columns,
          ~ workbook$add_cell_style(
              worksheet_name, 
              dims=wb_dims(x = dataframe,
                           cols = .y
              ),
              wrap_text = "1"
            )$set_col_widths(
              worksheet_name, 
              cols = match(.y, names(dataframe)),
              widths = .x
            )
    )
  }
  
  #Top row: align cells and wrap text
  workbook$add_cell_style(
    worksheet_name,
    dims=wb_dims(x = dataframe, select = "col_names"),
    horizontal = "center",
    vertical = "top",
    wrap_text = "1"
  )
  
  # Freeze panes
  if (!is.null(freeze_rows) & !is.null(freeze_columns)) {
    workbook$freeze_pane(
      worksheet_name, 
      first_active_row = freeze_rows + 1,
      first_active_col = freeze_columns + 1
    )
  } else if (!is.null(freeze_rows)) {
    workbook$freeze_pane(
      worksheet_name,
      first_active_row = freeze_rows + 1
    )
  } else if (!is.null(freeze_columns)) {
    workbook$freeze_pane(
      worksheet_name,
      first_active_col = freeze_columns + 1
    )
  }
  
  workbook
}    



#### Issues ####

# Auto column width does not take into account bold fonts

# bold fonts are possibly 5.2% wider 

# column_widths <- openxlsx2:::calc_col_width(
#   base_font = excel_wb$get_base_font(), 
#   col_width = vapply(registrations, 
#                      function(x) max(nchar(format(x))), 
#                      NA_real_)
#  )

  # This doesn't work because `format(x)` returns NA for date columns.
  # Note that `registrations` is a dataframe and `excel_wb` is a 
  # workbook environment