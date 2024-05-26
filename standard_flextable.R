require(flextable)
require(tidyverse)

# Standard flextable function
standard_flextable <-
  function(df, # A dataframe.
           # The table caption (NULL by default).         
           table_caption = NULL,
           # The caption alignment.
           # Change to "left" for wide tables.
           table_caption_alignment = "center",
           # The base font size. 
           # The caption is 2 points larger than the table.
           font_size = get_flextable_defaults()$font.size,
           # Make a vertical scroll bar for long tables.
           # In pixels. 400 recommended.
           scroll_height = 400,
           # By default, flextable will include all the
           # dataframe columns. Provide a character vector
           # of columns to include to override that. Columns that 
           # are not included will be "hidden", and can still be used
           # in formulas (e.g., highlighting rows).
           # Recommend using names(df) and discard() for the unneeded columns.
           table_columns = NULL) {
    
    if(exists(params) & exists("table_header_colour", where = params)) {
      table_header_colour <- params$table_header_colour
    } else {
      table_header_colour <- "gray90"
    }
    
    if(exists(params) & exists("table_header_font_colour", where = params)) {
      table_header_font_colour <- params$table_header_font_colour
    } else {
      table_header_font_colour <- "black"
    }    
    
    if(is.null(table_columns)){
      flextable_colkeys <- names(df)
    } else {
      flextable_colkeys <- table_columns
    }
    
    
    return(  
      df %>%
        flextable(col_keys = flextable_colkeys) %>%
        fontsize(size = font_size, part = "all") %>%
        autofit() %>%
        # Change the header background colour.
        bg(bg = table_header_colour, part = "header") %>%
        # and the font colour
        color(color = table_header_font_colour, part = "header") %>%
        {
          if(exists("table_border_colour", where = params)) {
            # Set the borders only if table border colour is specified
            border_outer(
              border = fp_border(color = table_border_colour, style = "solid")
            ) %>%
              border_inner(
                border = fp_border(color = table_border_colour, style = "solid")
              ) %>%
              border_inner(
                border = fp_border(color = "black", style = "solid"), 
                part = "header"
              )
          } else {.}
        } %>%
        # Centre the header cells.
        align(align = "center", part = "header") %>%
        # set the caption if it is not blank.
        {
          if (is.null(table_caption))
            . # . is the piped dataframe
          else
            set_caption(
              .,
              caption = as_paragraph(as_chunk(
                table_caption,
                # make the caption bold and 2 points larger
                # than the table text
                props = fp_text_default(font.size = font_size + 2,
                                        bold = TRUE)
              )),
              fp_p = officer::fp_par(
                text.align = table_caption_alignment,
                padding.bottom = font_size + 2,
                shading.color = "white"),
              # For wide tables that scroll,
              # we want the caption left-aligned.
              # align_with_table needs to be FALSE
              # or the text.align parameter
              # in fp_par (above) will be ignored.
              align_with_table = FALSE
            )
        } %>%
        # set scroll bar height if the table is taller than
        # the scroll_height parameter
        # Conversion is 1 inch = 96 pixels
        
        # if there is a caption, make it sticky
        {
          if (sum(.$header$rowheights,
                  .$body$rowheights,
                  .$footer$rowheights) * 96 > scroll_height) {
            set_table_properties(
              .,
              opts_html = if (is.null(table_caption)) {
                list(scroll = list(height = paste0(scroll_height, "px")))
              } else {
                list(
                  scroll = list(height = paste0(scroll_height, "px")),
                  extra_css = paste0(
                    " caption {position: sticky;top: 0px;z-index: 4;} ",
                    # We need to set the distance of the header row from the top
                    # to be the bottom edge of the caption.
                    # The bottom edge of the caption is the font height times the
                    # number of lines plus the padding at the bottom.
                    
                    # The font size can be grabbed from the first element of
                    # .$caption$value$font.size
                    
                    # Linebreaks are in .$caption$value$txt as "<br>".
                    # The number of lines is 1 + the number of linebreaks.
                    
                    # Unfortunately, the font size setting gets overidden by the
                    # browser default - 16px or 12pt
                    
                    # The measurements are in points.
                    # To convert points to pixels, we divide by 72
                    # and multiply by 96.
                    
                    " th {top: ",
                    round(
                      # 12 points times the number of rows plus the bottom padding
                      (12 * (sum(
                        str_detect(.$caption$value$txt, "<br>")
                      ) + 1) +
                        .$caption$fp_p$padding.bottom) / 72 * 96,
                      digits = 4
                    ),
                    "px !important;} "
                  )
                )
              }
            )
          } else {
            set_table_properties(., opts_html = list(scroll = list()))
          }
        }
    )
  }