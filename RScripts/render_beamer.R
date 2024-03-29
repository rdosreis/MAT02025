library(here)
library(rmarkdown)

pasta <- "Rmds"
arquivo <- "22_23_ta_intro_formulas"

arquivo_rmd <- paste0(arquivo, ".Rmd")
arquivo_pdf <- paste0(arquivo, ".pdf")
input <- here::here(pasta, arquivo_rmd)
arquivo_r <- paste0(arquivo, ".R")

rmarkdown::render(input = input, 
                  output_format = beamer_presentation(theme = "Antibes",
                                                      highlight = "default",
                                                      colortheme = "dolphin",
                                                      fonttheme = "structurebold",
                                                      includes = list(in_header = here::here("styles", "mystyle.tex")),
                                                      slide_level = 2,
                                                      keep_tex = FALSE,
                                                      number_sections = FALSE,
                                                      fig_caption = FALSE),
                  output_file = arquivo_pdf,
                  output_dir = here::here("output_pdf"),
                  encoding = "UTF-8")

knitr::purl(input = input,
            output = here::here("RScripts", arquivo_r),
            documentation = 1,
            encoding = "UTF-8")


# rmarkdown::render(input = input, 
#                   output_format = "powerpoint_presentation")
