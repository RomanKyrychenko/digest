library(readxl)
library(shiny)
library(rmarkdown)
library(tidyverse)
system('fc-cache -f digest/.fonts')
Sys.setlocale(,"UK_ua")
shinyApp(
  ui = fluidPage(
    fileInput("fl", "Excel",accept = c(".xlsx")),
    downloadButton("digest", "Generate report")
  ),
  server = function(input, output) {
    df <- reactive({
      inFile <- input$fl
      if(is.null(inFile))
        return(NULL)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
    data <-read_excel(paste(inFile$datapath, ".xlsx", sep=""),1,col_names = T)})
    output$digest <- downloadHandler(
      filename = "digest.pdf",
      content = function(file) {

        file.copy("digest_n.Rmd", "digest.Rmd", overwrite = T)

        params <- list(data = df())

        rmarkdown::render("digest.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)