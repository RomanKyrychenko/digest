Sys.setlocale(,"UK_ua")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(grid)
library(readxl)
library(rmarkdown)
library(gridExtra)
library(cowplot)
library(png)
library(extrafont)

unzip(".fonts.zip",exdir = "~/",overwrite = T)
system('fc-cache -f ~/.fonts')

ui <- dashboardPage(skin = "red",
                    title="Corestone work tools",
                    dashboardHeader(
                      title="Corestone work tools",
                      tags$li(class = "dropdown",
                              tags$a(href="http://corestone.expert/", target="_blank", 
                                     tags$img(height = "20px", alt="Corestone", src="http://corestone.expert/static/icons/ic-navbar-logo.svg")
                              )
                      ),
                      dropdownMenuOutput("sys"),
                      tags$li(class = "dropdown",
                              tags$a(href = "https://github.com/RomanKyrychenko",
                                     target = "_blank",
                                     tags$img(height = "20px", 
                                              src = "https://raw.githubusercontent.com/oraza/sectarianviolencePK/master/www/github.png")
                              )
                      )
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("ІноЗМІ", tabName = "ІноЗМІ",icon = icon("newspaper-o")),
                        menuItem("Infoflow", tabName = "Infoflow", icon = icon("vcard-o")),
                        #menuItem("System", tabName = "sys2", icon = icon("line-chart")),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        hr(),
                        menuItem("Documentation", icon = icon("file-text-o"), 
                                 href = "https://github.com/RomanKyrychenko/digest/blob/master/README.md"),
                        menuItem("Feedback & suggestion", icon = icon("envelope-o"),
                                 href = "mailto:?Roman.Kyrychenko@corestone.expert?subject=Feedback on Corestone work tools app"),
                        menuItem("Source code", icon = icon("file-code-o"), 
                                 href = "https://github.com/RomanKyrychenko/digest"),
                        menuItem("Fork me @ github", icon = icon("code-fork"), 
                                 href = "https://github.com/RomanKyrychenko") 
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "ІноЗМІ",
                                h2("ІноЗМІ"),
                                fileInput("fl", "Excel",accept = c(".xlsx")),
                                downloadButton("digest", "Generate report")
                        ),
                        tabItem(tabName = "Infoflow",
                                h2("Infoflow"),
                                fileInput('file1', 'Завантажте файл з даними',
                                          accept = c(".xlsx")),
                                tags$hr(),
                                downloadButton('down',"Завантажити в pdf!"),
                                downloadButton('do',"Завантажити в png!"),
                                plotOutput('plot', width = "3666px", height = "2358px")
                        )#,
                     #   tabItem(tabName = "sys2",
                     #           h2("System"),
                     #           actionButton("goButton", "Go!"),
                     #           verbatimTextOutput("nText"))
                     )
                    )
)

server <- function(input,output,server,session){
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
  df2 <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    data <- readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""),col_names = T)
    
    infoflow <- function(data){
      por <- c(9.9,8.85,7.8,6.75,5.7,4.65,3.6,2.55,1.5,.45)
      lin <- inner_join(data_frame(name=data[[1]][14:23],tb=#c(10,8.85,7.7,6.9,5.87,4.83,3.78,2.73,1.68,.63)),
                                     seq(10,0.1,-1.1)),
                        data_frame(name=data[[1]][2:11],net=#c(10,8.85,7.7,6.9,5.87,4.83,3.78,2.73,1.68,.63)),
                                     seq(10,0.1,-1.1)),
                        by="name")
      
      lin$tb2 <- 0
      lin$net2 <- 1
      
      p1 <- ggplot()+
        geom_bar(aes(reorder(data[[1]][2:11],as.numeric(data[[2]][2:11])),
                     as.numeric(data[[3]][2:11])),stat = "identity",width = 0.6,fill="#DFE0E1",color=NA)+
        geom_bar(aes(reorder(data[[1]][2:11],as.numeric(data[[2]][2:11])),
                     as.numeric(data[[2]][2:11])),stat = "identity",width = 0.4,fill="#A2C1C1",color=NA)+
        geom_text(aes(reorder(data[[1]][2:11],as.numeric(data[[2]][2:11])),0,
                      label=reorder(data[[1]][2:11],as.numeric(data[[2]][2:11]))),hjust=0,vjust=-2,size=6*3,
                  family = "PT Sans")+
        annotate("text",0.3,max(as.numeric(data[[2]][2:11])),label="тис. контактів",
                 hjust=1,vjust=-2,size=5*3,family = "PT Sans",color="#636566")+
        ylab("")+
        xlab("")+
        scale_x_discrete(labels=rev(data[[1]][2:11]),position = "top")+
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip() + theme(
          panel.grid = element_blank(), panel.border = element_blank(),
          axis.text.y = element_blank(),
          axis.line.x = element_line(size=1,color="#A2C1C1"),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(hjust=0,vjust=-2,size=14*3,color="#636566",family = "PT Sans"),
          axis.ticks.length = unit(0.1, "line"),
          #axis.ticks.length = unit(-1, "line"),
          plot.margin=unit(c(0,0,0,0), "cm")
        )
      
      p2 <- ggplot()+
        geom_bar(aes(reorder(data[[1]][14:23],as.numeric(data[[2]][14:23])),
                     as.numeric(data[[3]][14:23])),stat = "identity",width = 0.6,fill="#DFE0E1")+
        geom_bar(aes(reorder(data[[1]][14:23],as.numeric(data[[2]][14:23])),
                     as.numeric(data[[2]][14:23])),stat = "identity",width = 0.4,fill="#E1B5AC")+
        geom_text(aes(reorder(data[[1]][14:23],as.numeric(data[[2]][14:23])),0,
                      label=reorder(data[[1]][14:23],as.numeric(data[[2]][14:23]))),hjust=1,vjust=-2,size=6*3,family = "PT Sans")+
        annotate("text",0.3,max(as.numeric(data[[2]][14:23])),
                 label="млн контактів",hjust=0,vjust=-2,size=5*3,family = "PT Sans",color="#636566")+
        ylab("")+
        xlab("")+
        scale_x_discrete(labels=rev(data[[1]][14:23]))+
        coord_flip() + theme(
          panel.grid = element_blank(), panel.border = element_blank(),
          axis.line.x = element_line(color="#E1B5AC",size=1),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(hjust=1,vjust=-2,size=14*3,color="#636566",family = "PT Sans"),
          axis.ticks.length = unit(0.1, "line"),
          plot.margin=unit(c(0,0,0,0), "cm")
        ) + scale_y_reverse(expand = c(0, 0))
      
      p3 <- ggplot() + 
        geom_segment(aes(x = lin$tb2, xend=lin$net2,y=lin$tb,yend=lin$net),color="#BFBFBF",alpha=1,size=0.5,lineend = "round") +
        #scale_color_gradient(high="#136278",low="#c41e58")+
        ylab("")+
        xlab("")+
        scale_x_continuous(limits = c(0,1),expand = c(0, 0))+
        scale_y_continuous(limits = c(0,10))+
        theme(
          legend.position = "none",
          panel.grid = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(-1.4, "line"),
          plot.margin=unit(c(0,-0.5,0,-0.5,0), "cm")
        ) 
      
      p4 <- ggplot()+
        geom_text(aes(1,por,label=ifelse(!is.na(as.numeric(data[[4]][14:23])),paste0(round(as.numeric(data[[4]][14:23])*100),"%"),"∞")),family = "PT Sans",
                  hjust=1,color="#636566",size=6*3) +
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][14]>0,"green.png","red.png")), interpolate=TRUE), xmin=1, xmax=1.1, ymin=por[1]-0.1, ymax=por[1]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][15]>0,"green.png","red.png")), interpolate=TRUE), xmin=1, xmax=1.1, ymin=por[2]-0.1, ymax=por[2]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][16]>0,"green.png","red.png")), interpolate=TRUE), xmin=1, xmax=1.1, ymin=por[3]-0.1, ymax=por[3]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][17]>0,"green.png","red.png")), interpolate=TRUE), xmin=1, xmax=1.1, ymin=por[4]-0.1, ymax=por[4]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][18]>0,"green.png","red.png")), interpolate=TRUE), xmin=1, xmax=1.1, ymin=por[5]-0.1, ymax=por[5]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][19]>0,"green.png","red.png")), interpolate=TRUE), xmin=1, xmax=1.1, ymin=por[6]-0.1, ymax=por[6]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][20]>0,"green.png","red.png")), interpolate=TRUE), xmin=1, xmax=1.1, ymin=por[7]-0.1, ymax=por[7]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][21]>0,"green.png","red.png")), interpolate=TRUE), xmin=1, xmax=1.1, ymin=por[8]-0.1, ymax=por[8]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][22]>0,"green.png","red.png")), interpolate=TRUE), xmin=1, xmax=1.1, ymin=por[9]-0.1, ymax=por[9]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][23]>0,"green.png","red.png")), interpolate=TRUE), xmin=1, xmax=1.1, ymin=por[10]-0.1, ymax=por[10]+0.1)+
        ylab("")+
        xlab("")+
        scale_x_continuous(limits = c(0.52,1.1))+
        scale_y_continuous(limits = c(0,10.5),expand = c(0, 0))+
        theme(
          legend.position = "none",
          panel.grid = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(-1.4, "line"),
          plot.margin=unit(c(0,0,0,0,0), "cm")
        ) 
      
      p5 <- grid.arrange(ggplot() +
      {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][2],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][2],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][3],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][3],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][4],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][4],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][5],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][5],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][6],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][6],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][7],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][7],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][8],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][8],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][9],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][9],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][10],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][10],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][11],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][11],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ncol=1)
      p6 <- grid.arrange(ggplot() +
      {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][14],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][14],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][15],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][15],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][16],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][16],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][17],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][17],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][18],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][18],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][19],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][19],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][20],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][20],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][21],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][21],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][22],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][22],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ggplot() +
        {if(file.exists(paste0("persons/",stringi::stri_trans_general(data[[1]][23],"Ukrainian-Latin/BGN"),".png")))annotation_custom(rasterGrob(readPNG(paste0("persons/",stringi::stri_trans_general(data[[1]][23],"Ukrainian-Latin/BGN"),".png")), interpolate=TRUE), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)} +
        geom_point(),ncol=1)
      p7 <- ggplot()+
        geom_text(aes(1,por,label=ifelse(!is.na(as.numeric(data[[4]][2:11])),paste0(round(as.numeric(data[[4]][2:11])*100),"%"),"∞")),family = "PT Sans",hjust=0,
                  color="#636566",size=6*3) +
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][2]>0,"green.png","red.png")), interpolate=TRUE), xmin=0.9, xmax=1, ymin=por[1]-0.1, ymax=por[1]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][3]>0,"green.png","red.png")), interpolate=TRUE), xmin=0.9, xmax=1, ymin=por[2]-0.1, ymax=por[2]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][4]>0,"green.png","red.png")), interpolate=TRUE), xmin=0.9, xmax=1, ymin=por[3]-0.1, ymax=por[3]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][5]>0,"green.png","red.png")), interpolate=TRUE), xmin=0.9, xmax=1, ymin=por[4]-0.1, ymax=por[4]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][6]>0,"green.png","red.png")), interpolate=TRUE), xmin=0.9, xmax=1, ymin=por[5]-0.1, ymax=por[5]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][7]>0,"green.png","red.png")), interpolate=TRUE), xmin=0.9, xmax=1, ymin=por[6]-0.1, ymax=por[6]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][8]>0,"green.png","red.png")), interpolate=TRUE), xmin=0.9, xmax=1, ymin=por[7]-0.1, ymax=por[7]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][9]>0,"green.png","red.png")), interpolate=TRUE), xmin=0.9, xmax=1, ymin=por[8]-0.1, ymax=por[8]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][10]>0,"green.png","red.png")), interpolate=TRUE), xmin=0.9, xmax=1, ymin=por[9]-0.1, ymax=por[9]+0.1)+
        annotation_custom(rasterGrob(readPNG(ifelse(data[[4]][11]>0,"green.png","red.png")), interpolate=TRUE), xmin=0.9, xmax=1, ymin=por[10]-0.1, ymax=por[10]+0.1)+
        ylab("")+
        xlab("")+
        scale_x_continuous(limits = c(0.9,1.52))+
        scale_y_continuous(limits = c(0,10.5),expand = c(0, 0))+
        theme(
          legend.position = "none",
          panel.grid = element_blank(), 
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(-1.4, "line"),
          plot.margin=unit(c(0,0,0,0,0), "cm")
        ) 
      
      grid.arrange(p2,p4,p6,p3,p5,p7,p1,ggplot(),
                   layout_matrix=cbind(c(1,1,1,1,1,1,1,1,1,1,1,1,1), 
                                       c(2,2,2,2,2,2,2,2,2,2,2,2,8),
                                       c(3,3,3,3,3,3,3,3,3,3,3,3,8),
                                       c(4,4,4,4,4,4,4,4,4,4,4,4,8),
                                       c(5,5,5,5,5,5,5,5,5,5,5,5,8),
                                       c(6,6,6,6,6,6,6,6,6,6,6,6,8),
                                       c(7,7,7,7,7,7,7,7,7,7,7,7,7)),
                   widths=c(3,0.67,0.65,1,0.65,0.67,3))
    }
    infoflow(data)
  })
  
  output$plot <- renderPlot({
    tryCatch(df2())
  })
  
  output$down <- downloadHandler(
    filename = function(){
      paste0("infoflow-",Sys.Date(),".pdf")
    },
    content = function(file) {
      cairo_pdf(file, width=16.98, height=10.93,bg = "white")
      grid.draw(df2())
      dev.off()
    }
  )
  
  output$do <- downloadHandler(
    filename = function(){
      paste0("infoflow-",Sys.Date(),".png")
    },
    content = function(file) {
      png(file, width=3666, height=2358,bg = "white")
      grid.draw(df2())
      dev.off()
    }
  )
  session$onSessionEnded(stopApp)
}
options(shiny.trace=TRUE)
shinyApp(ui,server)