
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(plotly)
library(ggplot2)
library(ggthemes)
library(scales)
library(tidyr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)

# DATA WRANGLING ----------------------------------------------------------
# 
# 
# Selvbetjeningslaan <- read_excel("N:/FIT2/Desktop/Mobillån Dashboard/Selvbetjeningslaan.xlsx")
# 
# laan <- Selvbetjeningslaan %>% 
#   group_by(SAGSLBNR_I) %>% 
#   filter(min(DSA_START_TS)>="2016-12-01") %>%  #12/01-2016 blev mobillån åbnet for hele banken
#   ungroup() %>% 
#   group_by(SAGSLBNR_I) %>% 
#   filter(DSA_START_TS==max(DSA_START_TS)) %>% 
#   ungroup() %>% 
#   distinct(SAGSLBNR_I, .keep_all=T)
# 
# #Dan en køn variabel
# laan <- laan %>% 
#   mutate(kon=ifelse(INTERESSENTNR_I %% 2 == 1, "Mand", "Kvinde")) # Virksomheder bliver også tildelt køn her
# 
# 
# # Dan en kategorisk variabel på ansøgt beløb
# levels <- c( -Inf, 10000, 20000, 30000, 40000, 50000, Inf)
# labels <- c("0-10.000 kr.", "10.001-20.000 kr.", "20.001-30.000 kr.", "30.001-40.000 kr.", "40.001-50.000 kr.", "> 50.000 kr.")
# 
# laan <- laan %>% 
#   mutate(Ansoegt = cut(ANSOEGT_V, levels, labels = labels))
#   
# # Indtjening pr. dag
# 
# indtj_dag <- laan %>%
#   mutate(DSA_START_DATO=as.Date(DSA_START_TS)) %>% 
#   filter(is.na(PERSONALE)&DSA_START_DATO>="2016-12-01") %>% 
#   group_by(DSA_START_DATO) %>% 
#   summarise(lån_pr_dag=n()) %>% 
#   ungroup() %>% 
#   mutate(indtj_pr_dag=lån_pr_dag*800)


# SERVER SIDE -------------------------------------------------------------

server <- function(input, output) {
  addClass(selector = "body", class = "sidebar-collapse")
  
  df <- reactive({
    if (input$df == "Spar Nord Svenstrup") {
      df <- df1
    } else {
      df <- df2
    }
    df
  })
  
  output$udlaan <- renderValueBox({
    valueBox(
      paste0(round(sum(laan$ANSOEGT_V)/1000000, digits = 2)," (",round(sum(laan$ANSOEGT_V[!is.na(laan$PERSONALE)])/1000000, digits=2) ,")"), "Samlet udlån(Personaleudlån) - mio.kr.", color="red"
    )
  })
  
  output$indtjening <- renderValueBox({
    valueBox(
      paste0(sum(is.na(laan$PERSONALE))*800), "Gebyrer - kr.", color="red"
    )
  })
  
  output$udlaanpersonale <- renderValueBox({
    valueBox(
      paste0(round(sum(laan$ANSOEGT_V[!is.na(laan$PERSONALE)])/1000000, digits=2)),"Samlet udlån, Personale - mio.kr.",
      color = "red"
    )
  })
  

  output$antal_laan <- renderValueBox({
    valueBox(
      paste0(length(laan$SAGSLBNR_I)," (", length(laan$SAGSLBNR_I[!is.na(laan$PERSONALE)]),")") ,"Antal lån (Optaget af personale)",
      color = "navy"
    )
  })  
  
  
  output$knd_plot <- renderPlotly({
    
    
    alder <- laan %>% 
      filter(is.na(PERSONALE)&KND_ALDER_A>0) %>% 
      distinct(INTERESSENTNR_I, .keep_all=T)
    
    
    p <- ggplot(alder, aes(kon, KND_ALDER_A))+theme_economist_white(gray_bg=F, horizontal = F)+ylab("Alder")+xlab("")+geom_boxplot(fill="#cc0000", width = 0.2, outlier.colour ="#cc0000" )+theme(axis.text.y=element_text(size=14))+theme(panel.grid.major.y=element_line(), panel.grid.major.x=element_blank(), axis.title=element_text())+scale_y_continuous(limits = c(0,75),breaks=seq(0,70,10), expand = c(0,0))+scale_fill_manual(values=c("#cc0000"))+theme(legend.position = 'none', axis.ticks.x=element_blank(),plot.title = element_text(vjust = 4))
    
    ggplotly(p)
    
    
  })
  
  output$tid_plot <- renderPlotly({
    
    
    scat <- laan %>% 
      filter(is.na(PERSONALE)&KND_ALDER_A>0&ANSOEGT_V>0) %>% 
      distinct(INTERESSENTNR_I, .keep_all=T)
    
    
    p <- ggplot(scat, aes(KTOENGKTODEBSALDO1plusALM_INDL,ANSOEGT_V ))+theme_economist_white(gray_bg=F, horizontal = F)+ylab("Ansøgt Beløb, kr.")+xlab("Uudnyttet kredit+Indlån")+geom_point(color="#cc0000")+theme(axis.text.y=element_text(size=14))+theme(panel.grid.major.y=element_line(), panel.grid.major.x=element_blank(), axis.title=element_text())+scale_y_continuous(limits = c(0,max(scat$ANSOEGT_V)+10000),breaks=seq(0,max(scat$ANSOEGT_V)+10000,10000), expand = c(0,0))+scale_x_continuous(limits = c(0,200000),breaks=seq(0,200000,50000))+theme(legend.position = 'none', axis.ticks.x=element_blank(),plot.title = element_text(vjust = 4))
    
    ggplotly(p)
    
    
    
  })
  
  output$ansogt_plot <- renderPlotly({
    
    
    ansogt <- laan %>% 
      group_by(Ansoegt) %>% 
      summarise(Antal=n())
    
    
    p <- ggplot(ansogt, aes(Ansoegt, Antal))+theme_economist_white(gray_bg=F, horizontal = F)+ylab("Antal lån")+xlab("")+geom_bar(fill="#cc0000", width = 0.2, stat="identity")+theme(axis.text.y=element_text(size=14), axis.text.x = element_text(size=8))+theme(panel.grid.major.y=element_line(), panel.grid.major.x=element_blank(), axis.title=element_text())+scale_y_continuous(limits = c(0,max(ansogt$Antal)+10),breaks=seq(0,max(ansogt$Antal),10), expand = c(0,0))+scale_fill_manual(values=c("#cc0000"))+theme(legend.position = 'none', axis.ticks.x=element_blank(),plot.title = element_text(vjust = 4))
    
    ggplotly(p)
    
    
  })
  
  output$indtj_plot <- renderPlotly({
    
    
    p <- ggplot(indtj_dag, aes(DSA_START_DATO, indtj_pr_dag))+theme_economist_white(gray_bg=F, horizontal = F)+ylab("Kr.")+xlab("")+geom_line(size=1.2, color="#cc0000")+theme(axis.text.y=element_text(size=14), axis.text.x = element_text(size=8))+theme(panel.grid.major.y=element_line(), panel.grid.major.x=element_blank(), axis.title=element_text())+scale_y_continuous(limits = c(0,16000),breaks=seq(0,15000,2500), expand = c(0,0))+scale_fill_manual(values=c("#cc0000"))+theme(legend.position = 'none', axis.ticks.x=element_blank(),plot.title = element_text(vjust = 4))
    
    ggplotly(p)
    
    
  })
  
  output$laantype_plot <- renderPlotly({

    laan$SAGSNAVN_N[grepl("kredit", laan$SAGSNAVN_N)] <-'Kredit'
    laan$SAGSNAVN_N[grepl("lån", laan$SAGSNAVN_N)] <-'Lån'
    laan$SAGSNAVN_N[grepl("forhøjelse", laan$SAGSNAVN_N)] <-'Forhøjelse'
    
    
    laantype <- laan %>% 
      group_by(SAGSNAVN_N) %>% 
      summarise(Antal=n())
    
    p <- ggplot(laantype, aes(SAGSNAVN_N, Antal))+theme_economist_white(gray_bg=F, horizontal = F)+ylab("Antal")+xlab("")+geom_bar(fill="#cc0000", width = 0.2, stat="identity")+theme(axis.text.y=element_text(size=14), axis.text.x = element_text(size=8))+theme(panel.grid.major.y=element_line(), panel.grid.major.x=element_blank(), axis.title=element_text())+scale_y_continuous(limits = c(0,105),breaks=seq(0,100,10), expand = c(0,0))+scale_fill_manual(values=c("#cc0000"))+theme(legend.position = 'none', axis.ticks.x=element_blank(),plot.title = element_text(vjust = 4))
    
    ggplotly(p)
    
    
  })
  
}

shinyApp(ui, server)
