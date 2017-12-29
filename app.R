#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Pojazdy w Polsce"), 
   
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      width = 3,
        
      h2("Wybierz przedział czasowy:"),
      sliderInput(
        "bins",
        "Zakres czasu:",
        min = 2009,
        max = 2016,
        value = c(2009, 2016),
        sep = ""
      ), 
        
        # dateRangeInput("dates",h3("Date range"), 
        #                format = "yyyy", 
        #                start = "2009-01-01", 
        #                min = "2009-01-01", 
        #                end = "2016-12-31", 
        #                max = "2016-12-31",
        #                startview = "year"),
      
      
      h2("Wybierz obszar zainteresowania"),
      selectInput(
        inputId = "obszar",
        label = "Obszar:",
        choices = c("Polska", "Wojewodztwo"),
        selected = "Polska"
      ), 
        
      h2("Wybierz rodzaj pojazdów"),
      selectInput(
        inputId = "rodzaj",
        label = "Rodzaje pojazdów",
        choices = c(
          "pojazdy samochodowe i ciagniki",
          "motocykle ogolem",
          "samochody osobowe",
          "autobusy ogolem",
          "samochody ciezarowe",
          "samochody ciezorowo - osobowe",
          "samochody specjalne (lacznie z sanitarnymi)",
          "ciagniki samochodowe",
          "ciagniki siodlowe",
          "ciagniki rolnicze",
          "motorowery",
          "motocykle o pojemnosci silnika do 125 cm3")
      )
      
    ), 
      
      # Show a plot of the generated distribution
    mainPanel(textOutput("selected_range"),
              
              plotOutput("distPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #wczytywanie pliku z danymi
  library(readxl)
  daneXLS <- read_excel("data/dane.xls", sheet = "dane")
  
  #tworzenie tabel z rodzajami pojazdow
  library(sqldf)
  
  library(ggplot2)
  
  pojazdy_samochodowe_i_ciagniki = sqldf("select 
                                         Nazwa,
                                         pojazdy_samochodowe_i_ciagniki_2009 as '2009',
                                         pojazdy_samochodowe_i_ciagniki_2010 as '2010',
                                         pojazdy_samochodowe_i_ciagniki_2011 as '2011',
                                         pojazdy_samochodowe_i_ciagniki_2012 as '2012',
                                         pojazdy_samochodowe_i_ciagniki_2013 as '2013',
                                         pojazdy_samochodowe_i_ciagniki_2014 as '2014',
                                         pojazdy_samochodowe_i_ciagniki_2015 as '2015',
                                         pojazdy_samochodowe_i_ciagniki_2016 as '2016'
                                         from daneXLS")

  motocykle_ogolem = sqldf("select 
                           Nazwa,
                           motocykle_ogolem_2009 as '2009',
                           motocykle_ogolem_2010 as '2010',
                           motocykle_ogolem_2011 as '2011',
                           motocykle_ogolem_2012 as '2012',
                           motocykle_ogolem_2013 as '2013',
                           motocykle_ogolem_2014 as '2014',
                           motocykle_ogolem_2015 as '2015',
                           motocykle_ogolem_2016 as '2016'
                           from daneXLS")

  samochody_osobowe = sqldf("select 
                            Nazwa,
                            samochody_osobowe_2009 as '2009',
                            samochody_osobowe_2010 as '2010',
                            samochody_osobowe_2011 as '2011',
                            samochody_osobowe_2012 as '2012',
                            samochody_osobowe_2013 as '2013',
                            samochody_osobowe_2014 as '2014',
                            samochody_osobowe_2015 as '2015',
                            samochody_osobowe_2016 as '2016'
                            from daneXLS")

  autobusy_ogolem = sqldf("select 
                          Nazwa,
                          autobusy_ogolem_2009 as '2009',
                          autobusy_ogolem_2010 as '2010',
                          autobusy_ogolem_2011 as '2011',
                          autobusy_ogolem_2012 as '2012',
                          autobusy_ogolem_2013 as '2013',
                          autobusy_ogolem_2014 as '2014',
                          autobusy_ogolem_2015 as '2015',
                          autobusy_ogolem_2016 as '2016'
                          from daneXLS")

  samochody_ciezarowe = sqldf("select 
                              Nazwa,
                              samochody_ciezarowe_2009 as '2009',
                              samochody_ciezarowe_2010 as '2010',
                              samochody_ciezarowe_2011 as '2011',
                              samochody_ciezarowe_2012 as '2012',
                              samochody_ciezarowe_2013 as '2013',
                              samochody_ciezarowe_2014 as '2014',
                              samochody_ciezarowe_2015 as '2015',
                              samochody_ciezarowe_2016 as '2016'
                              from daneXLS")

  samochody_ciezarowo_osobowe = sqldf("select 
                                      Nazwa,
                                      samochody_ciezarowo_osobowe_2009 as '2009',
                                      samochody_ciezarowo_osobowe_2010 as '2010',
                                      samochody_ciezarowo_osobowe_2011 as '2011',
                                      samochody_ciezarowo_osobowe_2012 as '2012',
                                      samochody_ciezarowo_osobowe_2013 as '2013',
                                      samochody_ciezarowo_osobowe_2014 as '2014',
                                      samochody_ciezarowo_osobowe_2015 as '2015',
                                      samochody_ciezarowo_osobowe_2016 as '2016'
                                      from daneXLS")

  samochody_specjalne = sqldf("select 
                              Nazwa,
                              samochody_specjalne_2009 as '2009',
                              samochody_specjalne_2010 as '2010',
                              samochody_specjalne_2011 as '2011',
                              samochody_specjalne_2012 as '2012',
                              samochody_specjalne_2013 as '2013',
                              samochody_specjalne_2014 as '2014',
                              samochody_specjalne_2015 as '2015',
                              samochody_specjalne_2016 as '2016'
                              from daneXLS")

  ciagniki_samochodowe = sqldf("select 
                               Nazwa,
                               ciagniki_samochodowe_2009 as '2009',
                               ciagniki_samochodowe_2010 as '2010',
                               ciagniki_samochodowe_2011 as '2011',
                               ciagniki_samochodowe_2012 as '2012',
                               ciagniki_samochodowe_2013 as '2013',
                               ciagniki_samochodowe_2014 as '2014',
                               ciagniki_samochodowe_2015 as '2015',
                               ciagniki_samochodowe_2016 as '2016'
                               from daneXLS")

  ciagniki_siodlowe = sqldf("select 
                            Nazwa,
                            ciagniki_siodlowe_2009 as '2009',
                            ciagniki_siodlowe_2010 as '2010',
                            ciagniki_siodlowe_2011 as '2011',
                            ciagniki_siodlowe_2012 as '2012',
                            ciagniki_siodlowe_2013 as '2013',
                            ciagniki_siodlowe_2014 as '2014',
                            ciagniki_siodlowe_2015 as '2015',
                            ciagniki_siodlowe_2016 as '2016'
                            from daneXLS")

  ciagniki_rolnicze = sqldf("select 
                            Nazwa,
                            ciagniki_rolnicze_2009 as '2009',
                            ciagniki_rolnicze_2010 as '2010',
                            ciagniki_rolnicze_2011 as '2011',
                            ciagniki_rolnicze_2012 as '2012',
                            ciagniki_rolnicze_2013 as '2013',
                            ciagniki_rolnicze_2014 as '2014',
                            ciagniki_rolnicze_2015 as '2015',
                            ciagniki_rolnicze_2016 as '2016'
                            from daneXLS")

  motorowery = sqldf("select 
                     Nazwa,
                     motorowery_2009 as '2009',
                     motorowery_2010 as '2010',
                     motorowery_2011 as '2011',
                     motorowery_2012 as '2012',
                     motorowery_2013 as '2013',
                     motorowery_2014 as '2014',
                     motorowery_2015 as '2015',
                     motorowery_2016 as '2016'
                     from daneXLS")

  motocykle_o_pojemnosci_silnika_do_125_cm3 = sqldf("select 
                                                    Nazwa,
                                                    motocykle_o_pojemnosci_silnika_do_125_cm3_2009 as '2009',
                                                    motocykle_o_pojemnosci_silnika_do_125_cm3_2010 as '2010',
                                                    motocykle_o_pojemnosci_silnika_do_125_cm3_2011 as '2011',
                                                    motocykle_o_pojemnosci_silnika_do_125_cm3_2012 as '2012',
                                                    motocykle_o_pojemnosci_silnika_do_125_cm3_2013 as '2013',
                                                    motocykle_o_pojemnosci_silnika_do_125_cm3_2014 as '2014',
                                                    motocykle_o_pojemnosci_silnika_do_125_cm3_2015 as '2015',
                                                    motocykle_o_pojemnosci_silnika_do_125_cm3_2016 as '2016'
                                                    from daneXLS")
  
  rok <- function(start_year, end_year){
    index_one <- start_year - 2009
    if (start_year==end_year) {
      return((index_one + 1))
    } else {
      index_two <- end_year - start_year
      result <- (index_one + 1) : ((index_one + index_two) + 1)
      return(result)
    }
  }
  
  output$selected_range <- renderText({
    paste("selected: ", input$bins)
  })
  
  output$selected_range <- renderText({
    paste("selected: ", input$bins)
  })
   
  
  output$distPlot <- renderPlot({
    dane_plot <- switch (input$rodzaj,
                        "pojazdy samochodowe i ciagniki" = pojazdy_samochodowe_i_ciagniki,
                        "motocykle ogolem" = motocykle_ogolem,
                        "samochody osobowe" = samochody_osobowe,
                        "autobusy ogolem" = autobusy_ogolem,
                        "samochody ciezarowe" = samochody_ciezarowe,
                        "samochody ciezorowo - osobowe" = samochody_ciezarowo_osobowe,
                        "samochody specjalne (lacznie z sanitarnymi)" = samochody_specjalne,
                        "ciagniki samochodowe" = ciagniki_samochodowe,
                        "ciagniki siodlowe" = ciagniki_siodlowe,
                        "ciagniki rolnicze" = ciagniki_rolnicze,
                        "motorowery" = motorowery,
                        "motocykle o pojemnosci silnika do 125 cm3" = motocykle_o_pojemnosci_silnika_do_125_cm3
    )
     
    obszar <- switch (input$obszar,
                      "Polska" = 1,
                      "Wojewodztwo" = 2:17
    )
    powtorz = 1
    lata_powt = 1
    if (obszar == 1) {
      powtorz <- 1
      lata_powt = 1
    }else {
      powtorz <- 8
      lata_powt = 16
    }
     
    years <- input$bins
    
    rozpietosc <- years[2] - years[1]
    starting <- years[1] - 2009
    
    x    <- t(dane_plot)
    colnames(x) = x[1, ]       # the first row will be the header
    x = x[-1, ]                # removing the first row.

    names_col <- colnames(dane_plot)
    names_col = names_col[-1]
    fffff <- data.frame(x[,2:17])
      #x[rok(years[1], years[2]), obszar]
    
     #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    
    woje <- rep(dane_plot[obszar, 1], powtorz)
    
    lata <-c(rep("2009", lata_powt),
             rep("2010", lata_powt),
             rep("2011", lata_powt),
             rep("2012", lata_powt),
             rep("2013", lata_powt),
             rep("2014", lata_powt),
             rep("2015", lata_powt),
             rep("2016", lata_powt))
    
    srodek <- dane_plot[obszar, rok(years[1]+1, years[2]+1)]
    vex = NULL
    for (variable in rok(years[1], years[2])) {
      vex <- c(vex, srodek[, variable])
    }
    # vex <-c(srodek[, 1],
    #         srodek[, 2],
    #         srodek[, 3],
    #         srodek[, 4],
    #         srodek[, 5],
    #         srodek[, 6],
    #         srodek[, 7],
    #         srodek[, 8])
    
    dane_gg <- data.frame(woje, vex)
    
    dane_gg$woje <- factor(dane_gg$woje,
                           levels =  dane_plot[2:17, 1])
    
    p <-ggplot(dane_gg, aes(woje, vex))
    p +
    geom_bar(stat = "identity", aes(fill = lata), position = "dodge" ) + 
    theme(axis.text.x = element_text(angle=90, face="bold", colour="black"))
    
    # barplot(data[obszar, rok(years[1], years[2])],
    #         
    #         names.arg = colnames(data[obszar, starting:(starting + rozpietosc)]),
    #         legend = names_col[rok(years[1], years[2])]
    #         #beside = TRUE
    #         #col = c("lightblue", "mistyrose", "lightcyan", "lavender") 
    # )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)