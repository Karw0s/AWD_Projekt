#
# Autor: Michal Karwowski
# Grupa: I5B3S1
# Temat: Ilolsc pojazdow w Polsce na przestrzeni 2009 - 2016
#

library(shiny)
library(sqldf)
library(scales)
library(ggplot2)
library(readxl)
library(plotly)

#wczytywanie pliku z danymi
daneXLS <- read_excel("data/dane.xls", sheet = "dane")

#tworzenie tabel z rodzajami pojazdow
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

# Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel("Ilolść pojazdów w Polsce"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            width = 3,
            
            
            
            helpText("Wybierz obszar zainteresowania"),
            selectInput(
                inputId = "obszar",
                label = "Obszar:",
                choices = c("Polska", "Wojewodztwa"),
                selected = "Polska"
            ),
            
            helpText("Wybierz rodzaj pojazdów"),
            selectInput(
                inputId = "rodzaj",
                label = "Rodzaje pojazdow",
                choices = c(
                    "Pojazdy samochodowe i ciagniki",
                    "Motocykle",
                    "Samochody osobowe",
                    "Autobusy",
                    "Samochody ciezarowe",
                    "Samochody ciezorowo - osobowe",
                    "Samochody specjalne (lacznie z sanitarnymi)",
                    "Ciagniki samochodowe",
                    "Ciagniki siodlowe",
                    "Ciagniki rolnicze",
                    "Motorowery",
                    "Motocykle o pojemnosci silnika do 125 cm3"
                )
            )
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            textOutput("selected_range"),
            tabsetPanel(
                type = "tabs",
                tabPanel("Wykres słupkowy", 
                         helpText("Wybierz przedział czasowy:"),
                         sliderInput(
                             "bins",
                             "Zakres czasu:",
                             min = 2009,
                             max = 2016,
                             value = c(2009, 2016),
                             sep = ""
                         ),
                         plotlyOutput("dispPlot", height = 600)),
                tabPanel(
                    "Wykres kołowy",
                    helpText("Wybierz przedział czasowy:"),
                    sliderInput(
                        "pieBins",
                        "Rok:",
                        min = 2009,
                        max = 2016,
                        value = 2016,
                        sep = ""
                    ),
                    plotOutput("dispPiePlot", height = 600)),
                tabPanel("Podgląd danych", tableOutput("table"))
            ), 
            width = 9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    przedzial_rok <- function(start_year, end_year) {
        index_one <- start_year - 2009
        if (start_year == end_year) {
            return((index_one + 1))
        } else {
            index_two <- end_year - start_year
            result <- (index_one + 1):((index_one + index_two) + 1)
            return(result)
        }
    }
  
    output$selected_range <- renderText({
        paste("Wybrano: ", input$rodzaj)
    })
    
    output$dispPlot <- renderPlotly({
        dane_plot <- switch (input$rodzaj,
                             "Pojazdy samochodowe i ciagniki" = pojazdy_samochodowe_i_ciagniki,
                             "Motocykle" = motocykle_ogolem,
                             "Samochody osobowe" = samochody_osobowe,
                             "Autobusy" = autobusy_ogolem,
                             "Samochody ciezarowe" = samochody_ciezarowe,
                             "Samochody ciezorowo - osobowe" = samochody_ciezarowo_osobowe,
                             "Samochody specjalne (lacznie z sanitarnymi)" = samochody_specjalne,
                             "Ciagniki samochodowe" = ciagniki_samochodowe,
                             "Ciagniki siodlowe" = ciagniki_siodlowe,
                             "Ciagniki rolnicze" = ciagniki_rolnicze,
                             "Motorowery" = motorowery,
                             "Motocykle o pojemnosci silnika do 125 cm3" = motocykle_o_pojemnosci_silnika_do_125_cm3
        )
        
        obszar_rozp <- switch (input$obszar,
                               "Polska" = 1,
                               "Wojewodztwa" = 2:17
        )
        
        isPolska <- switch (input$obszar,
                            "Polska" = TRUE,
                            "Wojewodztwa" = FALSE
        )
        
        powtorz = NULL
        lata_powt = NULL
        if (isPolska) {
            powtorz = 1
            lata_powt = 1
        } else {
            powtorz = 8
            lata_powt = 16
        }
        
        years <- input$bins
        
        rozpietosc <- years[2] - years[1]
        starting <- years[1] - 2009
        
        obszar_nazwa <- rep(dane_plot[obszar_rozp, 1], (rozpietosc + 1))
        
        rok = NULL
        
        for (i in przedzial_rok(years[1] - 1, years[2] - 1)) {
            rok = c(rok, rep(as.character(2009 + i), lata_powt))
        }
        
        srodek <- dane_plot[obszar_rozp, przedzial_rok(years[1] + 1, years[2] + 1)]
        
        liczba_sztuk = NULL
        
        for (variable in przedzial_rok(years[1] + 1, years[2] + 1)) {
            liczba_sztuk <- c(liczba_sztuk, dane_plot[obszar_rozp, variable])
        }
        
        dane_gg <- data.frame(obszar_nazwa, liczba_sztuk)
        
        dane_gg$woje <- factor(dane_gg$obszar_nazwa,
                               levels =  dane_plot[obszar_rozp, 1])

        p <- ggplot(dane_gg, aes(obszar_nazwa, liczba_sztuk)) +
            geom_bar(stat = "identity", aes(fill = rok), position = position_dodge(), color="black",size = .3) +
            theme(
                axis.text.x = element_text(
                    angle = 90,
                    face = "bold",
                    colour = "black",
                    hjust = 1
                ),
                axis.title.x = element_blank(),
                legend.title = element_text(face = "bold")
            ) +
            xlab("Obszar") +
            ylab("[szt]") +
            labs(fill= "Rok")+
            scale_y_continuous(labels = comma)
        p <- ggplotly(p) %>% layout(margin = list(b = 160), legend = list(x = 100, y = 0.5),
                                    xaxis = list(title = ""), yaxis = list(title = "[szt]"))
    })
    
    output$dispPiePlot <- renderPlot({
        dane_plot <- switch (input$rodzaj,
                             "Pojazdy samochodowe i ciagniki" = pojazdy_samochodowe_i_ciagniki,
                             "Motocykle" = motocykle_ogolem,
                             "Samochody osobowe" = samochody_osobowe,
                             "Autobusy" = autobusy_ogolem,
                             "Samochody ciezarowe" = samochody_ciezarowe,
                             "Samochody ciezorowo - osobowe" = samochody_ciezarowo_osobowe,
                             "Samochody specjalne (lacznie z sanitarnymi)" = samochody_specjalne,
                             "Ciagniki samochodowe" = ciagniki_samochodowe,
                             "Ciagniki siodlowe" = ciagniki_siodlowe,
                             "Ciagniki rolnicze" = ciagniki_rolnicze,
                             "Motorowery" = motorowery,
                             "Motocykle o pojemnosci silnika do 125 cm3" = motocykle_o_pojemnosci_silnika_do_125_cm3
        )
        
        range <- switch (input$obszar,
                         "Polska" = 1,
                         "Wojewodztwa" = 2:17)
        
        isPolska <- switch (input$obszar,
                            "Polska" = TRUE,
                            "Wojewodztwa" = FALSE)
        
        starting <- input$pieBins - 2009
        
        ggplot(dane_plot[range,],
               aes(
                   x = "",
                   y = dane_plot[range, starting + 2],
                   fill = dane_plot[range, 1]
               )) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start = 0) +
            xlab("") +
            ylab("[szt]") +
            labs(fill = "Obszar")+
            scale_y_continuous(labels = comma)
        
        
    })
    
    output$table <- renderTable({
        dane_plot <- switch (input$rodzaj,
                             "Pojazdy samochodowe i ciagniki" = pojazdy_samochodowe_i_ciagniki,
                             "Motocykle" = motocykle_ogolem,
                             "Samochody osobowe" = samochody_osobowe,
                             "Autobusy" = autobusy_ogolem,
                             "Samochody ciezarowe" = samochody_ciezarowe,
                             "Samochody ciezorowo - osobowe" = samochody_ciezarowo_osobowe,
                             "Samochody specjalne (lacznie z sanitarnymi)" = samochody_specjalne,
                             "Ciagniki samochodowe" = ciagniki_samochodowe,
                             "Ciagniki siodlowe" = ciagniki_siodlowe,
                             "Ciagniki rolnicze" = ciagniki_rolnicze,
                             "Motorowery" = motorowery,
                             "Motocykle o pojemnosci silnika do 125 cm3" = motocykle_o_pojemnosci_silnika_do_125_cm3
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)