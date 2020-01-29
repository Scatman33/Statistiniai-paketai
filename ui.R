shinyUI(
  fluidPage(
    titlePanel(title = "This is my first shiny App"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Markeriai",
                 sliderInput("slider2", "SavivaldybiÅ³ markeriai pagal gyventoju skaiciu",
                                          min = 3000, max = 560000, value = c(3455, 552131)),
                 leafletOutput(outputId = "mymap1")),
        tabPanel("Filtras",
                 selectInput("selectas", "Filtravimas pagal kandidata",
                             choices = list("GIT", "ING", "VAL", "SAU"), selected = "GIT"),
                 dataTableOutput(outputId = "summary_table1")),
        tabPanel("Problema",
                 p("id - savivaldybės pavadinimas;"),
                 p("group - savivaldybę atitinkanti poligonų grupė;"),
                 p("GYV_SK - savivaldybėje gyvenančių žmonių skaičius;"),
                 p("LAIM - Lietuvos prezidento rinkimuose pirmame ture daugiausiai savivaldybėje iš kandidatų surinkusio kandidato trumpinys;"),
                 p("ARV - Arvydas Juozaitis;"),
                 p("GIT - Gitanas Nausėda;"),
                 p("ING - Ingrida Šimonytė;"),
                 p("MIN - Mindaugas Puidokas;"),
                 p("NAG - Naglis Puteikis;"),
                 p("SAU - Saulius Skvernelis;"),
                 p("VAL - Valentinas Mazuronis;"),
                 p("AND - Andriukaitis;"),
                 p("RESULT - šiame stulpelyje norima, kad pasirinkus šoninėje panelėje norimus kandidatus programa perskaičiuos šį stulpelį ir įrašys jame tą kandidato trumpinį, kuris daugiau surinko balsų savivaldybėje."),
                 p("Tada būtų galima nukreipti leaflet žemėlapio poligonų spalvinimą pagal šios lentelės rezultatus")),
        tabPanel("Sprendimas",
                 selectInput("selectas1", "Pirmas kandidatas",
                             choices = list("ARV", "GIT", "ING", "MIN", "NAG", "SAU", "VAL", "MAZ", "AND"), selected = "AND"),
                 selectInput("selectas2", "Antras kandidatas",
                             choices = list("ARV", "GIT", "ING", "MIN", "NAG", "SAU", "VAL", "MAZ", "AND"), selected = "ARV"),
                 dataTableOutput(outputId = "summary_table2")),
        tabPanel("Rezultatas 1",
                 selectInput("selectas1", "Pirmas kandidatas",
                             choices = list("ARV", "GIT", "ING", "MIN", "NAG", "SAU", "VAL", "MAZ", "AND"), selected = "AND"),
                 selectInput("selectas2", "Antras kandidatas",
                             choices = list("ARV", "GIT", "ING", "MIN", "NAG", "SAU", "VAL", "MAZ", "AND"), selected = "ARV"),
                 dataTableOutput(outputId = "summary_table3")),
        tabPanel("Rezultatas 2",
                 selectInput("selectas1", "Pirmas kandidatas",
                             choices = list("ARV", "GIT", "ING", "MIN", "NAG", "SAU", "VAL", "MAZ", "AND"), selected = "AND"),
                 selectInput("selectas2", "Antras kandidatas",
                             choices = list("ARV", "GIT", "ING", "MIN", "NAG", "SAU", "VAL", "MAZ", "AND"), selected = "ARV"),
                 leafletOutput(outputId = "mymap2")),
        
        tabPanel("Rezultatas 3",
                 selectInput("selectas1", "Pirmas kandidatas",
                             choices = list("ARV", "GIT", "ING", "MIN", "NAG", "SAU", "VAL", "MAZ", "AND"), selected = "AND"),
                 selectInput("selectas2", "Antras kandidatas",
                             choices = list("ARV", "GIT", "ING", "MIN", "NAG", "SAU", "VAL", "MAZ", "AND"), selected = "ARV"),
                 leafletOutput(outputId = "mymap3"),
                 dataTableOutput(outputId = "summary_table4")),
        
        tabPanel("Exp", leafletOutput(outputId = "mymap4"))
        )
      )
    )
  )
