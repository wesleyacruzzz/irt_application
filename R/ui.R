fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style1.css")
    ),
    argonDashPage(
        sidebar = argonDashSidebar(
            vertical = FALSE,
            skin = 'dark',
            background = "white",
            size = "md",
            #side = "left",
            id = "my_sidebar",
            brand_url = "http://theartandscienceofdata.wordpress.com/blog",
            brand_logo = "logo.jpg"
        ),
        footer = argonDashFooter(
            argonFooterMenu(
                argonFooterItem("X2 Soluções Estatísticas ®")
            )
        ),
        
        body = argonDashBody(
            wellPanel(uiOutput("nav"))
        )
    )
    
)