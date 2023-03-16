box::use(
  app/logic/data
)

box::use(
  dplyr[mutate,select,summarise,group_by],
  ggplot2[ggplot,geom_line,aes,theme,labs,element_text,margin,
          element_rect],
  shiny[plotOutput,renderPlot,NS,moduleServer]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  plotOutput(ns("sbc"))
}

#' @export
server <- function(id){
  moduleServer(id,function(input,output,session){
    
    output$sbc <- renderPlot({
      
        data$fetch_data() |>
        mutate(date = as.Date(date)) |>
        select(date,country,num_sold) |>
        group_by(date,country) |>
        summarise(num_sold = mean(num_sold),.groups = "drop") |>
        ggplot() +
        geom_line(mapping = aes(x = date,y = num_sold,group = as.factor(country),color = as.factor(country))) +
        theme(legend.position = "bottom",
              axis.text = element_text(size = 15),
              axis.title.y =element_text(size=14,margin = margin(0,10,0,0)),
              panel.background = element_rect(fill = NA)) +
        labs(x = "",
             y = "Books Sold",
             color = "Country")
      
      
    })
    
  })
  
}




