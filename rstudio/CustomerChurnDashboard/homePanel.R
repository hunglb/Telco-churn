# Sample Materials, provided under license.
# Licensed Materials - Property of IBM
# © Copyright IBM Corp. 2019. All Rights Reserved.
# US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.

clientButton <- function(id, name, risk, image) {
  tags$p(
    actionButton(paste0('client-btn-', id),
                 fluidRow(
                   column(3, 
                          tags$img(src = image, width = "100px", height = "100px")
                   ),
                   column(9,
                          tags$h3(name),
                          tags$h4(paste('Churn Risk: ', risk))
                   )
                 ),
                 style="width:100%"
    )
  )
}

homePanel <- function() {
  
  tabPanel(
    "Dashboard",
    tags$head(
      tags$style(HTML("
        .datatables {
          width: 100% !important;
        }
      "))
    ),
    shinyjs::useShinyjs(),
    
    fluidRow(
      column(5, panel(
        tags$h2("Top Action Clients"),
        tags$br(),
        lapply(clientIds, function(id){
          client <- clients[[toString(id)]]
          clientButton(id, client$name, client$risk, 
                       paste0("profiles/", client$image))
        })
      )),
      column(7, 
        panel(
          h2("Monthly Customer Churn"),
          ggiraphOutput("monthlyChurnPlot", width = "600px", height = "400px")
        ),
        panel(
           h2("Customers Churn Risk"),
           ggiraphOutput("customerRiskPlot", width = "600px", height = "400px")
         )
      )
    )
  )
}


# Monthly churn plot
monthlyChurnData <- data.frame(month=c("January", "February", "March", "April", "May", "June", "July", "August", "September"),
                               customers=c(4, 10, 9, 11, 13, 10, 8, 12, 9))
monthlyChurnPlot <- ggplot(data=monthlyChurnData, aes(x=reorder(month, 1:nrow(monthlyChurnData)), y=customers)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=customers), vjust=-0.3, size=3.5)+
    theme_minimal() +
    labs(x = "Month", y = "Customers Churned")


# Customer risk plot
riskLevels = c("Low Risk", "Medium Risk", "High Risk")
customerRiskData <- data.frame(riskLevel = riskLevels, count = c(302, 135, 42)) %>%
  mutate (
    hover_text = paste0(riskLevel, ": ", count)
  )
customerRiskPlot <- ggplot(customerRiskData, aes(y = count, fill = riskLevel)) +
  geom_bar_interactive(
    aes(x = 1, tooltip = hover_text),
    stat = "identity",
    show.legend = TRUE
  ) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=12)) +
  scale_fill_discrete(breaks=riskLevels)
customerRiskPlot <- customerRiskPlot + guides(fill=guide_legend(title="Risk Level"))


homeServer <- function(input, output, session, sessionVars) {
  
  # Observation events for client buttons
  lapply(paste0('client-btn-', clientIds),
         function(x){
           observeEvent(
             input[[x]],
             {
               id <- as.numeric(sub("client-btn-", "", x))
               sessionVars$selectedClientId <- id
               updateTabsetPanel(session, "lfeNav", selected = "clientPanel")
             }
           )
         })
  
  # Display plot
  output$monthlyChurnPlot <- renderggiraph(ggiraph(ggobj = monthlyChurnPlot, width_svg=6, height_svg=4))
  output$customerRiskPlot <- renderggiraph(ggiraph(ggobj = customerRiskPlot, width_svg=6, height_svg=4))
  
}
