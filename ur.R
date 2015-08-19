shinyUI(fluidPage(
  
  titlePanel("Inventory Status Dashboard"),downloadButton("downloadData", 'Download Data'),
  

    
    mainPanel(tabsetPanel(type="tab",
      tabPanel("Desk INV By Unworked Days",
               column(width=4,
                      checkboxGroupInput("OFF",
                                  "Office Select",
                                  choices=levels(INV$OFFICE),
                                  inline=T,
                                  selected=levels(INV$OFFICE)
              
                 
                 )),
               column(width=4,
                      checkboxGroupInput("DEPT",
                                         "Department Select",
                                         choices=levels(INV$DEPT),
                                         inline=T,
                                         selected=levels(INV$DEPT))  
                      
                      ),
               column(width=4,
                      selectInput("MGR",
                                  "Manager Select",
                                  choices=levels(INV$Manager),
                                  selected=levels(INV$Manager),
                                  size=3,
                                  multiple=T,
                                  selectize=F)
                      
                      ),
               dataTableOutput("table1")
    ),
    tabPanel("INV by Status",column(width=12,
                                       checkboxGroupInput("DEPT2",
                                                          "Department Select",
                                                          choices=levels(INV$DEPT),
                                                          inline=T,
                                                          selected=levels(INV$DEPT))), 
                                       dataTableOutput("table2")),
    tabPanel("Office totals",column(width=12,
                                    checkboxGroupInput("DEPT3",
                                                       "Department Select",
                                                       choices=levels(INV$DEPT),
                                                       inline=T,
                                                       selected=levels(INV$DEPT))), 
             dataTableOutput("table3"))

  )
)))
