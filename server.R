library(dplyr)
library(scales)

shinyServer(function(input, output) {
  
  output$table1 <- renderDataTable({
    sub <- subset(INV,OFFICE%in%c(input$OFF))
    sub1 <- subset(sub,DEPT%in%c(input$DEPT))
    sub2 <- subset(sub1,Manager%in%(input$MGR))
    
    collect <- armast %>%
      select(c(1,3)) %>%
      rename("DSK #"=DESK)
    library(plyr)
    INVMAST <- join(sub2,collect,by="DSK #",match="first")
    detach("package:plyr", unload=TRUE)
    
    INVMAST$Employee <- as.factor(INVMAST$Employee)
    
    DSKUNW <- INVMAST %>%
      group_by(DESK,MGR,Employee,Manager) %>%
      summarise(Number_of_Accounts=n(),
                Action_Days_Count = sum(Action_Days == 1),
                Seven_Days_Count = sum(Seven_Days == 1),
                Thirty_Days_Count = sum(Thirty_Days == 1)) %>%
      ungroup()
    Totals <- DSKUNW %>%
      summarise(Number_of_Accounts=sum(Number_of_Accounts),
                Action_Days_Count = sum(Action_Days_Count,na.rm=T),
                Seven_Days_Count = sum(Seven_Days_Count,na.rm=T),
                Thirty_Days_Count = sum(Thirty_Days_Count,na.rm=T))
    Totals$DESK<-'Total'
    Totals$MGR<-''
    Totals$Employee <- ''
    Totals$Manager <- ''
    
    DSKUNW <- rbind(DSKUNW,Totals)
    DSKUNW <- rename(DSKUNW,
                     ">Action_Days_Count"=Action_Days_Count,
                     ">7_Days_Count"=Seven_Days_Count,
                     ">30_Days_Count"=Thirty_Days_Count)
    
    DSKUNW
  })
  
  output$table2 <- renderDataTable({
    sub1 <- subset(INV,DEPT%in%c(input$DEPT2))
    
    STATICA <- sub1 %>%
      group_by(STAT) %>%
      summarise(Count= n(),
                Balance=(round(sum(BAL),2)))
    
    STATICA$Balance <- dollar(STATICA$Balance)
    STATICA
  })
  
  output$table3 <- renderDataTable({
    SUB <-subset(INV,DEPT%in%c(input$DEPT3))
    
    DailyDetail <- SUB %>%
    group_by(OFFICE) %>%
    summarise(Number_of_Accounts=n(),
              Action_Days_Count = sum(Action_Days == 1,na.rm=T),
              Action_Days_Percent = paste(round(Action_Days_Count/Number_of_Accounts*100,2),"%"),
              Seven_Days_Count = sum(Seven_Days == 1,na.rm=T),
              Seven_Days_Percent = paste(round(Seven_Days_Count/Number_of_Accounts*100,2),"%"),
              Thirty_Days_Count = sum(Thirty_Days == 1,na.rm=T),
              Thirty_Days_Percent = paste(round(Thirty_Days_Count/Number_of_Accounts*100,2),"%"))
  DailyDetail <- rename(DailyDetail,"%_of_INV"=Action_Days_Percent,"%_of_INV"=Seven_Days_Percent,"%_of_INV"=Thirty_Days_Percent)
  DailyDetail
  
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(INV, file)
    }
  )
  
})
