# This is the server code for a simple shiny app that shows how reservoir managers 
#decision making influences reservoir storage

library(shiny)
library(ggplot2)

resOut<-function(int, R1p, R2p, q_in) {
    # a reservoir has a storage capacity of 100% Each "reservoir" corresponds to a different management scenario
    res<- matrix(data= NA, nrow = 12, ncol = 6)
    rc<-as.numeric(c(80,80,60,40,40,30,30,40,50,80,95,100))
     # generate run based on input$int from ui.R
    res[1,]<- int
    res[1:12,5]=c(1:12)
    res[,6]<-rc[1:12]
    # discharge is a funtion of the reservoir managers attributes
    q_out<- matrix(data= NA, nrow = 11, ncol = 4)
    
    for(i in 1:11){

      #if (res[i,1] >= rc[i+1]){
      #  q_out[i,1] = res[i,1]+q_in[i] - rc[i+1]
      #} else {q_out[i,1] = 0}
    
      #rules for less risk averse manager (always +5% of rule curve)
      R1pi<-(1+R1p/100)
      
      if (res[i,1] >= (rc[i+1]*R1pi)){
        q_out[i,1] = res[i,1]+q_in[i] - (rc[i+1]*R1pi)
      } else if (res[i,1] < rc[i+1]){q_out[i,1] = 0
      } else {q_out[i,1]=0}
      
      #rules for risk averse manager (always -5% of rule curve)
      R2pi<-(1-R2p/100)
      if (res[i,2] >= (rc[i+1]*R2pi)){
        q_out[i,2] = (res[i,2]+q_in[i]- (rc[i+1]*R2pi))
      } else if (res[i,2] <= rc[i+1]){q_out[i,2] = 0
      } else {q_out[i,2]=0}
      
      #rules for manager that are dependent on the month of the year
      if (res[i,3] >= rc[i+1] && i > 6){
        q_out[i,3] = 0
      } else if (res[i,3] >= rc[i+1] && i < 6 && i>2 ){q_out[i,3] = res[i,3] - (rc[i+1]*0.9)
      } else if (res[i,3] >= rc[i+1] && i < 3){q_out[i,3] = res[i,3]*0.55
      } else if (res[i,3] <= rc[i+1]){q_out[i,3] = 0
      } else {q_out[i,3]=0}
      
      #rules for manager that are based on reservoir level
      if (res[i,4] >= (rc[i+1]-(rc[i+1]*0.1))){
        q_out[i,4] = (res[i,4]*0.1)
      } else if (res[i,4] <= rc[i+1]){q_out[i,4] = 0
      } else {q_out[i,4]=0}
      
      #add sreamflow to current reservoir storage
      res[i+1,1]=res[i,1]+q_in[i]-q_out[i,1]
      res[i+1,2]=res[i,2]+q_in[i]-q_out[i,2]
      res[i+1,3]=res[i,3]+q_in[i]-q_out[i,3]
      res[i+1,4]=res[i,4]+q_in[i]-q_out[i,4]
    }
    resL<-data.frame(res, q_in)
    #return(resL)
}

shinyServer(function(input, output) {
  #generate values for random inflow to the reservoir and plot them
  #this button makes the stuff in the brackets happen
  Q_in <- eventReactive(input$q,{runif(12, min = 2, max = 20)})
  
  output$qplot <- renderPlot({
    
    monthLab<-c('October', 'November', 'Decmeber', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'Septmeber')
    Qdata<-data.frame(monthLab, Q_in())
    Qdata$months<- factor(Qdata$monthLab, levels = c('October', 'November', 'Decmeber', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'Septmeber'))
    
    ggplot(Qdata, aes(months, Q_in())) +
      geom_bar(stat='identity', width=.85, fill="blue") +
      theme(axis.text.x = element_text(size=12, angle=45))+
      scale_y_continuous(name="Inflow to Reservoir (% of total volume)", trans="reverse")
  })
  

  curRes<- reactive({ return(resOut(input$int,input$R1p, input$R2p, Q_in()))})
  #curRes$months<- factor(data$months, levels = c('October', 'November', 'Decmeber', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'Septmeber'))
  
  output$disPlot <- renderPlot({
    #monthLab<-c('October', 'November', 'Decmeber', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'Septmeber')
   
    ggplot(curRes(), aes(x=X5)) +
      geom_line(aes(y=X6), lwd=1.05) +
      geom_line(aes(y=X1), col="red") +
      geom_line(aes(y=X2), col="blue") +
      geom_line(aes(y=X3), col="forestgreen") +
      geom_line(aes(y=X4), col="lightskyblue") +
      xlab("Month") +
      ylab("Reservoir Level (%)") 
      #ylim(0,100)
      #scale_x_discrete(breaks =c("1","2","3","4","5","6","7","8","9","10","11","12"), labels= monthLab)
    
   })
  
})
