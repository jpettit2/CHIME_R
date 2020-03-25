shinyServer(function(input, output, session) {

#
#General Calculations
total_infections <- reactive({as.numeric(input$i_hosp) / (as.numeric(input$i_market_share)/100) / (as.numeric(input$i_hosp_rate)/100)})

detection_prob <-  reactive({
    req(total_infections())
    
    input$i_inf / total_infections()})

# Susceptible (take infected out from initial susceptible population in future perhaps)
S   <- reactive({ 
    req(I()) 
    input$i_S #- I()
})

# Infected 
I   <- reactive({
    req(detection_prob())
    
    input$i_inf / detection_prob()})

# Growth Rate
intrinsic_growth_rate <- reactive({ 2^(1 / input$i_doubling) - 1})

# inverse of rec. days
gamma_p <- reactive({  1 / 14 # 1/recovery days
})
# 
# # Contact rate, beta
beta <- reactive({  (intrinsic_growth_rate() + gamma_p()) / S() * (1- as.numeric(input$i_rel_contact)/100)}) # {rate based on doubling time} / {initial S}

# 
r_t <- reactive({ beta() / gamma_p() * S() })# r_t is r_0 after distancing

r_naught <- reactive({ r_t() / (1-(as.numeric(input$i_rel_contact)/100))})

doubling_time_t <- reactive({ 1/log2(beta()*S() - gamma_p() +1)}) # doubling time after distancing


df_forecast <- #eventReactive(input$submit,
    df_forecast <-  reactive({
        
        req(S())
        req(I())
        req(beta())
        req(gamma_p())
        req(input$i_n_days)
        
        n_days <- input$i_n_days
        
        #initialize N
        N = S()+I()+0 #R is zero initially
        
        #initialize dataframe with first entry  
        df_sir<- sir(S = S(), I= I() , R = 0,beta = beta(),gamma_p = gamma_p(),N = N,day = 1)
        
        for(i in 2:n_days){
            
            N = df_sir$Sn[i-1] + df_sir$In[i-1] + df_sir$Rn[i-1]
            
            df_temp<-sir(df_sir$Sn[i-1], df_sir$In[i-1], df_sir$Rn[i-1], beta = beta(),gamma_p = gamma_p(),N = N, day = i)
            
            df_sir <- merge(df_sir,df_temp,all = T)
            
        }
        
        #calculate admits
        
        
        hosp <- df_sir$In*(input$i_market_share/100)*(input$i_hosp_rate/100)+df_sir$Rn*(input$i_market_share/100)*(input$i_hosp_rate/100)
        icu <- df_sir$In*(input$i_market_share/100)*(input$i_icu_rate/100)+df_sir$Rn*(input$i_market_share/100)*(input$i_icu_rate/100)
        vent <- df_sir$In*(input$i_market_share/100)*(input$i_vent_rate/100)+df_sir$Rn*(input$i_market_share/100)*(input$i_vent_rate/100)
        
        days <- df_sir$Day
        
        df_admits <- data.frame(days,hosp,icu,vent)
        
        df_admits <- df_admits %>% 
            dplyr::mutate(hosp_admits = floor(hosp - lag(x = hosp, n = 1,default = df_admits$hosp[1])),
                          icu_admits = floor(icu - lag(x = icu, n = 1,default = df_admits$icu[1])),
                          vent_admits = floor(vent - lag(x = vent, n = 1,default = df_admits$vent[1])))
        
        df_admits[df_admits<0]<-0
        
        
        #Calculate census
        req(input$i_hosp_los)
        req(input$i_icu_los)
        req(input$i_vent_los)
        
        df_admits$hosp_census<- rollapplyr(data = df_admits$hosp_admits,width =as.numeric(input$i_hosp_los), FUN = sum, align = "right",fill = 0, partial = TRUE)
        df_admits$icu_census<-  rollapplyr(data = df_admits$icu_admits,width =as.numeric(input$i_icu_los), FUN = sum, align = "right",fill = 0, partial = TRUE)   
        df_admits$vent_census<- rollapplyr(data = df_admits$vent_admits,width =as.numeric(input$i_vent_los), FUN = sum, align = "right",fill = 0, partial = TRUE) 
        
        
        
        df_admits$hosp_census_curr <- input$i_hosp_census
        df_admits$icu_census_curr <- input$i_icu_census
        df_admits$vent_census_curr <- input$i_vent_census
        df_admits$hosp_capacity <- input$i_hosp_cap
        df_admits$icu_capacity <- input$i_icu_cap
        df_admits$vent_capacity <- input$i_vent_cap
        
        df_admits
    })




#Button to click when ready to generate forecast




###########################################
# tiAnalysis
###########################################

# server code for the tiAnalysis tab goes here...

output$plot_admits <- renderPlotly({
    req(df_forecast())
    
    
    df_admits <- df_forecast()
    
    plot_ly(data = df_admits,x=~days) %>% 
        add_trace(y = ~hosp_admits, name = "Hospital",mode="none",fill = 'tozeroy') %>% 
        add_trace(y = ~icu_admits,name="ICU Admits", mode="none",fill = 'tozeroy') %>% 
        add_trace(y = ~vent_admits, name = "Vent. Admits",mode="none", fill = 'tozeroy') %>% 
        layout(title = 'Daily Admissions',
               xaxis = list(title = "Days"),
               yaxis = list(title = 'Number of Cases'),
               hovermode = 'compare',
               colorway = c('orange',  'green',  'blue'))
    
    
})


output$plot_census <- renderPlotly({
    req(df_forecast())
    
    df_admits <- df_forecast()
    
    plot_ly(data = df_admits,x=~days,mode = 'none') %>% 
        add_trace(y = ~hosp_census, name = "Hospital",mode = "none", fill = 'tozeroy') %>% 
        add_lines(y =~hosp_capacity, name = "Hospital Capacity") %>%
        add_trace(y = ~icu_census,name="ICU Beds Needed",mode = "none", fill = 'tozeroy') %>% 
        add_lines(y =~icu_capacity, name = "ICU Capacity") %>% 
        add_trace(y = ~vent_census, name = "Vents Needed",mode = "none", fill = 'tozeroy') %>% 
        add_lines(y =~vent_capacity, name = "Vent. Capacity") %>%
        layout(title = 'Census',
               xaxis = list(title = "Days"),
               yaxis = list(title = 'Number of Cases'),
               hovermode = 'compare',
               colorway = c('orange', 'orange', 'green', 'green', 'blue', 'blue'))
    
})



df_report <- reactive({
    req(df_forecast())
    
    df <- df_forecast()[c(1,5:10)]#input$i_hosp_los:input$i_n_days,
    #as.numeric(input$i_vent_los)
    names(df) <- c("Day", "Expected Daily Hospital Admits", "Expected Daily ICU Admits", "Expected Daily New Vent Patients", "Expected Hospital Census", "Expected ICU Census", "Expected Vent Census")
    
    #df <- round(df)
    df
})



output$forecast <- renderDataTable({ 
    req(df_forecast())
    
    
    df <- df_forecast()[c(1,5:10)]
    
    names(df) <- c("Day", "Expected Daily Hospital Admits", "Expected Daily ICU Admits", "Expected Daily New Vent Patients", "Expected Hospital Census", "Expected ICU Census", "Expected Vent Census")
    
    
    df
},rownames= FALSE)



output$report <- downloadHandler(
    
    filename = paste(Sys.Date(),"Surge_data.csv",sep = "_"),
    
    
    content = function(file) {
        write.csv(df_report(), file, row.names = F)
    }
    
    
    
)





})
