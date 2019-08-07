#package yang diperlukan
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Randomize Group"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "sample_size",label = "Sample size",value = 50,min = 1,max = 1000), #input jumlah sampel, value = nilai yang pertama kali muncul
            numericInput(inputId = "group_size",label = "Number of group",value = 5,min = 1,max = 1000), #input jumlah kelompok
            actionButton(inputId = "start",label = "Start") #tombol start
        ),

        # Show a plot of the generated distribution
        mainPanel(
           downloadButton("download","download result"), #tombol download
           h4("Summary"), #heading
           tableOutput("sum_desc"), #tabel
           h4("Result"), #heading
           dataTableOutput("result") #tabel (fiturnya lebih lengkap)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

resul <- eventReactive(input$start,{ #hanya melakukan proses randomize ketika tombol start ditekan
    random_group <- function(n,group){
        #input
        sample_size <- n
        num_group <- group
        
        #number of people in each group
        n_per_group <- floor(sample_size/num_group)
        
        group_df <- data.frame(member = seq(1,n),
                               group = 0)
        
        max_group <- data.frame(group = seq(1,num_group),
                                max_member = n_per_group)
        
        for (i in 1:num_group) {
            if (sum(max_group$max_member)!=sample_size) {
                max_group$max_member[i] <- max_group$max_member[i]+1  
            }
        }
        
        #Assign people to group
        for (i in 1:sample_size) {
            group_df$group[i] <- round(runif(n = 1,min = 1,max = num_group))
            
            if (length(group_df[group_df$group %in% group_df$group[i],2]) > max_group[max_group$group %in% group_df$group[i],2]) {
                while (length(group_df[group_df$group %in% group_df$group[i],2]) > max_group[max_group$group %in% group_df$group[i],2]) {
                    group_df$group[i] <- round(runif(n = 1,min = 1,max = num_group))  
                }  
            }
        }
        return(group_df)
    }
    random_group(input$sample_size,input$group_size)
})

output$result <- renderDataTable({resul()}) #menampilkan rincian anggota kelompok

output$sum_desc <- renderTable({ #menampilkan ringkasan jumlah anggota di setiap kelompok
    rox <- resul()
    rox$group <- as.integer(rox$group)
    rox <- rox %>% group_by(group) %>% summarise(total = n()) #rekap (kalau di excel setara pivot table)
})

observeEvent("start",{ #apa yang terjadi ketika tombol start diklik
    resul()
})

output$download <- downloadHandler( #apa yang terjadi ketika tombol download diklik
    filename = function() {paste("random-", Sys.Date(), ".csv", sep="")},
    content = function(file) {
        obju <- resul()
        write.csv(obju, file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
