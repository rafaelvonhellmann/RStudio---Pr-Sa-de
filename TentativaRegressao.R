#### Regressão e Pressão 

        attach(DFPAC)

        modelo <- lm(PASM ~ CCor + CGI, data = DFPAC)
        
        summary(modelo)
        
        
        
        
        
        GSSC <- ggplot(DFPAC, aes(x= as.numeric (PASM), y =Sexo, label = "Oi")) + 
                geom_density(alpha=.3) + geom_text(size = 10) + 
                scale_x_continuous (name = "Pressão Arterial Sistólica Média (45-59 Anos)", limits = c(0, 300)) + scale_y_continuous(name = "Probabilidade", limits = c(0, 1)) + ggtitle("PASM e Sexo (45-59 anos") +
                annotate("rect", xmin = -0.002, xmax = 0.102, ymin = -0.056, ymax = -0.044, fill="white", colour="red") + geom_text ()
                annotate("text", x = 0.05, y = -0.05, parse = TRUE) +
                theme_economist() +
                theme(axis.line.x = element_line(size=.5, colour = "black"),
                      axis.line.y = element_line(size=.5, colour = "black"),
                      axis.text.x=element_text(colour="black", size = 9),
                      axis.text.y=element_text(colour="black", size = 9),
                      panel.grid.major = element_line(colour = "#d3d3d3"),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(), panel.background = element_blank(),
                      plot.title = element_text(family = "OfficinaSanITC-Book"),
                      text=element_text(family="OfficinaSanITC-Book"))
        
        
        library(ggthemes)
        library(ggplot2)
        library(grid)
        
        theme_economist(base_size = 10, base_family = "sans", horizontal = TRUE,
                        dkpanel = FALSE, stata = FALSE)
        
        theme_economist_white(base_size = 11, base_family = "sans",
                              gray_bg = TRUE, horizontal = TRUE)
        
        fill <- "#4271AE"
        line <- "#1F3552"
        
        p11 <- ggplot(DFPAC, aes(x=as.numeric (PASM), y=Sexo)) + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
                ggtitle("PASM e Sexo (45-59 anos") +
        
                annotate("rect", xmin = -0.002, xmax = 0.102, ymin = -0.056, ymax = -0.044, fill="white", colour="red") +
                annotate("text", x = 0.05, y = -0.05, parse = TRUE) +
                theme_economist() +
                theme(axis.line.x = element_line(size=.5, colour = "black"),
                      axis.line.y = element_line(size=.5, colour = "black"),
                      axis.text.x=element_text(colour="black", size = 9),
                      axis.text.y=element_text(colour="black", size = 9),
                      panel.grid.major = element_line(colour = "#d3d3d3"),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(), panel.background = element_blank(),
                      plot.title = element_text(family = "OfficinaSanITC-Book"),
                      text=element_text(family="OfficinaSanITC-Book"))