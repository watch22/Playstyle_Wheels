

#Load Packages ----
pacman::p_load(tidyverse, StatsBombR, SBpitch, soccermatics, extrafont, ggupset, tibbletime, ggtext, ggrepel, glue, patchwork, cowplot, gtable, grid, magick, here, ggsoccer, janitor, rvest, ggimage)

#roll function
roll <- function( x , n ){
  if( n == 0 )
    return( x )
  c( tail(x,n) , head(x,-n) )
}

#Import Data ---
playstyle_averages <- read.csv(file = here("data", "The Athletic Playstyle Wheel_Averages.csv"))
playstyle <- read.csv(file = here("data", "The Athletic Playstyle Wheel_Full.csv"))

#Prepare Data ------
psw <- playstyle %>% 
  pivot_longer(-c(Club), names_to = "group", values_to = "value") %>% 
  rename(club = Club) %>% 
  mutate(metric_abb = substr(group,7,9),
         group_abb = substr(group,1,3),
         metric = case_when(metric_abb == "BLD" ~ "Build Up",
                                 metric_abb == "FLT" ~ "Field Tilt",
                                 metric_abb == "SFT" ~ "Safety",
                                 metric_abb == "PRG" ~ "Progressive Passes",
                                 metric_abb == "SWT" ~ "Switches",
                                 metric_abb == "DRB" ~ "Dribbling",
                                 metric_abb == "SET" ~ "Set Pieces",
                                 metric_abb == "CHN" ~ "Chance Creation",
                                 metric_abb == "HTR" ~ "High Transition",
                                 metric_abb == "STD" ~ "Start Distance",
                                 metric_abb == "CPR" ~ "Counter-Press",
                                 metric_abb == "HPR" ~ "High Press",
                                 metric_abb == "HID" ~ "High Defence",
                                 metric_abb == "LOD" ~ "Low Defence",
                                 metric_abb == "SPD" ~ "Set-Piece Defence"
                                 ),
         group = case_when(group_abb == "POS" ~ "Possess",
                           group_abb == "DIS" ~ "Disrupt",
                           group_abb == "FIN" ~ "Finish",
                           group_abb == "PRS" ~ "Press",
                           group_abb == "DEF" ~ "Defend")) %>% 
  select(club,group,metric,metric_abb,value)

psw$club <- as.factor(psw$club)
psw$club <- factor(psw$club,levels(psw$club)[c(12,11,6,17,1,13,19,10,4,20,14,7,3,2,16,8,9,5,18,15)])
clubs <- levels(psw$club)

psw$metric <- as.factor(psw$metric)
psw$metric <- factor(psw$metric,levels(psw$metric)[c(1,5,11,10,15,4,12,2,8,14,3,7,6,9,13)])
metrics <- levels(psw$metric)

psw_avg <- playstyle_averages %>% 
  pivot_longer(-c(Club), names_to = "group", values_to = "value") %>% 
  rename(club = Club)

psw_avg$club <- as.factor(psw_avg$club)
psw_avg$club <- factor(psw_avg$club,levels(psw_avg$club)[c(12,11,6,17,1,13,19,10,4,20,14,7,3,2,16,8,9,5,18,15)])

clubs <- levels(psw_avg$club)

#adjust label angles ----
#averages plots
groups <- unique(psw_avg$group)
#find the difference in angle between to labels and divide by two.
temp <- (360/(length(groups))/2)  
#get the angle for every label
myAng <- seq(-temp, -360 + temp, length.out = length(groups))
#rotate label by 180 in some places for readability
ang<-ifelse(myAng < -90, myAng + 180, myAng)
#rotate some labels back for readability
ang<-ifelse(ang < -90, ang + 180, ang)

#full plots
temp_full <- (360/(length(metrics))/2) 
myAng_full <- seq(-temp_full, -360 + temp_full, length.out = length(metrics))
#rotate label by 180 in some places for readability
ang_full <- ifelse(myAng_full < -90, myAng_full + 180, myAng_full)
#rotate some labels back for readability
ang_full <-ifelse(ang_full < -90, ang_full + 180, ang_full)

#Assign Colours & Fonts ---
bgcol <- "black"
textcol <- "white"
wheelcols <- c("Possess" = "#34DB99",
               "Disrupt" = "#968259",
               "Finish" = "#FCB703",
               "Press" = "#F34A4A",
               "Defend" = "#1A85CA")
bodyfont <- "Roboto Condensed"
titlefont <- "Zilla Slab"
linesize <- 0.5
opacity_main <- 1
opacity_sub <- 0.25
wheelalpha <- c("Possess" = opacity_main,
               "Disrupt" = opacity_main,
               "Finish" = opacity_main,
               "Press" = opacity_main,
               "Defend" = opacity_main)
wheelcolsfull <- c("Build Up" = "#34DB99", "Field Tilt" = "#34DB99", "Safety" = "#34DB99",
                   "Progressive Passes" = "#968259", "Switches" = "#968259", "Dribbling" = "#968259",
               "Set Pieces" = "#FCB703", "Chance Creation" = "#FCB703", "High Transition" = "#FCB703",
               "Start Distance" = "#F34A4A", "Counter-Press" = "#F34A4A", "High Press" = "#F34A4A",
               "High Defence" = "#1A85CA", "Low Defence" = "#1A85CA", "Set-Piece Defence" = "#1A85CA")


#Plot 1a: Team by Team // Group by Group -----
i <- 1
j <- 1

psw_club <- list()

for(j in 0:(length(groups) - 1)){

for(i in 1:length(clubs)){
  
psw_club[[i]] <- psw_avg %>%
  filter(club %in% clubs[i]) %>% 
  mutate(value = floor(value)) %>% 
ggplot(aes(x = factor(group, levels = c("Possess", "Disrupt", "Finish", "Press", "Defend")), y = value, label = group)) +
  geom_col(aes(fill = group, alpha = group), width = 1, colour = bgcol) + 
  scale_fill_manual(values = wheelcols, labels = rev(c("Bass", "Hats", "Kick","Pad", "Percussion"))) +
  scale_alpha_manual(values = roll(c(1,rep(opacity_sub,4)),j) * wheelalpha, guide = "none") +
  geom_text(y = 11.5,
            colour = textcol,
            family = bodyfont,
            angle = ang,
            size = 6) +
  #Annotate Lines
  geom_hline(yintercept = seq(-2, 9, by = 1), color = bgcol, size = linesize, alpha = 1) +
  geom_hline(yintercept = seq(0, 10, by = 10), color = textcol, size = linesize, alpha = 1) +
  annotate(geom = "line", x = c(0.5,0.5), y = c(0,12), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(1.5,1.5), y = c(0,12), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(2.5,2.5), y = c(0,12), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(3.5,3.5), y = c(0,12), colour = "white", size = linesize) + 
  annotate(geom = "line", x = c(4.5,4.5), y = c(0,12), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(5.5,5.5), y = c(0,12), colour = "white", size = linesize) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  coord_polar() +
  theme(plot.title = element_text(family = titlefont, colour = textcol, size = 30, face = "bold"),
        plot.subtitle = element_text(family = titlefont, colour = textcol, size = 25),
        plot.caption = element_text(family = bodyfont, colour = textcol, size = 12, hjust = 1, margin = margin(5, 5, 5, 5)),
        panel.background = element_rect(colour = bgcol, fill = bgcol),
        plot.background = element_rect(colour = bgcol, fill = bgcol),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(family = bodyfont, colour = textcol, size = 15),
        legend.background = element_rect(fill = bgcol, colour = bgcol),
        legend.key = element_rect(fill = bgcol, colour = bgcol))  +
  labs(title = paste("Averaged Playstyle Wheel"),
       subtitle = clubs[i],
       caption = paste("Data and style from", dQuote("Introducing playstyle wheels"), "by John Muller.", "\nPublished on The Athletic on May 18, 2022", sep = " "))

psw_club[[i]]

ggsave(psw_club[[i]], file = here::here("outputs", "Averaged", paste(i, " - ", clubs[i], " - ", groups[j+1] ,".png", sep = "")), 
       width = 20, height = 20, units = "cm", dpi = 600)
}
}

#Plot 1b: Team by Team // All groups -----
i <- 1

psw_club <- list()

  for(i in 1:length(clubs)){
    
    psw_club[[i]] <- psw_avg %>%
      filter(club %in% clubs[i]) %>% 
      mutate(value = floor(value)) %>% 
      ggplot(aes(x = factor(group, levels = c("Possess", "Disrupt", "Finish", "Press", "Defend")), y = value, label = group)) +
      geom_col(aes(fill = group, alpha = group), width = 1, colour = bgcol) + 
      scale_fill_manual(values = wheelcols, labels = rev(c("Bass", "Hats", "Kick","Pad", "Percussion"))) +
      scale_alpha_manual(values = wheelalpha, guide = "none") +
      geom_text(y = 11.5,
                colour = textcol,
                family = bodyfont,
                angle = ang,
                size = 6) +
      #Annotate Lines
      geom_hline(yintercept = seq(-2, 9, by = 1), color = bgcol, size = linesize, alpha = 1) +
      geom_hline(yintercept = seq(0, 10, by = 10), color = textcol, size = linesize, alpha = 1) +
      annotate(geom = "line", x = c(0.5,0.5), y = c(0,12), colour = "white", size = linesize) +
      annotate(geom = "line", x = c(1.5,1.5), y = c(0,12), colour = "white", size = linesize) +
      annotate(geom = "line", x = c(2.5,2.5), y = c(0,12), colour = "white", size = linesize) +
      annotate(geom = "line", x = c(3.5,3.5), y = c(0,12), colour = "white", size = linesize) + 
      annotate(geom = "line", x = c(4.5,4.5), y = c(0,12), colour = "white", size = linesize) +
      annotate(geom = "line", x = c(5.5,5.5), y = c(0,12), colour = "white", size = linesize) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(expand = c(0,0)) +
      coord_polar() +
      theme(plot.title = element_text(family = titlefont, colour = textcol, size = 30, face = "bold"),
            plot.subtitle = element_text(family = titlefont, colour = textcol, size = 25),
            plot.caption = element_text(family = bodyfont, colour = textcol, size = 12, hjust = 1, margin = margin(5, 5, 5, 5)),
            panel.background = element_rect(colour = bgcol, fill = bgcol),
            plot.background = element_rect(colour = bgcol, fill = bgcol),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.position = "right",
            legend.title = element_blank(),
            legend.text = element_text(family = bodyfont, colour = textcol, size = 15),
            legend.background = element_rect(fill = bgcol, colour = bgcol),
            legend.key = element_rect(fill = bgcol, colour = bgcol))  +
      labs(title = paste("Averaged Playstyle Wheel"),
           subtitle = clubs[i],
           caption = paste("Data and style from", dQuote("Introducing playstyle wheels"), "by John Muller.", "\nPublished on The Athletic on May 18, 2022", sep = " "))
    
    psw_club[[i]]
    
    ggsave(psw_club[[i]], file = here::here("outputs", "Averaged", paste(i, " - ", clubs[i], " - Full Averaged Wheel.png", sep = "")), 
           width = 20, height = 20, units = "cm", dpi = 600)
  }


#Plot 2: All Teams -----

psw_avg_full <- psw_avg %>%
  mutate(value = floor(value),
         ang_exp = rep(ang,20)) %>% 
  ggplot(aes(x = factor(group, levels = c("Possess", "Disrupt", "Finish", "Press", "Defend")), y = value, label = group)) +
  geom_col(aes(fill = group, alpha = group), width = 1, colour = bgcol, show.legend = FALSE) + 
  scale_fill_manual(values = wheelcols) +
  scale_alpha_manual(values = wheelalpha) +
  geom_text(aes(angle = ang_exp),
            y = 12,
            colour = textcol,
            family = bodyfont,
            size = 3) +
  geom_hline(yintercept = seq(-2, 9, by = 1), color = bgcol, size = linesize, alpha = 1) +
  geom_hline(yintercept = seq(0, 10, by = 10), color = textcol, size = linesize, alpha = 1) +
  annotate(geom = "line", x = c(0.5,0.5), y = c(0,12), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(1.5,1.5), y = c(0,12), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(2.5,2.5), y = c(0,12), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(3.5,3.5), y = c(0,12), colour = "white", size = linesize) + 
  annotate(geom = "line", x = c(4.5,4.5), y = c(0,12), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(5.5,5.5), y = c(0,12), colour = "white", size = linesize) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  coord_polar() +
  #theme_minimal() + 
  theme(plot.title = element_text(family = titlefont, colour = textcol, size = 24),
        plot.subtitle = element_text(family = titlefont, colour = textcol, size = 16),
        plot.caption = element_text(family = bodyfont, colour = textcol, size = 12, hjust = 1, margin = margin(5, 5, 5, 5)),
        panel.background = element_rect(colour = bgcol, fill = bgcol),
        plot.background = element_rect(colour = bgcol, fill = bgcol),
        strip.background = element_rect(colour = bgcol, fill = bgcol),
        strip.text = element_text(family = bodyfont, colour = textcol, size = 12),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = paste("Averaged Playstyle Wheel"),
       subtitle = "Premier League, 2021-22",
       caption = paste("Data and style from", dQuote("Introducing playstyle wheels"), "by John Muller.", "\n Published on The Athletic on May 18, 2022", sep = "")) +
  facet_wrap(~ club, nrow = 4)

ggsave(psw_avg_full, file = here::here("outputs", "Averaged", "Averaged_Wheel_All.png"), 
       width = 30, height = 30, units = "cm", dpi = 600)

#end plot 2 ----

#Plot 3: Full Wheel ------
psw_club_full <- list()

for(i in 1:length(clubs)){

  psw_club_full[[i]] <- psw %>%
  filter(club %in% clubs[i]) %>% 
  ggplot(aes(x = factor(metric, levels = metrics), y = value, label = metric)) +
  geom_col(aes(fill = group), width = 1, colour = bgcol) + 
  #scale_fill_manual(values = wheelcols, labels = rev(c("Bass", "Hats", "Kick","Pad", "Percussion"))) +
  scale_fill_manual(values = wheelcols) +
  #scale_alpha_manual(values = wheelalpha, guide = 'none') +
  geom_text(aes(label = stringr::str_wrap(metric,10)),
            y = 11.5,
            colour = textcol,
            family = bodyfont,
            #fontface = "bold",
            angle = ang_full,
            size = 5) +
  geom_hline(yintercept = seq(-2, 9, by = 1), color = bgcol, size = linesize, alpha = 1) +
  geom_hline(yintercept = seq(0, 10, by = 10), color = textcol, size = linesize, alpha = 1) +
  annotate(geom = "line", x = c(0.5,0.5), y = c(0,13), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(3.5,3.5), y = c(0,13), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(6.5,6.5), y = c(0,13), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(9.5,9.5), y = c(0,13), colour = "white", size = linesize) + 
  annotate(geom = "line", x = c(12.5,12.5), y = c(0,13), colour = "white", size = linesize) +
  annotate(geom = "line", x = c(15.5,15.5), y = c(0,13), colour = "white", size = linesize) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  coord_polar() +
  theme(plot.title = element_text(family = titlefont, colour = textcol, size = 35, face = "bold"),
        plot.subtitle = element_text(family = titlefont, colour = textcol, size = 30),
        plot.caption = element_text(family = bodyfont, colour = textcol, size = 18, hjust = 1, margin = margin(5, 5, 5, 5)),
        panel.background = element_rect(colour = bgcol, fill = bgcol),
        plot.background = element_rect(colour = bgcol, fill = bgcol),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(10,10,10,10),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(family = bodyfont, colour = textcol, size = 18),
        legend.background = element_rect(fill = bgcol, colour = bgcol),
        legend.key = element_rect(fill = bgcol, colour = bgcol)) +
  labs(title = paste(clubs[i],"'s playstyle wheel",sep=""),
       subtitle = "Premier League, 2021-22",
       caption = paste("Data and style from", dQuote("Introducing playstyle wheels"), "by John Muller", "\nPublished on The Athletic on May 18, 2022", sep = " "))
    
  psw_club_full[[i]]
  
  ggsave(psw_club_full[[i]], file = here::here("outputs", "Full", paste(i, " - ", clubs[i], " - Full Playstyle Wheel (Article).png", sep = "")), 
         width = 30, height = 30, units = "cm", dpi = 900)
}


