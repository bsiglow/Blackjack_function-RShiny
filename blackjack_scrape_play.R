### Lets start the scrape ###

library(tidyverse)

library(rvest) # rvest or harvest this is what we will be using as well. 

library(magrittr)

## Reading in three of the tables on how to respond to black jack when the dealer has to hit on a soft 17 [The dealer showing a 6 would have to hit even if their under card was an A or the ability to have a 7 or 17, a soft 17] and a 4-8 deck shoot (traditional casino)

#setwd("C:/Users/Ben Siglow/Documents/Side_Work/Blackjack_function-RShiny")

BJ21 <-  "https://blog.prepscholar.com/blackjack-strategy"

BJ21_table_1 <- read_html(BJ21) %>%
  html_table() %>%
  magrittr::extract2(13)
colnames(BJ21_table_1) <- BJ21_table_1[1,] # rename columns on first row

BJ21_table_1 <-  BJ21_table_1[-1,] # Remove the now duplicate first row of the col labels

alpha_1_processing <-  BJ21_table_1[1,]
beta_1_processing <- BJ21_table_1[-1,]

charlie_1_processing <- rbind(alpha_1_processing,alpha_1_processing,alpha_1_processing,alpha_1_processing,alpha_1_processing)

BJ21_table_1 <- rbind(charlie_1_processing,beta_1_processing)


BJ21_table_1[1,1] <- 4
BJ21_table_1[2,1] <- 5
BJ21_table_1[3,1] <- 6
BJ21_table_1[4,1] <- 7
BJ21_table_1[5,1] <- 8

BJ21_table_1$Hard <- as.numeric(BJ21_table_1$Hard)
BJ21_table_1$Hard[is.na(BJ21_table_1$Hard)] <-  18
BJ21_table_1$Hard <-  as.character(BJ21_table_1$Hard)

#save(BJ21_table_1, file = "BJ21_table_1")
  
BJ21_table_2 <- read_html(BJ21) %>%
  html_table() %>%
  magrittr::extract2(14)
colnames(BJ21_table_2) <- BJ21_table_2[1,] # rename columns on first row

BJ21_table_2 <-  BJ21_table_2[-1,] # Remove the now duplicate first row of the col labels
BJ21_table_2$Soft <-  as.numeric(BJ21_table_2$Soft)
BJ21_table_2$Soft[is.na(BJ21_table_2$Soft)] <-  20
BJ21_table_2[,1] <- as.character(BJ21_table_2[,1])

#save(BJ21_table_2, file = "BJ21_table_2")

BJ21_table_3 <- read_html(BJ21) %>%
  html_table() %>%
  magrittr::extract2(15)

colnames(BJ21_table_3) <- BJ21_table_3[1,] # rename columns on first row

BJ21_table_3 <-  BJ21_table_3[-1,] # Remove the now duplicate first row of the col labels

colnames(BJ21_table_3)[1] <-  "Split"

BJ21_table_3[1,1] <- 2
BJ21_table_3[2,1] <- 3
BJ21_table_3[3,1] <- 4
BJ21_table_3[4,1] <- 6
BJ21_table_3[5,1] <- 7
BJ21_table_3[6,1] <- 8
BJ21_table_3[7,1] <- 9
BJ21_table_3[8,1] <- "A"

BJ21_table_3[,1] <- as.character(BJ21_table_3[,1])

#save(BJ21_table_3, file = "BJ21_table_3")

#load(url("https://www.dropbox.com/s/mixp2ttg0fujspf/BJ21_table_1?dl=1"))
#load(url("https://www.dropbox.com/s/ey6agxy1axv3r1t/BJ21_table_2?dl=1"))
#load(url("https://www.dropbox.com/s/xfwplo52lpdcg74/BJ21_table_3?dl=1"))


### Function start
Black_jackplay <-  function(Your_hand_card1, Your_hand_card2, Dealer_show){

  V_ace_value <- "A"
  V_king_value <- "K"
  V_queen_value <- "Q"
  V_jack_value <- "J"

  
Dealer_show <- tolower(as.character(Dealer_show))
Your_hand_card1 <- tolower(as.character(Your_hand_card1))
Your_hand_card2 <- tolower(as.character(Your_hand_card2))

name_card <- c("zero", "one","two","three","four","five","six","seven","eight","nine","ten")
num_card <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                 six=6, seven=7, eight=8, nine=9, ten = 10)

if(Your_hand_card1  %in% name_card){
  Your_hand_card1 <- num_card[Your_hand_card1]
}else{Your_hand_card1 <- Your_hand_card1}

if(Your_hand_card2  %in% name_card){
  Your_hand_card2 <- as.character(num_card[Your_hand_card2])
}else{
  Your_hand_card2 <- Your_hand_card2
}


P_ace_value <- c("Ace", "1", "11", "a", "A", "ace")
P_king_value <- c("King","king", "k", "K")
P_queen_value <- c("Queen", "queen", "Q","q")
P_jack_value <- c("Jack", "J", "jack", "j")
P_not_split <- c("5","10","K","Q","J")

if(Your_hand_card1 %in% P_ace_value){
  Your_hand_card1 <- V_ace_value
  }else if(Your_hand_card1 %in% P_king_value){
    Your_hand_card1 <-  V_king_value
  }else if(Your_hand_card1 %in% P_queen_value){
    Your_hand_card1 <- V_queen_value
  }else if(Your_hand_card1 %in% P_jack_value){
    Your_hand_card1 <- V_jack_value
  }else{
    Your_hand_card1 <- Your_hand_card1
  }

if(Your_hand_card2 %in% P_ace_value){
  Your_hand_card2 <- V_ace_value
  }else if(Your_hand_card2 %in% P_king_value){
  Your_hand_card2 <- V_king_value
  }else if(Your_hand_card2 %in% P_queen_value){
  Your_hand_card2 <- V_queen_value
  }else if(Your_hand_card2 %in% P_jack_value){
  Your_hand_card2 <- V_jack_value
  }else{
    Your_hand_card2 <- Your_hand_card2
  }

if(Dealer_show %in% P_ace_value){
  Dealer_show <- V_ace_value
}else if(Dealer_show %in% P_king_value){
  Dealer_show <- V_king_value
}else if(Dealer_show %in% P_queen_value){
  Dealer_show <- V_queen_value
}else if(Dealer_show %in% P_jack_value){
  Dealer_show <- V_jack_value
}else{
  Dealer_show <- Dealer_show
}


 
  if((Your_hand_card1 != Your_hand_card2 | (Your_hand_card1 %in% P_not_split & Your_hand_card2 %in% P_not_split)) & (Your_hand_card1 != "A" & Your_hand_card2 != "A")){
  df <- BJ21_table_1
  Hard_Soft_Split <-  "Hard"
  }else if(((Your_hand_card1 == "A") | (Your_hand_card2 == "A")) & (Your_hand_card1 != Your_hand_card2)){
  df <-  BJ21_table_2
  Hard_Soft_Split <-  "Soft"
  }else if((Your_hand_card1 == Your_hand_card2) & (!(Your_hand_card1 %in% P_not_split))){
  df <- BJ21_table_3
  Hard_Soft_Split  <- "Split"
  }else{
    df <- BJ21_table_1
    Hard_Soft_Split <-  "Hard"
  }

if(Your_hand_card1 %in% P_ace_value){
  Your_hand_card1 <- 11
}else if(Your_hand_card1 %in% P_king_value){
  Your_hand_card1 <- 10
}else if(Your_hand_card1 %in% P_queen_value){
  Your_hand_card1 <- 10
}else if(Your_hand_card1 %in% P_jack_value){
  Your_hand_card1 <- 10
}else{
  Your_hand_card1 <- Your_hand_card1
}

if(Your_hand_card2 %in% P_ace_value){
  Your_hand_card2 <- 11
}else if(Your_hand_card2 %in% P_king_value){
  Your_hand_card2 <- 10
}else if(Your_hand_card2 %in% P_queen_value){
  Your_hand_card2 <- 10
}else if(Your_hand_card2 %in% P_jack_value){
  Your_hand_card2 <- 10
}else{
  Your_hand_card2 <- Your_hand_card2
}

if(Dealer_show %in% P_ace_value){
  Dealer_show <- "A"
}else if(Dealer_show %in% P_king_value){
  Dealer_show <- "10"
}else if(Dealer_show %in% P_queen_value){
  Dealer_show <- "10"
}else if(Dealer_show %in% P_jack_value){
  Dealer_show <- "10"
}else{
  Dealer_show <- Dealer_show
}

Your_hand_card1 <- as.numeric(Your_hand_card1)
Your_hand_card2 <- as.numeric(Your_hand_card2)



  if(Hard_Soft_Split != "Split"){
  Hand <- Your_hand_card1 + Your_hand_card2  
  }else{Hand <-  Your_hand_card1
  }


  if(Hand == 11 & Hard_Soft_Split == "Split"){
    Hand <- "A"
  }else if((Hand > 18 & Hand < 21 & Hand != 11) & Hard_Soft_Split == "Hard"){
    Hand <-  18
  }else if((Hand > 20 & Hand < 21 & Hand != 11) & (Hard_Soft_Split == "Soft")){
    Hand <-  20
  }else if(Hand == 21){
    Hand <- "BJ"
  }
  else{ Hand <-  Hand
  }

  if(Hand != "BJ"){
  action <-  df[df[,1] == as.character(Hand), as.character(Dealer_show)]
  }else{
  action <- "BJ"
  }
  
  if(action == "H"){
    action <- "Hit"
  }else if(action == "S"){
    action <- "Stay"
  }else if(action == "Dh"){
    action <- "Double down if allowed, If not stay"
  }else if(action == "Ds"){
    action <-  "Double Down if allowed, if not Stand"
  }else if(action == "Rh"){
    action <-  "Surrender if allowed if not hit"
  }else if(action == "Rs"){
    action <-  "Surrender if allowed if not stand"
  }else if(action == "Rp"){
    action <- "Surrender if allowed, if not split"
  }else if(action == "P"){
    action <- "Split"
  }else if(action == "Ph"){
    action <- "Split if double after hit is allowed, if not hit"
  }else if(action == "Pd"){
    action <- "Split if double after hit is allowed, if not double"
  }else if(action == "Ps"){
      action <- "Split if double after hit is allowed, if not stand"
  }else if(action == "BJ"){
      action <- "Winner Winner Chicken Dinner !!!!"
  }
    
  
  return(print(action))
  
}

#save(Black_jackplay, file = "Black_jackplay")

Black_jackplay(Your_hand_card1 = "TEN" ,Your_hand_card2 = 3 ,Dealer_show = 9)






