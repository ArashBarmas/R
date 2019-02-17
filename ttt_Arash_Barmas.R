

play<- function()
  {
  computer <- FALSE
  player<-0 #This simply counts how many times we have played
  x <- as.character(1:9)
  game_over = FALSE
  display(x)
  one_two<- readline("How many humans are playing? ")
  if (one_two==1)
    computer= TRUE

  if (!computer)# 2 players, 2 humans
    {
        while(!game_over)
        {
          if (player%%2==0)
            pos <- readline("X choose a spot to play : ")
          else
            pos <- readline("O choose a spot to play : ")
          player <- player + 1
          if (error_check(as.numeric(pos),x))
                {
                  player <- player -1
                  next
                 }
          else
            {
              x <- fill(as.numeric(pos), player,x)
              display(x)
              if (win_drawn(x)==TRUE)
                  {
                    game_over=TRUE
                    break
                  }
             }# else
              
        }#while loop
    }#if(!computer)
  
  else if (computer) # 1 player, 1 computer
  {
    x_or_o <- readline("Should the computer play first or second? 1(X) or 2(O)? ")
    x_or_o <- as.numeric(x_or_o)
    turn <- FALSE
    while(!game_over)
      {
        if (x_or_o == 2)
            {
              pos <- readline("X choose a spot to play : ")

              if (error_check(as.numeric(pos),x))
                next
              else
              {
                x <- fill(as.numeric(pos), (x_or_o-1) ,x)
                display(x)
                if (win_drawn(x)==TRUE)
                  {
                    game_over=TRUE
                    break
                  }
               }# else

              x <- fill(computer_turn(x,x_or_o), (x_or_o) ,x)
              cat("\n")
              display(x)
              if (win_drawn(x)==TRUE)
              {
                game_over=TRUE
                break
              }
             
            }
        else # when x_or_o is 1
          {
            if (turn == FALSE)
            {
              x <- fill(computer_turn(x,x_or_o), (x_or_o) ,x)
            }
            cat("\n")
            display(x)
            if (win_drawn(x)==TRUE)
            {
              game_over=TRUE
              break
            }
            
            pos <- readline("O choose a spot to play : ")

            if (error_check(as.numeric(pos),x))
            {
              turn <- TRUE
              next
            }
            
            else
            {
              x <- fill(as.numeric(pos), (x_or_o+1) ,x)
              display(x)
              if (win_drawn(x)==TRUE)
              {
                game_over=TRUE
                break
              }
            }# else
            turn <- FALSE
           }# else

       }#While loop
  
  }# else if (with computer)
}

winnings<- list(c(1,2,3),c(4,5,6),c(7,8,9),c(1,4,7),c(2,5,8),c(3,6,9),c(1,5,9),c(3,5,7))

computer_turn <- function(x,x_or_o)
{
  y1<- which(x=="X")#computer
  y2<- which(x=="O")
  unfilled_spots<- which((!is.na(suppressWarnings(as.numeric(x))))==TRUE)# available spots
  if (x_or_o==1)
  {
    computer_vec <- y1# X
    opp_vec <- y2 # O
  }
  else
  {
    computer_vec <- y2# O
    opp_vec <- y1# X
  }
  # The optimal stategy is to see if psoition 5 is taken or not, if it's available it should choose it
  if (5 %in% unfilled_spots)
  {
    cat("\ncomputer chose 5")
    return(5)
  }

  # first we see if potential of winning exists or not
  for (each in winnings)
        {
          if (sum(each%in% computer_vec)==2)# check to see if winning can happen for computer
              {
                if (each[which((each%in% computer_vec)==FALSE)] %in% unfilled_spots)#check see if spot is not-filled
                {
                  cat("\ncomputer chose", each[which((each%in% computer_vec)==FALSE)])
                  return (each[which((each%in% computer_vec)==FALSE)])
                }
            
               }# first if
         }# first for loop
  # Second it checks if it can block the human player
    for (each in winnings)
      {
           if (sum(each%in% opp_vec)==2)
              {
                if (each[which((each%in% opp_vec)==FALSE)] %in% unfilled_spots)
                {
                  cat("\ncomputer chose", each[which((each%in% opp_vec)==FALSE)])
                    return (each[which((each%in% opp_vec)==FALSE)])
                }
              
               }# else if
         }# second for loop
  
      if (length(unfilled_spots)>1)
      {
        random_choice <- sample(unfilled_spots,1)
        cat("\ncomputer chose", random_choice)
        return (random_choice)
      }
      else if (length(unfilled_spots)==1)
        return (unfilled_spots)
                

}

win_drawn<- function(x)# This function checks whether winning or drawn takes place
  {
  
  y1<- which(x=="X")
  y2<- which(x=="O")
  for (each in winnings)# This for loop checks if wining has been occured
  {
    if ((sum(each%in% y1)==3) || (sum(each%in% y2)==3))
        {
            if (length(y1)>length(y2))
              cat("\n***First player(X) won!*** ")
            else
              cat("\n***Second Player(O) won!*** ")
            return (TRUE)
         }
  }
  if (sum(is.na(suppressWarnings(as.numeric(x))))==9)# This part checks if game is drawn
      {
        cat("\nGame is drawn")
        return (TRUE)
      }
  
  return (FALSE)
}
  
display <- function(x=1:9)
{
  cat(" ", x[1],"|",x[2],"|", x[3],"\n ---+---+---\n ",x[4],"|",x[5],"|", x[6],"\n ---+---+---\n ",
      x[7],"|",x[8],"|", x[9],"\n")
  
}
# fill function modifies the vector x with the new inputs by the user and returns the vector itself
fill<- function(num,i,x)# this function fill in the X or O in the appropriate spot
{
  if (i%%2==1){
    x[num] <- "X"
  }
  else
  {
    x[num] <- "O"
  }
  x
}
error_check<- function(num,x)# This function checks to if number entered is valid or not
{
  if (num>9)
    {
      cat(" Spots must be chosen from 1-9, please choose valid spot")
      return(1)
    }
  else if (num %in% which(is.na(suppressWarnings(as.numeric(x)))))
    {
      print(" This spot is already taken, please choose a valid ")
      return (2)
    }
  return (0)
}


#play()




