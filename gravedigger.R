## Creepy Computer Games
## Reynold, Colin and McCaig, Rob, Creepy Computer Games (Usborne, London).
## https://archive.org/details/Creepy_Computer_Games_1983_Usborne_Publishing/
## Gravedigger by Alan Ramsey

## Initiate board
A <- matrix(ncol = 20, nrow = 10)
A[, ] <- " "

## Starting variables
W <- 0 # Move number
X <- 5 # Remaining holes
death <- 0 # Game over?

## Initiate pieces
Y <- "*"
B <- "+"
C <- "O" 
D <- ":"
E <- "X"
Z <- " "

## Draw board
## Add borders
A[c(1, 10), ] <- D
A[, 1] <- D
A[1:8, 20] <- D
## Add graves
for (i in 1:20){
    A[floor(runif(1) * 7 + 2), floor(runif(1) * 15 + 3)] <- B
}

## Starting positions
## Player
M <- 2
N <- 2
A[N, M] <- Y
## Skeletons
S <- c(4, 19, 3, 19, 2, 19)

## Game play
repeat{    
    ## Position skeletons
    for (J in seq(1, 5, by = 2)) {
        A[S[J], S[J + 1]] <- E
    }
    W <- W + 1 ## Move counter
    if (W > 60) {
        print("The clock's struck midnight")
        print("Aghhhhh!!!!")
        break
    }
    ## Print board
    v <- paste(as.vector(t(A)), collapse = "")        
    for (i in 1:10)
        print(substr(v, (i - 1) * 20 + 1, (i - 1) * 20 + 20))
    ## Enter move
    A1 <- toupper(readline(paste0("Enter move ", W, " (You can go N, S, E or W): ")))
    ## Move player
    T <- N
    U <- M
    if (A1 == "N") {
        T <- N - 1
    }
    if (A1 == "E") {
        U <- M + 1
    }
    if (A1 == "S") {
        T <- N + 1
    }
    if (A1 == "W") {
        U <- M - 1
    }
    ## Collission detection
    if (A[T, U] == D | A[T, U] == B) { # Edge or grave
        print("That way's blocked")
    }
    if (A[T, U] == C) { # Hole
        print("You've fallen into one of your own holes")
        break
    }
    if (A[T, U] == E) { # Skeleton
        death <- 1
    }
    if (T == 9 & U == 20) { # Escaped
        print("You're free!")
        print(paste0("Your performance rating is ",
                    floor((60 - W) / 60 * (96 + X)), "%"))
        break
    }
    if (death == 1) {
        print("Urk! You've been scared to death by a skeleton.")
        break
    }
    if (A[T, U] == Z) { # Player can move
        ## Move player and dig hole
        A [N, M] <- Z
        if (X != 0) {
            B1 <- toupper(readline("Would you like to dig a hole (Y or N): "))
            if (B1 == "Y") {
                X <- X - 1
                A[N, M] <- C
            }
        }
        N <- T
        M <- U
        A[T, U] <- Y
        ## Move skeletons
        for (J in seq(1, 5, by = 2)) {
            ## Store skeleton position in temp variable
            P <- S[J]
            Q <- S[J + 1]
            if (any(c(A[P + 1, Q], A[P - 1 , Q], A[P, Q - 1], A[P, Q + 1]) == Y)) {
                death <- 1
            } else
            {
                ## Move skeletons
                if (A1 == "S" & A[P + 1, Q] == Z){
                    S[J] <- S[J] + 1 # Follow player
                    A[P, Q] <- Z
                }
                if (A1 == "N" & A[P - 1, Q] == Z){
                    S[J] <- S[J] - 1 # Follow player
                    A[P, Q] <- Z
                }
                if (A1 == "E" & A[P, Q - 1] == Z & M < Q){
                    S[J + 1] <- S[J + 1] - 1 # Move towards player
                    A[P, Q] <- Z
                }
                if (A1 == "E" & A[P, Q + 1] == Z & M > Q) {
                    S[J + 1] <- S[J + 1] + 1 # Reverse direction
                    A[P, Q] <- Z
                }
            }
        }
    }
}

