## Load Tic Tac Toe functions
source("TicTacToe.R")
library(animation)

## WAR GAMES TIC TAC TOE
draw.board.wargames <- function(game) {
    xo <- c("X", " ", "O") # Symbols
    par(mar = rep(1,4), bg = "#050811")
    plot.new()
    plot.window(xlim = c(0,30), ylim = c(0,30))
    abline(h = c(10, 20), col = "#588fca", lwd = 20)
    abline(v = c(10, 20), col = "#588fca", lwd = 20)
    text(rep(c(5, 15, 25), 3), c(rep(25, 3), rep(15,3), rep(5, 3)), xo[game + 2],
         cex = 20, col = "#588fca")
    text(0, 0, "lucidmanager.org", col = "#588fca", cex = 2, adj = 0)
    ## Identify location of any three in a row
    square <- t(matrix(game, nrow = 3))
    hor <- abs(rowSums(square))
    if (any(hor == 3)) 
        hor <- (4 - which(hor == 3)) * 10 - 5 
    else 
        hor <- 0
    ver <- abs(colSums(square))
    if (any(ver == 3)) 
        ver <- which(ver == 3) * 10 - 5 
    else
        ver <- 0
    diag1 <- sum(diag(square))
    diag2 <- sum(diag(t(apply(square, 2, rev)))) 
    ## Draw winning lines 
    if (all(hor > 0)) for (i in hor) lines(c(0, 30), rep(i, 2), lwd = 20, col="#588fca")
    if (all(ver > 0)) for (i in ver) lines(rep(i, 2), c(0, 30), lwd = 20, col="#588fca")
    if (abs(diag1) == 3) lines(c(2, 28), c(28, 2), lwd = 20, col = "#588fca")
    if (abs(diag2) == 3) lines(c(2, 28), c(2, 28), lwd = 20, col = "#588fca")
}

saveGIF ({
    for (i in 1:10) {
        game <- rep(0, 9) # Empty board
        winner <- 0 # Define winner
        player <- -1 # First 
        while (0 %in% game & winner == 0) { # Keep playing until win or full board
            empty <- which(game == 0) # Define empty squares
            if (length(empty) == 9)
                move <- empty[sample(length(empty), 1)] # Random move
            else
                move <- minimax(game, player)[[1]]
            game[move] <- player # Change board
            draw.board.wargames(game)
            winner <- ganador(game) # Evaulate game
            player <- -player # Change player
        }
    }
},
interval = .1, movie.name = "wargames.gif",
ani.width = 1024, ani.height = 1024)



