# TIC TAC TOE
# The Devil is in the Data
# peter@prevos.net

# Draw the game board
draw.board <- function(game) {
    xo <- c("X", " ", "O") # Symbols
    par(mar = rep(1,4))
    plot.new()
    plot.window(xlim = c(0,30), ylim = c(0,30))
    abline(h = c(10, 20), col = "darkgrey", lwd = 4)
    abline(v = c(10, 20), col = "darkgrey", lwd = 4)
    text(rep(c(5, 15, 25), 3), c(rep(25, 3), rep(15,3), rep(5, 3)), xo[game + 2], cex = 4)
    # Identify location of any three in a row
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
    # Draw winning lines 
    if (all(hor > 0)) for (i in hor) lines(c(0, 30), rep(i, 2), lwd = 10, col="red")
    if (all(ver > 0)) for (i in ver) lines(rep(i, 2), c(0, 30), lwd = 10, col="red")
    if (abs(diag1) == 3) lines(c(2, 28), c(28, 2), lwd = 10, col = "red")
    if (abs(diag2) == 3) lines(c(2, 28), c(2, 28), lwd = 10, col = "red")
}

# Human player enters a move
move.human <- function(game) {
    text(4, 0, "Click on screen to move", col = "grey", cex=.7)
    empty <- which(game == 0)
    move <- 0
    while (!move %in% empty) {
        coords <- locator(n = 1) # add lines
        coords$x <- floor(abs(coords$x) / 10) + 1
        coords$y <- floor(abs(coords$y) / 10) + 1
        move <- coords$x + 3 * (3 - coords$y)
    }
    return (move)
}

# Evaluate board position
eval.game <- function(game, player) {
    # Determine game score
    square <- t(matrix(game, nrow = 3))
    hor <- rowSums(square)
    ver <- colSums(square)
    diag1 <- sum(diag(square))
    diag2 <- sum(diag(t(apply(square, 2, rev))))
    eval <- c(hor, ver, diag1, diag2)
    # Determine best score
    minimax <- ifelse(player == -1, "min", "max")
    best.score <- do.call(minimax, list(eval))
    if (abs(best.score) == 3) best.score <- best.score * 2
    return (best.score)
}

# Determine computer move
move.computer <- function(game, player) {
    empty <- which(game == 0)
    eval <- matrix(nrow = 10, ncol = 9, data = 0)
    for (i in empty) {
        game.tmp <- game
        game.tmp[i] <- player
        eval[1, i] <- eval.game(game.tmp, player)
        empty.tmp <- which(game.tmp ==0)
        for (j in empty.tmp) {
            game.tmp1 <- game.tmp
            game.tmp1[j] <- -player
            eval[(j + 1), i] <- eval.game(game.tmp1, -player)
        }
    }
    if (!any(abs(eval[1,]) == 6)) { # When winning, play move
        # Analyse opponent move
        minimax <- ifelse(player == -1, "max", "min") # Minimax
        best.opponent <- apply(eval[-1,], 1, minimax)
        eval[1,] <- eval[1,] * -player * best.opponent
    }
    # Add randomisation and strategic values
    board <- c(3, 2, 3, 2, 4, 2, 3, 2, 3) # Strategic values
    board <- sapply(board, function(x) runif(1, 0.1 * x, (0.1 * x) + 0.1)) # Randomise
    eval[1, empty] <- eval[1, empty] + player * board[empty] # Randomise moves
    # Pick best game
    minimax <- ifelse(player == -1, "which.min", "which.max") # Minimax
    move <- do.call(minimax, list(eval[1,])) # Select best move
    return(move)
}

# Main game engine
tic.tac.toe <- function(player1 = "human", player2 = "computer") {
    game <- rep(0, 9) # Empty board
    winner <- FALSE # Define winner
    player <- 1 # First player
    players <- c(player1, player2)
    draw.board(game)
    while (0 %in% game & !winner) { # Keep playing until win or full board
        if (players[(player + 3) %% 3] == "human") # Human player
            move <- move.human(game)
        else # Computer player
            move <- move.computer(game, player)
        game[move] <- player # Change board
        draw.board(game)
        winner <- max(eval.game(game, 1), abs(eval.game(game, -1))) == 6 # Winner, winner, chicken dinner?
        player <- -player # Change player
    }
}

tic.tac.toe()


