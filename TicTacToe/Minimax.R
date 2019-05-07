# Minimax coe by Alberto C <alb.cab94@gmail.com>

ganador <- function(juego, player) {
    game <- matrix(juego, nrow = 3, byrow = T)
    hor <- rowSums(game)
    ver <- colSums(game)
    diag <- c(sum(diag(game)), sum(diag(apply(game, 1, rev))))
    if (-3 %in% c(hor, ver, diag))
        return(-10)
    if (3 %in% c(hor, ver, diag))
        return(10)
    else
        return(0)
}

minimax <- function(juego, player) {
    free <- which(juego == 0)
    if(length(free) == 1) {
        juego[free] <- player
        return(list(move = free, U = ganador(juego, player)))
    }
    poss.results <- rep(0, 9)
    for(i in free) {
        game <- juego
        game[i] <- player
        poss.results[i] <- ganador(game, player)
    }
    mm <- ifelse(player == -1, "which.min", "which.max")
    if(any(poss.results == (player * 10))) {
        move <- do.call(mm, list(poss.results))
        return(list(move = move, U = poss.results[move]))
    }
    for(i in free) {
        game <- juego
        game[i] <- player
        poss.results[i] <- minimax(game, -player)$U
    }
    random <- runif(9, 0, 0.1)
    poss.results[-free] <- 100 * -player
    poss.results <- poss.results + (player * random)
    move <- do.call(mm, list(poss.results))
    return(list(move = move, U = poss.results[move]))
}


# Main game engine
tic.tac.toe.minimax <- function(player1 = "human", player2 = "computer") {
    game <- rep(0, 9) # Empty board
    winner <- FALSE # Define winner
    player <- 1 # First player
    players <- c(player1, player2)
    draw.board(game)
    while (0 %in% game & !winner) { # Keep playing until win or full board
        if (players[(player + 3) %% 3] == "human") # Human player
            move <- move.human(game)
        else { # Computer player
            move <- minimax(game, player)
            move <- move$move
            }
        game[move] <- player # Change board
        draw.board(game)
        winner <- max(eval.game(game, 1), abs(eval.game(game, -1))) == 6 # Winner, winner, chicken dinner?
        player <- -player # Change player
    }
}

tic.tac.toe.minimax()


