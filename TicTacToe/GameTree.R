
game.tree <- function(game, player, depth = 3, d.count = 0) {
    d.count <- d.count + 1
    for (i in which(game == 0)) {
        game.tmp <- game
        game.tmp[i] <- player
        score <- eval.game(game.tmp, player)
        print(paste(paste(game.tmp, collapse = ","), "|", score, "|", d.count))
        draw.board(game.tmp)
        text(15,15, score, col = "lightblue", cex=10)
        if (d.count < depth) Recall(game.tmp, -player, depth, d.count)
    }
}
game.tree(game, player = 1)

