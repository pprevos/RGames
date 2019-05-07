## Fill maze with blocks
maze <- matrix(nrow = 18, ncol = 17)
maze[, ] <- "#"

## Print Maze
print_board <- function(board) {
    nr <- nrow(board)
    nc <- ncol(board)
    v <- paste(as.vector(t(board)), collapse = "")
    for (i in 1:nr)
        print(substr(v, (i - 1) * nc + 1, (i - 1) * nc + nc))
}

## Generate maze
path <- 0
x <- 16
y <- 17

while(path < 800) {
    direction <- rep(0, 2) # Reset directions (two dimensons)
    direction[sample(1:2, 1)] <- sample(c(-1, 1), 1) # Random direction    
    passage <- sample(1:6, 1) # Random Length of passage
    for (i in 1:passage) {
        maze[y, x] <- " "
        ## Set temp location to check impact
        xt <- x + direction[1]
        yt <- y + direction[2]
        ## Border test
        if (xt != 1 & xt != 17 & yt != 1 & yt != 18) {
            path <- path + 1
            ## Cavern test
            tile <- maze[yt, xt]
            maze[yt, xt] <- " "
            if (sum(maze[(yt - 1):yt, (xt - 1):xt] == " ") != 4 &
                sum(maze[(yt - 1):yt, xt:(xt + 1)] == " ") != 4 &
                sum(maze[yt:(yt + 1), (xt - 1):xt] == " ") != 4 &
                sum(maze[yt:(yt + 1), xt:(xt + 1)] == " ") != 4) {
                # Move drill
                x <- xt
                y <- yt
            } else maze[yt, xt] <- tile
        }
    }        
}

## Locate exit
tile <- "#"
while (tile == "#") {
    x <- sample(2:16, 1)
    y <- sample(2:7, 1)
    tile <- maze[y, x]
}
maze[y, x] <- "H"

tile <- "#"
while (tile == "#") {
    x <- sample(2:16, 1)
    y <- sample(2:5, 1)
    tile <- maze[y, x]
}
maze[y, x] <- "R"

place_player <- function(x, y, compass) {
    tokens <- c("^", ">", "V", "<")
    maze[y, x] <<- tokens[which(compass == c("N", "E", "S", "W"))]
}

place_player(16, 17, ifelse(maze[16, 16] != "#", "N", "W"))

print_board(maze)
