## Helper functions to keep syntax close to ZX81
val <- function(p) as.numeric(p)
x <- function(p, q = p) substr(X, p, q)

## Ported ZX81 code
X <- "100cannot doyou walk opened   closed   a sword  a key    nothing  a chest  a dragon a corpse taken    you died you won. "
repeat {
    cat(paste("You are in a", substr("cavepit halllake", val(x(1)) * 4 - 3, val(x(1)) * 4), "\n"))
    u <- tolower(readline(prompt = "What would you like to do? "))
    m <- 2 * (u == "north") * (x(1) < "3") -
      2 * (u == "south") * (x(1) > "2") +
      (x(1) == "2" & u == "west") -
      (x(1) == "3" & u == "east")
    a <- (3 * val(x(2)) + 2 * (x(3) == "2")) * (x(1) == 2 & u == "look chest") +
      (11 + (x(3) == "2")) * (x(1) == "3" & u == "kill dragon") +
      (m != 0) +
      (5 + val(x(1))) * (u == "look") +
      (6 - val(x(3))) * (u == "inventory") +
      (6 - (x(3) == "0")) * (x(1) == 4 & u == "look corpse") +
      10 * (x(1, 3) == "400" & u == "get key") +
      2 * (x(1, 3) == "201" & u == "open chest") +
      10 * (x(1, 3) == "211" & u == "get sword")
    X <- paste0(val(x(1)) + m, val(x(2)) + (a == 2), val(x(3)) + (a == 10), substring(X, 4))
    cat(paste(substr(X, a * 9 + 4, a * 9 + 12), "\n"))
    if (a >= 11) break
}
