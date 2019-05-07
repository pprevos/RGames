# Assessment
score.hand <- function(hand) {
    # Extract values and suits
    cards.value <- c(1:9, "T", "J", "Q", "K", "A")
    values <- substr(hand, 1, 1)
    values <- unlist(lapply(values, function(x) which(cards.value == x)))
    suits <- substr(hand, 2, 2)
    occurence <- rle(values[order(values)])
    nKind <- function(values, n) {
        ifelse(any(occurence$lengths == n), max(occurence$values[occurence$lengths == n]), NA)
    }
    # Vector to store poker hands scores
    score <- rep(NA, 10)
    # High Card
    score[1] <- max(values)
    # One Pair
    score[2] <- nKind(values, 2)
    # Two Pairs
    score[3] <- ifelse(sum(occurence$lengths == 2) == 2, max(occurence$values[occurence$lengths == 2]), NA)
    # Three of a Kind
    score[4] <- nKind(values, 3)
    # Straight
    if (all(values[order(values)] - min(values) == 0:4))
        score[5] <- max(values)
    # Flush
    score[6] <- ifelse(length(unique(suits)) == 1, max(values), NA)
    # Full House
    score[7] <- max(c(score[2], score[4]))
    # Four of a Kind
    score[8] <- nKind(values, 4)
    # Straight Flush
    score[9] <- ifelse(!is.na(score[5]) & !is.na(score[6]), max(values), NA)
    # Royal Flush
    score[10] <- ifelse(!is.na(score[5]) & !is.na(score[6]) & max(values) == 14, 14, NA)
    return(max(score + 20 * 1:10, na.rm = TRUE))
}

# Define deck of cards
suits <- c("S", "H", "C", "D")
values <- c(2:9, "T", "J", "Q", "K", "A")
deck <- paste(sapply(values, function(s) paste(s, suits, sep = "")))

# Shuffle the deck
deck <- deck[sample(1:52, 52)]

for (i in 1:10) {
    hand <- sample(deck, 5, replace = FALSE)
    print(paste(paste(hand, collapse=", "), "-", score.hand(hand)))
}

