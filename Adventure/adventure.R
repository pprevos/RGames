## Text adventure in R
## The Devil is in the Data
## lucidmanager.org

## Initiate game
verbs <- c("look", "take", "put", "use", "wait", "kill")
moves <- c("north", "south", "east", "west", "up", "down")
map <- read.csv("map.csv")
objects <- read.csv("objects.csv")
actions <- readLines("actions.md")
room <- 5
health <- 6
capacity <- 5

## Displays relevant prose (descriptions and results of actions)
prose <- function(key) {
    key <- tolower(key)
    line <- which(actions == paste("##", key)) + 1
    if(length(line) == 0)
        return()
    while(substr(actions[line], 1, 1) != "#") {
        cat(paste(actions[line], "\n"))
        line <- line + 1
    }
    cat("\n")
}

## Describe the local surroundings
look <- function(room) {
    ## Describe
    prose(paste("room", room))
    ## Directions
    passages <- moves[which(map[map$room == room, 3:8] != 0)]
    cat(paste0("You can go: ", paste(passages, collapse = ", "), ".\n"))
    ## Objects
    stuff <- subset(objects, type == "object" & location == room)$name
    if (length(stuff) != 0)
        cat(paste0("You see: ", paste(stuff, collapse = ", "), ".\n"))
    ## Inventory
    inventory()
    cat("\n")
    stuff <- subset(objects, type == "object" & location == 0)$name
    if (length(stuff) > 0)
        cat(paste0("You have ", paste(stuff, collapse = ", "), ".\n\n"))
}

## Take an object
take <- function(object) {
    if (is.na(object))
        return(cat("You cannot take this.\n"))
    stuff <- subset(objects, type == "object" & location == room)$name
    if (object %in% stuff) {
        ob_num <- which(objects$name == object)
        if (capacity - objects$weight[ob_num] <= 0)
            return(cat(paste0("You cannot carry the ", object, ".\n")))
        objects$location[ob_num] <<- 0
        capacity <<- capacity - objects$weight[ob_num]
        cat(paste0("You take the ", object, ".\n"))
        inventory()
        }
    else
        cat(paste0("You cannot take the ", object, ".\n"))
}

## Put and object on the ground
put <- function(object)  {
    stuff <- subset(objects, type == "object" & location == 0)$name
    if (object %in% stuff) {
        ob_num <- which(objects$name == object)
        objects$location[ob_num] <<- room
        capacity <<- capacity + objects$weight[ob_num]
        cat(paste0("You place the ", object, " on the ground.\n"))
        inventory()
        }
    else
        cat(paste0("You don't have the ", object, ".\n"))
}

## Use an object
use <- function(object) {
    if (is.na(object))
        return(cat("You don't have that.\n"))
    if (objects[which(objects$name == object), "location"] != 0)
        return(cat(paste0("You don't have a ", object, ".\n")))
    if (object == "bandage") {
        objects$location[1] <<- 100
        capacity <- capacity - objects$weight[1]
        if (room == objects$location[8] | objects$health[8] > 0) {
            prose("heal blacksmith")
            objects$status[8] <<- 30
        } else {
            if (health == 6)
                cat("You eat the bandage. It leaves a strange taste in or mouth.")
            else {
                cat("You use the bandage to heal your wounds. You feel much better now.\n")
                health <<- 6
            }
        }
    }
    if (object == "sword")
        prose("fingernails")
    if (object == "flute") {
        if (room > 20) { ## use the flute inside the tempel
            prose("flute1")
            prose("flute2")
            room <<- 49 - room
            look(room)
        } else
            prose("flute1")
    }
    if (object == "rope") {
        if (room == 14) {
            prose("rope")
            ## Create new room connections
            map$up[14] <<- 15
            map$down[15] <<- 14
            ## Rope is no longer an object
            objects$location[4] <<- 100
            health <<- health + objects$weight[4]
            ## New room descriptions
            actions <<- actions[-which(actions == "## rope in tree")]
        } else
            prose("skipping")
    }
}

## Wait for situation to change
wait <- function(dummy) {
    cat("You wait a little while ...\n")
}

## Kill something
kill <- function(object) {
    opponent <- which(objects$name == object)
    weapon <- ifelse(objects$location[2] == 0, "sword", "fists")
    ## Existing actor?
    if (is.na(object) | objects$type[opponent] != "actor")
        return(cat("Now, now, not so agressive please.\n"))
    ## No opponent present
    if (objects$location[opponent] != room) {
        return(prose(paste("shadow fight", weapon)))
    }
    ## Dead opponent
    if (objects$health[opponent] <= 0)
        return(cat(paste0("You poke the dead ", object, ".\n")))
    ## Fight sequence
    strength <- ifelse(objects$location[2] == 0, 2, 1)
    cat(paste0("You attach the ", object, "with your ", weapon, ".\n"))
    if (runif(1, 0, 1) > 0.4) {
        cat()
        objects$health[opponent] <- objects$health[opponent] - strength
        if (objects$health[opponent] == 1)
            cat(paste0("The ", object, " is heavily wounded.\n"))
        if (objects$health[opponent] <= 0) {
            cat(paste0("The ", object, " succumbs to the blows of your ", weapon, ".\n"))
        }         
    } else
        cat(paste0("Your enthusiam is commendable, but you don't manage to hit the ", object, ".\n"))     
}
        
## Move player
walk <- function(direction) {
    r <- map[map$room == room, direction]
    if (r != 0)
        room <<- r
    else
        cat("You can't go that way.\n")
    look(room)
}

## Actors
actors <- function() {
    ## Wizard (object 5) hands the fluit and vanishes
    if (room == objects$location[5]) {
        prose("wizard")
        objects$location[5] <<- 100 ## Wizard vanishes
        objects$location[3] <<- 0 ## You have the fluit
        health <<- health - objects$weight[3]
        inventory()
    }
    ## Crows (object 6) appear randomly in the forest
    if (room < 10) {
        objects$health[6] <<- objects$health[6] - 1
        if (objects$health[6] > 0)
            return()
        else {
            objects$health[6] <<- sample(1:5, 1)
            prose("bird")
            ## Crows randomly hurt the player
            if (runif(1, 0, 1) < 0.5)
                prose("bird misses")
            else {
                prose("bird hits")
                health <<- health - 1
            }
        }
    }
    ## Dragon (object 7)
    if (objects$health[7] <= 0 & room == 17)
        prose("dead dragon")
    if (room == 17) {
        prose("dragon attack")
        if (runif(1, 0, 1) < 0.5) {
            prose("dragon hit 1")
            health <<- health - 2
        } else
            prose("dragon miss")   
    }
    if (room == 18) {
        prose("dragon hit 2")
        room <<- 16
        health <<- health - 1
    }    
    ## Blacksmith (object 8)
    if (room == objects$location[8]) {
        if (objects$health[8] <= 0)
            return(cat("The dead blacksmith lies on the floor.\n"))
        if (objects$status[8] == 10) {
            prose("blacksmith wounded")
            objects$status[8] <<- 20
        }
        if (objects$status[8] >= 20 & objects$status[8] < 30) {
            prose("blacksmith pleading")
            objects$status[8] <<- objects$status[8] + 1
            if (objects$status[8] == 25)
                objects$status[8] <<- 60
        }
        if (objects$status[8] == 30) {
            prose("blacksmith healed")
            objects$status[8] <<- 40
            objects$location[8] <<- 11
        }
        if (objects$status[8] >= 40 & objects$status[8] < 50) {
            objects$status[8] <<- objects$status[8] + 1
            if (room == objects$location[8])
                prose("blacksmith forges sword")
            if (objects$status[8] == 43)
                objects$status[8] <<- 50
        }
        if (objects$status[8] == 50) {
            if (room == objects$location[8]) {
                prose("blacksmith gives sword")
                objects$location[2] <<- 11
                objects$location[8] <<- 100
                objects$status[8] <<- 0
            }
        }
        if (objects$status[8] == 60) {
            if (room == objects$location[8]) {
                prose("blacksmith dies")
                objects$health[8] <<- 0
                objects$status[8] <<- 0
            }
        }
    }
}

## Game Play loop
while (health > 0) {
    verb <- NA
    move <- NA
    object <- NA
    command <- readline(prompt = "What would you like to do? :")
    command <- tolower(command)
    words <- unlist(strsplit(command, " "))
    verb <- verbs[verbs %in% words][1] # First valid verb in the list
    direction <- moves[moves %in% words][1] # First valid direction in the list
    object <- objects$name[objects$name %in% words][1] # First valid object
    cat("\n")
    if (!is.na(direction)) {
        walk(direction)
    }
    if (!is.na(verb)) {
        if (verb == "look")
            arg <- room
        else
            arg <- object
        do.call(verb, list(arg))
    }
    if (is.na(direction) & is.na(verb))
        cat("You are talking gibberish.\n\n")
    else {
        actors()
        if(health <= 0)
            prose("death")
    }
}
