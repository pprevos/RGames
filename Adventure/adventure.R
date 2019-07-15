## Text adventure in R
## The Devil is in the Data
## lucidmanager.org

## Initiate game
verbs <- c("look", "take", "put", "use", "wait", "kill", "quit")
moves <- c("north", "south", "east", "west", "up", "down")
map <- read.csv("Adventure/map.csv")
objects <- read.csv("Adventure/objects.csv", stringsAsFactors = FALSE)
actions <- readLines("Adventure/actions.md")
room <- 2
health <- 6
capacity <- 6
move <- 0

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

## List objects carried by player
inventory <- function() {
    stuff <- subset(objects, type == "object" & location == 0)$name
    if (length(stuff) > 0)
        cat(paste0("You have ", paste(stuff, collapse = ", "), ".\n\n"))
}

## Show health, capacity and objects from inventory

showStatus <- function() {
    # informs health
    cat(paste0("You have ", health, " points of health\n"))
    # informs capacity
    cat(paste0("You can carry ", capacity, " objects\n"))
    # informs on the inventory
    inventory()
}

## Describe the local surroundings
look <- function(room) {
    ## Describe
    prose(paste("room", room))
    ## Directions
    passages <- moves[which(map[map$room == room, 3:8] != 0)]
    cat(paste0("You can go: ", paste(passages, collapse = ", "), ".\n\n"))
    ## Objects
    stuff <- subset(objects, type == "object" & location == room)$name
    if (length(stuff) != 0)
        cat(paste0("You see: ", paste(stuff, collapse = ", "), ".\n"))
    inventory()
}

## Take an object
take <- function(object) {
    if (is.na(object)) {
        return(cat("You cannot take this.\n"))}
    stuff <- subset(objects, type == "object" & location == room)$name
    if (object %in% stuff) {
        ob_num <- which(objects$name == object)
        if (capacity - objects$weight[ob_num] < 0)
            return(cat(paste0("You cannot carry the ", object, ".\n")))
        objects$location[ob_num] <<- 0
        capacity <<- capacity - objects$weight[ob_num]
        cat(paste0("You take the ", object, ".\n"))
        inventory()
    } else {
        cat(paste0("You cannot take the ", object, ".\n"))
    }
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
    if (is.na(object)) {
        return(cat("You don't have that.\n"))
    } else if (objects[which(objects$name == object), "location"] != 0) {
        return(cat(paste0("You don't have a ", object, ".\n")))
    } else switch (object,
                   bandage = { # if object is bandage
                       objects$location[1] <<- 99
                       capacity <<- capacity + objects$weight[1]
                       if (room == objects$location[8] & objects$health[8] > 0) {
                           prose("heal blacksmith")
                           objects$status[8] <<- 30
                       } else {
                           if (health == 6) {
                               prose("eat bandage")
                           } else {
                               prose("use bandage")
                               health <<- 6
                           }
                       }
                   },
                   sword = { # if object is sword
                       prose("fingernails")
                       
                   },
                   flute = { # if object is flute
                       if (room > 20) { ## use the flute inside the temple
                           prose("flute1")
                           prose("flute2")
                           room <<- 49 - room
                           if (room == 24) {
                               health <<- 99
                           } else {
                               look(room)}
                       } else {
                           prose("flute1")}
                   },
                   rope = { # if object is rope
                       if (room == 14) {
                           prose("rope")
                           ## Create new room connections
                           map$up[14] <<- 15
                           map$down[15] <<- 14
                           ## Rope is no longer an object
                           objects$location[4] <<- 99
                           capacity <<- capacity + objects$weight[4]
                           ## New room descriptions
                           actions[which(actions == "## rope in tree")] <<- "\n"
                           ## Look
                           look(room)
                       } else {
                           prose("skipping")
                           health <<- health - 1
                       }# if it is something else
                   },
                   )
}

## Wait for situation to change
wait <- function(dummy) {
    cat("You wait a little while ...\n\n")
}

## Kill something
kill <- function(object) {
    opponent <- which(objects$name == object)
    weapon <- ifelse(objects$location[2] == 0, "sword", "fists")
    ## is opponent listed in objects?
    if (length(which(objects$name == object)) == 0) {
        return(cat("You cannot possibly do that.\n"))
    ## Existing actor?
    } else if (is.na(object) | objects$type[opponent] != "actor") {
        return(cat("Now, now, not so agressive please.\n"))
    ## No opponent present
    } else if (objects$location[opponent] != room) {
        return(prose(paste("shadow fight", weapon)))
    ## Dead opponent
    } else if  (objects$health[opponent] <= 0) {
        return(cat(paste0("You poke the dead ", object, ".\n")))
    ## everything else
    }
     
    ## Fight sequence
    strength <- ifelse(weapon == "sword", 2, 1)
    cat(paste0("You attack the ", object, " with your ", weapon, ".\n\n"))
    if (runif(1, 0, 1) > 0.3) {
        objects$health[opponent] <<- objects$health[opponent] - strength
        if (objects$health[opponent] == 1)
            cat(paste0("The ", object, " is heavily wounded.\n"))
        if (objects$health[opponent] <= 0) {
            cat(paste0("The ", object, " succumbs to the blows of your ", weapon, ".\n"))
        }         
    } else
        cat(paste0("Your enthusiam is commendable, but you don't manage to hit the ",
                   object, ".\n"))     
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
    if (objects$health[7] > 0 & room == 17) {
        prose("dragon attack")
        if (runif(1, 0, 1) < 0.5) {
            prose("dragon hit 1")
            health <<- health - 2
        } else
            prose("dragon miss")   
    }
    if (objects$health[7] > 0 & room == 18) {
        prose("dragon hit 2")
        room <<- 16
        health <<- health - 1
    }    
    ## Blacksmith (object 8)
    if (room == objects$location[8]) {
        if (objects$health[8] <= 0)
            prose("dead blacksmith")
        else {
            if (objects$status[8] == 70) {
                prose("blacksmith forest")
                objects$location[8] <<- sample(1:9, 1)
            }
            if (objects$status[8] == 60) {
                if (room == objects$location[8]) {
                    prose("blacksmith dies")
                    objects$health[8] <<- 0
                    objects$status[8] <<- 0
                }
            }
            if (objects$status[8] == 50) {
                if (room == objects$location[8]) {
                    prose("blacksmith gives sword")
                    objects$location[2] <<- 11
                    objects$location[8] <<- sample(1:9, 1)
                    objects$status[8] <<- 70
                    look(room)
                }
            }
            if (objects$status[8] >= 40 & objects$status[8] < 50) {
                objects$status[8] <<- objects$status[8] + 1
                if (room == objects$location[8])
                    prose("blacksmith forges sword")
                if (objects$status[8] == 42)
                    objects$status[8] <<- 50
            }
            if (objects$status[8] == 30) {
                prose("blacksmith healed")
                objects$status[8] <<- 40
                objects$location[8] <<- 11
            }
            if (objects$status[8] >= 20 & objects$status[8] < 30) {
                prose("blacksmith pleading")
                objects$status[8] <<- objects$status[8] + 1
                if (objects$status[8] == 25)
                    objects$status[8] <<- 60
            }
            if (objects$status[8] == 10) {
                prose("blacksmith wounded")
                objects$status[8] <<- 20
            }
        }
    }
    if (room == 24) {
        if (health == 99)
            prose("victory")
        else {
            prose("thrown out")
            health <<- health - 1
            room <<- 20
            look(20
                 )
        }
    }
}

## Game Play loop
while (health > 0 & health < 99) {
    if (move == 0)
        prose("opening")
    verb <- NA
    direction <- NA
    object <- NA
    cat("----------------------------------------\n")
    command <- readline(prompt = "What would you like to do? :")
    command <- tolower(command)
    words <- unlist(strsplit(command, " "))
    verb <<- verbs[verbs %in% words][1] # First valid verb in the list
    direction <<- moves[moves %in% words][1] # First valid direction in the list
    object <<- objects$name[objects$name %in% words][1] # First valid object
    cat("\n")
    if (!is.na(direction)) {
        walk(direction)
    }
    if (!is.na(verb)) {
        if (verb == "look")
            arg <- room
        else if (verb == "quit")
            break
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
    move <- move + 1
}
