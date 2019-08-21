# 1. Draw a tic-tac-toe board.
# 2. Add the playing loop and switch the players.
# 3. Add click-to-play.
# 4. Add check for a win.
# 5. Require play on empty square. (And change empty representation
#    from "E" to " " because it makes board easier to read.)
# 6. Add computer player without AI, just choosing an remaining empty square randomly

#### ----- Best Strategy Link --------
## https://www.wikihow.com/Win-at-Tic-Tac-Toe strategy of winning


rm(list = ls())
won <- function(board, player, debug = FALSE) {
  stopifnot(player == "X" | player == "O") # test the validity of the parameter: player

  stopifnot(board %in% c(" ", "X", "O")) # test the validity of the parameter: board
  stopifnot(dim(board)[1] == dim(board)[2]) ## test if the board is square

  if (debug) {
    cat(sep = "", "player=", player, ", board=", "\n")
    print(board)
  }

  return(
    all(board[1:3, 1] == player) |
      all(board[1:3, 2] == player) |
      all(board[1:3, 3] == player) |
      all(board[1, 1:3] == player) |
      all(board[2, 1:3] == player) |
      all(board[3, 1:3] == player) |
      all(diag(board) == player) |
      all(diag(board[3:1, ]) == player)
  )
}

# test.board = matrix(data = c("X", "O", " ",
#                              "O", "X", "O",
#                              "X", "O", "X"), nrow = 3, ncol = 3)
# print(test.board)
# stopifnot( won(test.board, "X"))
# stopifnot(!won(test.board, "O"))
#
# test.board[2, 2] = "O"
# print(test.board)
# stopifnot(!won(test.board, "X"))
# stopifnot( won(test.board, "O"))


winningchoice <- function(board, player) {
  player <- ifelse(test = (player == "X"), yes = "O", no = "X")

  m <- 0
  for (i in 1:9) {
    board0 <- board
    index <- i
    col <- x[i]
    row <- y[i]
    if (board[row, col] == " ") {
      board0[row, col] <- player
      if (won(board = board0, player = player, debug = FALSE)) {
        m <- i
        return(m)
      }
    }
  }
  return(m)
}

first5movement <- function(board, i) {
  if (i == 1) {
    index <- 5
    col <- x[index]
    row <- y[index]
  } else if (i == 2) {
    if (board[2, 2] == " ") {
      index <- 5
      col <- x[index]
      row <- y[index]
    } else {
      index <- 1
      col <- x[index]
      row <- y[index]
    }
  }
  else if (i == 3) {
    if (board[1, 2] == "X" | board[2, 1] == "X") {
      index <- 9
      col <- x[index]
      row <- y[index]
    } else if (board[3, 2] == "X" | board[2, 3] == "X") {
      index <- 1
      col <- x[index]
      row <- y[index]
    } else if (board[1, 1] == "X" | board[3, 3] == "X") {
      index <- 3
      col <- x[index]
      row <- y[index]
    } else {
      index <- 1
      col <- x[index]
      row <- y[index]
    }
  } else if (i == 4) {
    if (board[2, 2] == "O") {
      if ((board[1, 2] == "X") | (board[2, 1] == "X") | (board[2, 3] == "X") | (board[3, 2] == "X")) {
        if ((sum(board[2, ] == "X") == 2) | (sum(board[, 2] == "X") == 2)) {
          index <- 1
          col <- x[index]
          row <- y[index]
        } else if ((board[2, 1] == "X") & ((board[1, 2] == "X") | (board[3, 2] == "X"))) {
          index <- 1
          col <- x[index]
          row <- y[index]
        } else if ((board[2, 3] == "X") & ((board[1, 2] == "X") | (board[3, 2] == "X"))) {
          index <- 7
          col <- x[index]
          row <- y[index]
        } else {
          index <- winningchoice(board, player)
          if (index != 0) {
            col <- x[index]
            row <- y[index]
          } else {
            if (board[1, 1] == " ") {
              index <- 1
              col <- x[index]
              row <- y[index]
            } else {
              index <- 3
              col <- x[index]
              row <- y[index]
            }
          }
        }
      } else {
        index <- 2
        col <- x[index]
        row <- y[index]
      }
    } else {
      if (board[3, 3] == "X") {
        index <- 3
        col <- x[index]
        row <- y[index]
      } else {
        index <- winningchoice(board, player)
        col <- x[index]
        row <- y[index]
      }
    }
  } else if (i == 5) {
    player0 <- ifelse(test = (player == "X"), yes = "O", no = "X")
    index <- winningchoice(board, player0)
    if (index != 0) {
      col <- x[index]
      row <- y[index]
    } else {
      index <- winningchoice(board, player)
      if (index == 0) {
        safe.sample <- function(x, ...) x[sample.int(length(x), ...)] # safe sampling
        # When x has length 1, sample(x, 1) has a different meaning. Check ?sample.

        index <- safe.sample(x = which(c(board) == " "), size = 1) # choose an remaining empty square randomly
        # by using sample() and which()

        col <- x[index]
        row <- y[index]
      } else {
        col <- x[index]
        row <- y[index]
      }
    }
  } else {
    player0 <- ifelse(test = (player == "X"), yes = "O", no = "X")
    index <- winningchoice(board, player0)
    if (index != 0) {
      col <- x[index]
      row <- y[index]
    }
  }
  a <- c(col, row)
  return(a)
}


#------ main program------


player <- "X" # input "X" if human first input "O" if AI first


x <- rep(1:3, each = 3)
y <- rep(1:3, times = 3)

plot(x, y, type = "n", xlim = c(0, 4), ylim = c(4, 0), xaxt = "n", yaxt = "n", ann = FALSE) # hide the axes and their labels
segments(
  x0 = c(0.5, 0.5, 1.5, 2.5),
  y0 = c(1.5, 2.5, 0.5, 0.5),
  x1 = c(3.5, 3.5, 1.5, 2.5),
  y1 = c(1.5, 2.5, 3.5, 3.5)
)

board <- matrix(data = rep(" ", times = 9), nrow = 3, ncol = 3)


winner <- " "

for (i in 1:9) {
  if (player == "X") { # human player
    repeat {
      index <- identify(x, y, n = 1, plot = FALSE, tolerance = 0.75) # use the larger tolerance

      col <- x[index]
      row <- y[index]

      if (board[row, col] == " ") {
        rect(1, 3.55, 3, 4, col = par("bg"), border = par("bg")) # hide the warning
        break
      } else {
        text(x = 2, y = 3.7, labels = "Please click on empty square.") # show the warning
      }
    }
  } else { # computer player
    if (i <= 5) {
      a <- first5movement(board, i)
      col <- a[1]
      row <- a[2]
    }
    else {
      index <- winningchoice(board, player)
      if (index == 0) {
        safe.sample <- function(x, ...) x[sample.int(length(x), ...)] # safe sampling
        # When x has length 1, sample(x, 1) has a different meaning. Check ?sample.

        index <- safe.sample(x = which(c(board) == " "), size = 1) # choose an remaining empty square randomly
        # by using sample() and which()

        col <- x[index]
        row <- y[index]
      } else {
        col <- x[index]
        row <- y[index]
      }
    }
  }


  board[row, col] <- player
  text(x = col, y = row, labels = player)

  # cat(sep = "", "i=", i, ", player=", player, ", index=", index,
  #     ", row=", row, ", col=", col, ", board:", "\n")
  # print(board)

  if (won(board, player, debug = FALSE)) {
    winner <- player
    break
  }

  player <- ifelse(test = (player == "X"), yes = "O", no = "X")
}

announcement <- ifelse(winner == " ", "Draw!", paste(winner, " won!")) # announce draw if so
text(x = 2, y = 1 / 3, labels = announcement, col = "red")