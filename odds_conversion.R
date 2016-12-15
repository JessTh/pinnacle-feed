

# ODDS CONVERSION ---------------------------------------------------------

# American to Decimal odds
am.to.dec <- function(odds) {
  if(is.na(odds)) return(NA)
  if(odds > 0) {    # 109 -> 2.09
    round((odds+100)/100, 3)
  } else {          # -129 -> risk 129 to win 100
    round(-100/odds+1, 3)
  }
}


# PROBABILITY TO ODDS -----------------------------------------------------

## American
am.odds <- function(prob){
  if(prob>0.5) {
    round(prob/(1-prob)*-100,0)
  } else {
    round((1-prob)/prob*100,0)
  }
}

## Decimal
dec.odds <- function(prob){
  round(1/prob, 3)
}


# ODDS TO PROBABILITY -----------------------------------------------------

## American
am.prob <- function(odds){
  if(odds<0){
    round(odds/(odds-100),3)
  } else {
    round(100/(odds+100),3)
  }
}

## Decimal
dec.prob <- function(odds){
  round(1/odds, 3)
}
