library(XML)
library(httr)
library(plyr)

base.url <- 'http://xml.pinnaclesports.com/pinnacleFeed.aspx'
## Sports league: http://xml.pinnaclesports.com/leagues.asp

## Call limit: 1 call/min

# level : tag                 : attributes
# --------------------------------------------------------
# 1     : event               : event_date, gamenumber, league, IsLive
# 2     : participant (many)  : name, contestatnum, rotnum, visiting/home
# 3     : periods (many/NA)   : number, descr, cutoff, status, update, spread_max, moneyline_max, total_max
# 4     : moneyline (1/NA)    : visiting, home 
# 4     : spread (1/NA)       : visiting, home, adj.vis, adj.home
# 4     : total (1/NA)        : total_points, over_adjust, under_adjust


# Feed requests -----------------------------------------------------------

# Header:
# <PinnacleFeedTime>1430913235898</PinnacleFeedTime>
# <lastContest>32426246</lastContest>
# <lastGame>199828993</lastGame>


## Get feed with all current events. 
## Output: 'content' and 'last.feed' (timestamp for the request, to use for rate limitation)

getFeed <- function(){
  res  <- GET(base.url)
  cat('Status: '); cat(http_status(res)$message); cat('\n');
  last <- xmlValue(getNodeSet(content(res), "//PinnacleFeedTime")[[1]])
  return(list(content=content(res), last.feed=last))
}

## Get feed for specific sport,  e.g: 'Tennis', 'Basketball'

getSport <- function(sport){
  url  <- paste(base.url, '?sporttype=',sport, sep='')
  res  <- GET(url)
  cat('Status: '); cat(http_status(res)$message); cat('\n');
  return(content(res))
}


## Get updates since last call (all events)
## last: timestamp from last call.
## Output: 'content' and 'last.feed'

getUpdates <- function(last) {
  url  <- paste(base.url, '?last=', last, sep='')
  res  <- GET(url)
  cat('Status: '); cat(http_status(res)$message); cat('\n');
  last <- xmlValue(getNodeSet(content(res), "//PinnacleFeedTime")[[1]])
  return(list(content=content(res), last.feed=last))
}


# Feed content to data.frames ---------------------------------------------

## Return details on all events (event info, participants, prices) from feed (XML-content)
## output: data.frame

getAllDetails <- function(doc) {
  evs   <- getNodeSet(doc, "//event")
  ldply(evs, function(x) {
    evs   <- getEvents(x)
    pts   <- getParticipants(x)
    pds   <- getPeriods(x)
    if( nrow(pds) > 0) {
      join(pts, pds, by='selection')
    }
  })
}


## Events --------------------

## Return event details as data.frame, from feed (XML to data.frame)

getEvents <- function(doc){
  evs <- getNodeSet(doc, "//event")
  ldply(evs, eventDetails )
}

## Get details for an event (XML to data.frame)
eventDetails <- function(event){
  data.frame(
    time    = xmlValue(xmlChildren(event)$event_datetimeGMT),
    game    = xmlValue(xmlChildren(event)$gamenumber),
    sport   = xmlValue(xmlChildren(event)$sporttype),
    league  = xmlValue(xmlChildren(event)$league),
    live    = xmlValue(xmlChildren(event)$IsLive)
  )
}


## Participants --------------

## Return all participants, from feed (XML to data.frame)

getAllParticipants <- function(doc){
  pts   <- getNodeSet(doc, "//participant")
  ldply(pts, participantDetails)
}

## Get participants for a specific event (XML to data.frame)

getParticipants <- function(event){
  pts   <- xmlChildren(xmlChildren(event)$participants)
  ldply(pts, participantDetails)
}

## Get details for a participant (XML to data.frame)

participantDetails <- function(x){
  data.frame(
    name      = xmlValue(xmlChildren(x)$participant_name),
    number    = xmlValue(xmlChildren(x)$contestantnum),
    rotnum    = xmlValue(xmlChildren(x)$rotnum),
    selection = xmlValue(xmlChildren(x)$visiting_home_draw)
  )
}


## Periods --------------------

## Prices: Moneyline, Spread (adjusted) and Totals (adjusted)

getAllPeriods <- function(doc){
  pds   <- getNodeSet(doc, "//period")
  ldply(pds, periodDetails)
}

## Get peroiod detailfs for an event

getPeriods <- function(event){
  pds   <- xmlChildren(xmlChildren(event)$periods)
  ldply(pds, periodDetails_df)
}

## Get details for a period (XML to data.frame)

periodDetails <- function(x) {
  data.frame(
    number          = xmlValue(xmlChildren(x)$period_number),
    descr           = xmlValue(xmlChildren(x)$period_description),
    cutoff          = xmlValue(xmlChildren(x)$periodcutoff_datetimeGMT),
    status          = xmlValue(xmlChildren(x)$period_status),
    update          = xmlValue(xmlChildren(x)$period_update),
    spread.max      = xmlValue(xmlChildren(x)$spread_maximum),
    ml.max          = xmlValue(xmlChildren(x)$moneyline_maximum),
    total.max       = xmlValue(xmlChildren(x)$total_maximum),
    
    ml.home         = getMoneyline(xmlChildren(x)$moneyline)$home,
    ml.vis          = getMoneyline(xmlChildren(x)$moneyline)$visiting,
    ml.draw         = getMoneyline(xmlChildren(x)$moneyline)$draw,
    spr.home        = getSpread(xmlChildren(x)$spread)$home,
    spr.vis         = getSpread(xmlChildren(x)$spread)$visiting,
    spr.adj.home    = getSpread(xmlChildren(x)$spread)$adj.home,
    spr.adj.vis     = getSpread(xmlChildren(x)$spread)$adj.vis,
    total.points    = getTotal(xmlChildren(x)$total)$points,
    total.adj.overr = getTotal(xmlChildren(x)$total)$adj.over,
    total.adj.under = getTotal(xmlChildren(x)$total)$adj.under
  )
}

## Get details for a period, tall format (used to join on selection) (XML to data.frame)

periodDetails_df <- function(x) {
  cbind(
    number          = xmlValue(xmlChildren(x)$period_number),
    descr           = xmlValue(xmlChildren(x)$period_description),
    cutoff          = xmlValue(xmlChildren(x)$periodcutoff_datetimeGMT),
    status          = xmlValue(xmlChildren(x)$period_status),
    update          = xmlValue(xmlChildren(x)$period_update),
    spread.max      = xmlValue(xmlChildren(x)$spread_maximum),
    ml.max          = xmlValue(xmlChildren(x)$moneyline_maximum),
    total.max       = xmlValue(xmlChildren(x)$total_maximum),
    total.points    = getTotal(xmlChildren(x)$total)$points,
    total.adj.overr = getTotal(xmlChildren(x)$total)$adj.over,
    total.adj.under = getTotal(xmlChildren(x)$total)$adj.under,
    rbind(data.frame(
      selection       = 'Home',
      moneyline       = getMoneyline(xmlChildren(x)$moneyline)$home,
      spread          = getSpread(xmlChildren(x)$spread)$home,
      spread.adj      = getSpread(xmlChildren(x)$spread)$adj.home
    ), data.frame(
      selection       = 'Visiting',
      moneyline       = getMoneyline(xmlChildren(x)$moneyline)$visiting,
      spread          = getSpread(xmlChildren(x)$spread)$visiting,
      spread.adj      = getSpread(xmlChildren(x)$spread)$adj.vis
    ), data.frame(
      selection       = 'Draw', 
      moneyline       = getMoneyline(xmlChildren(x)$moneyline)$draw,
      spread          = NA,
      spread.adj      = NA
    )))
}

## price detail help functions:

getMoneyline <- function(ent){
  if( length(ent)==0) return( data.frame(t(rep(NA, 3))))
  data.frame(
    visiting = xmlValue(xmlChildren(ent)$moneyline_visiting),
    home     = xmlValue(xmlChildren(ent)$moneyline_home),
    draw     = xmlValue(xmlChildren(ent)$moneyline_draw)
  )
}

getSpread <- function(ent){
  if( length(ent)==0) return( data.frame(t(rep(NA, 4))))
  data.frame(
    visiting = xmlValue(xmlChildren(ent)$spread_visiting),
    home     = xmlValue(xmlChildren(ent)$spread_home),
    adj.vis  = xmlValue(xmlChildren(ent)$spread_adjust_visiting),
    adj.home = xmlValue(xmlChildren(ent)$spread_adjust_home)
  )
}

getTotal <- function(ent){
  if( length(ent)==0) return( data.frame(t(rep(NA, 3))))
  data.frame(
    points    = xmlValue(xmlChildren(ent)$total_points),
    adj.over  = xmlValue(xmlChildren(ent)$over_adjust),
    adj.under = xmlValue(xmlChildren(ent)$under_adjust)
  )
}

