library(xts)
serverUrl = "https://envirocar.org/api/stable"
bbox = matrix(c(7.61,51.96,7.62,51.97),ncol=2,dimnames=list(c("x","y"),c("min","max")))
interval = .parseISO8601('2013-12-01T13:30/2013-12-06T13:39')
trackIDs = getTrackIDs(serverUrl,bbox)
trackIDs[1:3]
tracks = lapply(trackIDs[2:3],importSingleTrack,serverUrl=serverUrl)
length(tracks)
tracks[0]
?Tracks
debug(importSingleTrack)
undebug(importSingleTrack)
tracks
sessionInfo()
traceback()
debug(spacetime:::TrackStats)
debug(spacetime:::TrackSummary) #für debuggen einer nicht sichtbaren Methode dreifaces :
undebug(spacetime:::TrackSummary)
?identical

debug(importSingleTrack)
track = importSingleTrack(serverUrl,"52a1db8ae4b0593ccdf18c0b")
singleTrackUrl=paste(serverUrl,"52a1db8ae4b0593ccdf18c0b",sep="/")
singleTrackUrl
rawData = fromJSON(getURL(singleTrackUrl,ssl.verifypeer = FALSE))
ids = lapply(rawData$features,function(X) X$properties$id)
ids
summary(track)
track@units
attr(track,"units")="test"
track@units
stidf = STIDF(geometry(track), track$time, track@data)
trackObject = Track(stidf)
summary(trackObject)
stplot()
stidf@units=track
summary(track)
sessionInfo()
