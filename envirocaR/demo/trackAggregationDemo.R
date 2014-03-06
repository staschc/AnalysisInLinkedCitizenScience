library(envirocaR)
serverUrlAndID = "https://envirocar.org/api/dev/tracks/51d333eae4b01cbb274b7073"
serverUrl = "https://envirocar.org/api/stable"
trackID = "51d333eae4b01cbb274b7073"
trackData = importSingleTrack(serverUrl,trackID)

trackIds = getTrackIDs(serverUrl)
size = length(trackIds)
randomId = trackIds[sample(1:size,1)]

trackData = importSingleTrack(serverUrl,randomId)
class(trackData)

?aggregateTrack
newTrack = aggregateTrack(track=trackData,phen="Speed",interval=10,fn=mean)
class(newTrack)

newTrack2 = aggregateTrack(track=trackData)
class(newTrack2)

plot(trackData@tracks$Track1@sp@coords,col="blue",pch="*")
points(newTrack@sp,col="red",pch="o")
points(newTrack2@sp,col="green",pch="O")

plot(trackData@tracks$Track1@data$time,trackData@tracks$Track1@data$Speed,type="l",col="blue",ylab="Speed",xlab="Minutes");
lines(newTrack@data$time,newTrack@data$aggr.Speed,col="red")
lines(newTrack2@data$time,newTrack2@data$Speed,col="green")


