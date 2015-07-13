writeXML = function(nodes, filename, dl) {
  capture.output(nodes, file=paste(dl@directory, filename, sep=""))
}
