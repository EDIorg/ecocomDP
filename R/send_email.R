#' Send email
#'
#' @param from (character) Email address
#' @param to (character) Email address
#' @param attachment (character) Attachment file name with full path and file extension
#' @param smtp.relay (character) SMTP relay
#' @param relay.user (character) Relay user
#' @param relay.user.pass (character) Relay user password
#' @param subject (character) Subject line
#' @param msg (character) Message
#'
#' @details Works for Linux. May not work for other OS.
#' 
#' @export
#' 
send_email <- function(from, to, attachment, smtp.relay, relay.user, 
                      relay.user.pass, subject, msg) {
  
  cmd <- paste("sendemail -f", from, "-t", to, "-a", attachment, "-s", 
               smtp.relay, "-xu", relay.user, "-xp", relay.user.pass, "-u", 
               subject, "-m", msg)
  
  system(cmd)
}