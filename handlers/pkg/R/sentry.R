##***********************************************************************
## this program is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## this program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with the nens libraray.  If not, see
## <http://www.gnu.org/licenses/>.
##
## Copyright © 2011, 2012 by Mario Frasca
##
## Library    : logging
##
## Purpose    : implement a sentry logging handler
##
## Usage      : library(logging.handlers)
##
## $Id$
##
## initial programmer :  Mario Frasca
##
## initial date       :  20110426
##

sentryAction <- function(msg, conf, record, ...) {
  if(!all(c(require(RCurl),
            require(uuid),
            require(rjson),
            require(digest))))
    stop("sentryAction depends on RCurl, uuid, rjson, digest.")
  
  if(exists('dsn', envir=conf)) {
    ## first time doing something with this handler: parse the dsn
    glued <- gsub('(.*)://(.*):(.*)@([^/]+)(.*)/(\\w)', '\\1://\\4\\5::\\2::\\3::\\6',
                  with(conf, dsn), perl=TRUE)
    parts <- strsplit(glued, "::")[[1]]
    assign('server', parts[1], envir=conf)
    assign('sentry.public.key', parts[2], envir=conf)
    assign('sentry.private.key', parts[3], envir=conf)
    assign('project', parts[4], envir=conf)
    rm('dsn', envir=conf)
  }
  
  anythingMissing <- !sapply(c("server", "sentry.private.key", "sentry.public.key", "project"),
                             exists, envir=conf)
  
  if(length(list(...)) && 'dry' %in% names(list(...))) {
    return(all(!anythingMissing))
  }
  
  # Past the dry run, do the real work from here
  
  if(any(anythingMissing)) {
    missing <- names(anythingMissing)[anythingMissing]
    stop(paste("this handler with sentryAction misses ", paste(missing, collapse=", "), ".\n", sep=""))
  }
  
  if(missing(record))  # needed for `level` and `timestamp` fields.
    stop("sentryAction needs to receive the logging record.\n")
  
  # Bail early if the record level doesn't match a threshold
  if (exists('threshold', envir=conf)) {
    # The higher the number the more coarse the level (FINEST is 1, FATAL is 50)
    if (as.numeric(record$level) < loglevels[[with(conf, threshold)]]) {
      return()
    }
  }
  
  sentry.server <- with(conf, server)
  sentry.private.key <- with(conf, sentry.private.key)
  sentry.public.key <- with(conf, sentry.public.key)
  project <-  with(conf, project)
  client.name <- tryCatch(with(conf, client.name), error = function(e) "r.logging")
  
  # Convert the timestamp to UTC
  recordTimestamp <- as.POSIXct(record$timestamp)
  attributes(recordTimestamp)$tzone <- "GMT"
  formattedRecordTimestamp <- format(recordTimestamp, "%Y-%m-%dT%H:%M:%S")

  # Get the current sdk version
  sdkVersion <- as.character(packageVersion('logging.handlers'))

  params <- list("project" = project,
                 "event_id" = gsub("-", "", as.character(UUIDgenerate(FALSE))),
                 "timestamp" = formattedRecordTimestamp,
                 "message" = record$msg,
                 "level" = as.numeric(record$level),
                 "logger" = record$logger,
                 "server_name" = client.name,
                 "platform" = "other",
                 "sdk" = list("name" = "r-logging.handler",
                              "version" = sdkVersion))
  
  if(exists('client.env', envir=conf)) {
    params$environment <- with(conf, client.env)
  }

  metadata <- list()
  metadata$call_stack <- paste(lapply(sys.calls(), deparse), collapse=" || ")
  params$extra <- metadata
  
  repr <- as.character(toJSON(params))
  
  url <- paste(sentry.server, "api", "store", "", sep="/")
  timestamp <- Sys.time()
  timestampSeconds <- format(timestamp, "%s")
  
  x.sentry.auth.parts <- c(paste("sentry_version", "5", sep="="),
                           paste("sentry_timestamp", timestampSeconds, sep="="),
                           paste("sentry_key", sentry.public.key, sep="="),
                           paste("sentry_secret", sentry.private.key, sep="="), # TODO: Per the doc this is optional and we should only pass it if it was provided on the DSN. Right now we require it.
                           paste("sentry_client", paste("r-logging.handler", sdkVersion, sep="/"), sep="="))
  x.sentry.auth <- paste("Sentry", paste(x.sentry.auth.parts, collapse=", "))
  hdr <- c('Content-Type' = 'application/octet-stream', 'X-Sentry-Auth' = x.sentry.auth)
  
  httpPOST(url, httpheader = hdr, postfields = toJSON(params))
  
}
