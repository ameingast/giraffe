module Giraffe.Wai.HttpRequestParserTest where

import qualified Network.HTTP.Types(http11)

defaultRequest :: Request
defaultRequest = Request { 
  requestMethod = "GET",
  httpVersion = http11,
  rawPathInfo = "/",
  rawQueryString = "",
  serverName = "localhost",
  serverPort = 80,
  requestHeaders = [],
  isSecure = False,
  remoteHost = undefined,
  pathInfo = [],
  queryString = [],
  requestBody = mempty,
  vault = mempty
}

testParseScrapeRequest = id

testParseAnnounceRequest = id