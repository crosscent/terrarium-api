module HTTP.Base 
    ( getRequest
    ) where

import Network.BufferType   ( buf_empty
                            , bufferOps
                            , BufferOp
                            , BufferType (..) )
import Network.HTTP         ( RequestMethod ( GET ))
import Network.HTTP.Headers
import Network.HTTP.Base    ( Request (..)
                            , Request_String )
import Network.URI          ( parseURI
                            , URI )

defaultUserAgent :: String
defaultUserAgent = "BetterMatter.com"

toBufOps :: BufferType a => Request a -> BufferOp a
toBufOps _ = bufferOps

mkRequest :: BufferType ty => RequestMethod -> URI -> Request ty
mkRequest meth uri = req
    where req = Request { rqURI     = uri 
                        , rqBody    = empty
                        , rqHeaders = [ Header HdrContentLength "0" 
                                      , Header HdrUserAgent defaultUserAgent]
                        , rqMethod  = meth}
          empty = buf_empty (toBufOps req)

getRequest :: String -> Request_String
getRequest urlString =
    case parseURI urlString of
      Nothing   -> error ("headRequest: not a valid URL - " ++ urlString)
      Just u    -> mkRequest GET u
