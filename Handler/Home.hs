module Handler.Home where

import Import

getHomeR :: Handler Value
getHomeR = sendResponseStatus status200 ("SICK DUDE" :: Text)
