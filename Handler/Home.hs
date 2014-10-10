module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    sendResponseStatus status200 ("SICK DUDE" :: Text)
