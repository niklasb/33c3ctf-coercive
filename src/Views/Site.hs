{-# LANGUAGE OverloadedStrings #-}
module Views.Site where

import Control.Monad
import Data.Monoid (mempty)
import Text.Blaze.XHtml5 ((!))
import qualified Data.Text as T
import qualified Text.Blaze.Bootstrap as H
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A

siteView :: H.Html -> H.Html
siteView body =
    H.html $
    do H.head $
        do H.title "Coercive"
           H.meta ! A.charset "utf-8"
           H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
           H.link ! A.href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" ! A.rel "stylesheet"
           H.link ! A.href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.7.0/styles/default.min.css" ! A.rel "stylesheet"
           H.link ! A.href "/css/blog.css" ! A.rel "stylesheet"
       H.body $
        do H.div ! A.class_ "blog-masthead" $
            H.div ! A.class_ "container" $
             H.nav ! A.class_ "blog-nav" $
              do H.a ! A.class_ "blog-nav-item" ! A.href "/" $ "Coercive Data Analysis"
           H.div ! A.class_ "container" $ body
           H.div ! A.class_ "blog-footer" $
            do H.p $
                do H.span "Blog template built for "
                   H.a ! A.href "http://getbootstrap.com" $ "Bootstrap"
                   H.span " by "
                   H.a ! A.href "https://twitter.com/mdo" $ "@mdo"
           H.script ! A.src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.7.0/highlight.min.js" $ mempty
           H.script $ "hljs.initHighlightingOnLoad();"


panelWithErrorView :: T.Text -> Maybe T.Text -> H.Html -> H.Html
panelWithErrorView title mError ct =
    H.div ! A.class_ "panel panel-info" ! A.style "margin-top: 30px;" $
     do H.div ! A.class_ "panel-heading" $
         H.div ! A.class_ "panel-title" $ H.toHtml title
        H.div ! A.class_ "panel-body" $
         do case mError of
              Just errMsg ->
                  H.alertBox H.BootAlertDanger (H.toHtml errMsg)
              Nothing -> mempty
            H.div ct
