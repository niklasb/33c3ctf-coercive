{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Views.Home where

import Control.Monad
import Text.Blaze.XHtml5 ((!))
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import qualified Data.Text as T

data HomeView = HomeView
  -- [(Name, Url)]
  { home_dataSets :: [(T.Text, T.Text)]
  , home_reports :: [T.Text]
  }

homeView :: HomeView -> H.Html
homeView (HomeView{..}) =
  do H.div ! A.class_ "blog-header" $
      do H.h1 ! A.class_ "blog-title" $ "Coercive Data Analysis Framework"
         H.p ! A.class_ "lead blog-description" $ "Process your data like never before in your life"
     H.div ! A.class_ "row" $
      do H.div ! A.class_ "col-sm-3 col-sm-offset-1 blog-sidebar" $
          do H.div ! A.class_ "sidebar-module" $
              do H.h4 "Data Sets"
                 H.ol ! A.class_ "list-unstyled" $
                   forM_ home_dataSets $ \(name, url) ->
                     H.li $ H.a ! A.href (H.textValue url) $ H.toHtml name
                 H.form ! A.action "/dataset" ! A.method "post" ! A.enctype "multipart/form-data" $
                  do H.input ! A.type_ "file" ! A.size "50"
                     H.input ! A.type_ "submit" ! A.value "Upload CSV"
             H.div ! A.class_ "sidebar-module" $
              do H.h4 "Reports"
                 H.ol ! A.class_ "list-unstyled" $
                   forM_ home_reports $ \name ->
                     H.li $ H.a ! A.href (H.textValue (T.append "/report/" name)) $ H.toHtml name
                 H.a ! A.href "/report/new" $ "Create new report..."
