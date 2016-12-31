{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Views.Report where

import Data.Monoid
import Control.Monad
import NeatInterpolation
import Text.Blaze.XHtml5 ((!))
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import qualified Text.Blaze.Bootstrap as H
import qualified Data.Text as T

data ReportView = ReportView
  { report_rendered :: T.Text
  , report_name :: T.Text
  , report_code :: T.Text
  }

reportView :: ReportView -> H.Html
reportView (ReportView{..}) =
  do H.div ! A.class_ "blog-header" $
      H.p ! A.class_ "lead blog-description" $ H.toHtml $ T.append "Report " report_name
     H.div ! A.class_ "row report" $
      do H.preEscapedText report_rendered
         H.hr ! A.style "margin-top: 50px;"
         H.h3 "Report code"
         H.pre $ H.code ! A.class_ "haskell" $ H.toHtml report_code

exampleReport = [string|
    heading "An example report"

    let xy = points [(0, 1), (2, 3), (5, 1)]

    plot $ do
      dat xy
      add

    table "Foo" "Bar" xy
    |]

createReportView :: H.Html
createReportView =
  do H.div ! A.class_ "blog-header" $
      do H.h1 ! A.class_ "blog-title" $ "Coercive Data Analysis Framework"
         H.p ! A.class_ "lead blog-description" $ "Create a new report"
     H.div ! A.class_ "row" $
      do H.form ! A.action "/report" ! A.method "post" $
          do H.row $ H.textarea ! A.class_ "report-input" ! A.name "report"
                                ! A.rows "20" ! A.cols "80" $
               H.toHtml exampleReport
             H.row ! A.style "margin-top: 10px;" $
               H.input ! A.type_ "submit" ! A.value "Create report"
