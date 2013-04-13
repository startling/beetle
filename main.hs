{-# Language OverloadedStrings #-}
-- base
import Data.Monoid
import Control.Applicative
-- trifecta
import Text.Trifecta
-- beetle
import Beetle.Parse
import Beetle.Abstract
import Beetle.Compile
import Language.Javascript
-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- blaze-markup
import Text.Blaze
import Text.Blaze.Internal
-- blaze-html
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

-- Read the javascript runtime, read the beetle file, and
-- write it out as a single html file.
main :: IO ()
main = do
  runtime <- T.readFile "runtime.js"
  d <- parseFromFile (many dec) "examples/example.beetle"
  e <- maybe (return []) return $ d
  let js = T.intercalate "\n\n" . map
       (printStatement . declaration (Variable "fuck")) $ e
  let h = html $ T.intercalate "\n\n" [runtime, js]
  writeFile "out.html" $ renderHtml h

-- An html template.
html :: Text -> Html
html js = B.docTypeHtml $ do
  B.head $ do
    B.meta ! A.charset "utf-8"
    B.title "beetle game"
    B.script (return ())
      ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
    B.script
      ( preEscapedText js
      ) ! A.type_ "text/javascript"
  B.body $ do
    B.div (return ()) ! A.id "thing"