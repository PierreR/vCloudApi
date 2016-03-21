-- |

module VCloud.Prelude (
    module Exports
  , LazyByteString
  , LazyText
  , print
) where

-- General
import           Control.Monad.IO.Class    as Exports
import           Control.Monad.Trans.Maybe as Exports
import           Data.ByteString           as Exports (ByteString)
import           Data.Map                  as Exports (Map)
import           Data.Text                 as Exports (Text)

-- More specific
import           BasePrelude               as Exports hiding (break, loop, print)
import           Control.Lens              as Exports hiding (index, lazy,
                                                       uncons, (&))
-- VCLoud specific

import           Network.Connection        as Exports (TLSSettings (TLSSettingsSimple))
import           Text.XML                  as Exports (Element (Element), Document (Document),
                                                       Prologue (Prologue))
import qualified Prelude
import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy

type LazyByteString = Data.ByteString.Lazy.ByteString
type LazyText = Data.Text.Lazy.Text

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print
