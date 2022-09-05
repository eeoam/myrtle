module Myrtle.Commands where

import Data.Generics.Labels
import GHC.Generics (Generic)
import Data.Function ((&))
import Lens.Micro ((^.), (.~))

import Data.Text (Text, pack)
import Data.String.Interpolate ( i, iii )

import System.Process.Typed

--import Segor.Types (Sending(..), Minting(..), Spending(..))

cardano_cli, testnet :: Text
cardano_cli = "/root/.local/bin/cardano-cli"
testnet = "--testnet-magic 1097911063"

query :: String -> IO ExitCode
query = runProcess . shell . queryCmd

queryCmd :: String -> String
queryCmd addr = 
    [iii|#{cardano_cli} query utxo
    #{testnet}
    --address #{addr} 
    |]


--------------------------------------------------------------
-- | sender is "/config/workspace/MyPrograms/swap-test/addr/seller.addr"
sender = "addr_test1vpra4fdvstljtkayvw069tq98q8nnly777d72vghnaxdr2qv068g5"
sender_key = "/config/workspace/MyPrograms/swap-test/keys/seller.skey"

-- | receiver = "/config/workspace/MyPrograms/swap-test/addr/buyer.addr"
receiver = "addr_test1vpvwy3sadnsetnzygwlts2z6zx9qx8gpfffsdfr4gm4vnxscwv35d"