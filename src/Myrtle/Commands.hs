module Myrtle.Commands where

import Data.Generics.Labels
import GHC.Generics (Generic)
import Data.Function ((&))
import Lens.Micro ((^.), (.~))

import Data.Text (Text, pack)
import Data.String.Interpolate ( i, iii )

import System.Process.Typed

cardano_cli, testnet :: Text
cardano_cli = "/root/.local/bin/cardano-cli"
testnet = "--testnet-magic 1097911063"

-- | Query the current status of the testnet.

query_testnet :: IO ExitCode
query_testnet = runProcess . shell $
    [iii|#{cardano_cli} query tip
    #{testnet}
    |]

-- | Querying the UTxOs at an address.
query :: String -> IO ExitCode
query = runProcess . shell . queryCmd

queryCmd :: String -> String
queryCmd addr = 
    [iii|#{cardano_cli} query utxo
    #{testnet}
    --address #{addr} 
    |]

-- | Sending ADA.
data Sending = Sending 
    { balance, amt :: Integer
    , tx_ins :: [ String ]
    , tx_outs :: [ String ]
    , current_slot, secs_valid  :: Integer
    , fee :: Integer
    , out_file :: FilePath
    } deriving Generic

sending = Sending
    { balance = 0
    , amt = 0
    , tx_ins = []
    , tx_outs = []
    , current_slot = 0
    , secs_valid = 0
    , fee = 0
    , out_file = "tx1.draft"
    }




mkTxIn, mkTxOut :: String -> String
mkTxIn txin = "--tx-in " <> txin <> " "
mkTxOut txout = "--tx-out " <> txout <> " "
-- #{mconcat $ map mkTxIn txsin}

send :: Sending -> IO ExitCode
send = runProcess . shell . sendCmd

sendCmd :: Sending -> String
sendCmd r = 
    [iii|#{cardano_cli} transaction build-raw
    --babbage-era
    #{mconcat $ map mkTxIn $ r ^. #tx_ins}
    #{mconcat $ map mkTxOut $ r ^. #tx_outs}
    --invalid-hereafter #{show $ r ^. #current_slot + r ^. #secs_valid}
    --fee #{show $ r ^. #fee}
    --out-file #{r ^. #out_file}
    |]

-- | Calculating the minimum fee for a transaction.
data Costing = Costing 
    { tx_file :: String 
    , tx_in_count :: Integer
    , tx_out_count :: Integer
    , witness_count  :: Integer
    } deriving Generic

costing = Costing
    { tx_file = "tx1.draft"
    , tx_in_count = 0
    , tx_out_count = 0
    , witness_count = 0
    } 



cost :: Costing -> IO ExitCode
cost = runProcess . shell . costCmd

costCmd :: Costing -> String
costCmd r = 
    [iii|#{cardano_cli} transaction calculate-min-fee
    #{testnet}
    --protocol-params-file protocol-v1.json
    --tx-body-file #{r ^. #tx_file}
    --tx-in-count #{show $ r ^. #tx_in_count}
    --tx-out-count #{show $ r ^. #tx_out_count}
    --witness-count #{r ^. #witness_count}
    |]

-- | Minting transaction.
data Minting = Minting
    { txin :: String
    , tx_in_collateral :: String
    , tx_out_addr :: String
    , amt  :: Integer
    , pid  :: String
    , tkn  :: String
    , script_file :: String
    , redeemer_file :: String
    , protocol_file :: String
    , change_addr :: String
    , tx_file :: String
    } deriving (Show, Generic)

minting = Minting
      { txin = oref
      , tx_in_collateral = oref
      , tx_out_addr = ""
      , amt = 1
      , pid = "13605c02e123c3d57ec9e3880f5a5dc39b07cd1a3f6ee440755c9f15"
      , tkn = "545354"
      , script_file = "minting-policy-0.plutusV2"
      , redeemer_file = "unit.json"
      , change_addr = ""
      , protocol_file = "protocol.json"
      , tx_file = "tx.unsigned"
      }
      where oref = "c593486d715ee8962f6ea54bd0dd74e71e21b18100fef1e177e32f7471ae977e#0"

mint :: Minting -> IO ExitCode
mint = runProcess . shell . mintCmd

mintCmd :: Minting -> String
mintCmd r =
    [iii|#{cardano_cli} transaction build
    --babbage-era
    --tx-in #{r ^. #txin}
    --tx-in-collateral #{r ^. #tx_in_collateral}
    --tx-out "#{r ^. #tx_out_addr} + 2000000 lovelace + #{show $ r ^. #amt} #{r ^. #pid}.#{r ^. #tkn}"
    --mint "#{show $ r ^. #amt} #{r ^. #pid}.#{r ^. #tkn}"
    --mint-script-file #{r ^. #script_file}
    --mint-redeemer-file #{r ^. #redeemer_file}
    --change-address #{r ^. #change_addr}
    --protocol-params-file #{r ^. #protocol_file}
    --#{testnet}
    --out-file #{r ^. #tx_file}
    |]

-- | Signing and submitting a transaction.
sign :: FilePath -> FilePath -> FilePath -> IO ExitCode
sign skfile unsigned signed = (runProcess . shell) 
    [iii|#{cardano_cli} transaction sign
    #{testnet}
    --tx-body-file #{unsigned}
    --signing-key-file #{skfile}
    --out-file #{signed}
    |]



submit :: FilePath -> IO ExitCode
submit tx = runProcess $ shell
    [iii|#{cardano_cli} transaction submit
    #{testnet}
    --tx-file #{tx}
    |]

--------------------------------------------------------------
-- | Sending Ada example
--------------------------------------------------------------
-- | sender is "/config/workspace/MyPrograms/swap-test/addr/seller.addr"
sender = "addr_test1vpra4fdvstljtkayvw069tq98q8nnly777d72vghnaxdr2qv068g5"
sender_key = "/config/workspace/MyPrograms/swap-test/keys/seller.skey"

-- | receiver = "/config/workspace/MyPrograms/swap-test/addr/buyer.addr"
receiver = "addr_test1vpvwy3sadnsetnzygwlts2z6zx9qx8gpfffsdfr4gm4vnxscwv35d"

-- | *Myrtle.Commands> query sender

sraw = sending
    & #balance .~ 490425078
    & #amt .~ 4000000
    & #tx_ins .~ [ "1fb637d1bfcfc792cfbf9cb869249789e9aff4e7636f3531f9de449f25d180cf#0" ]
    & #tx_outs .~ [ receiver ++ "+0", sender ++ "+0"]

-- | *Myrtle.Commands> send sraw

cargz = costing
    & #tx_in_count .~ 1
    & #tx_out_count .~ 2
    & #witness_count .~ 1

-- | *Myrtle.Commands> cost cargz

-- | *Myrtle.Commands> query_testnet

sr = sraw
    & #fee .~ 174433
    & #tx_outs .~ [ receiver ++ "+" ++ (show $ sr ^. #amt), sender ++ "+" ++ (show change)]
    & #current_slot .~ 68082963 -- ^ 2022-09-06 0917h
    & #secs_valid .~ 16 * 60
    & #out_file .~ "tx1.raw"
    where change = sr ^. #balance - (sr ^. #amt + sr ^. #fee)

-- | *Myrtle.Commands> sign sender_key (sr ^. #out_file) "tx1.signed"
-- | *Myrtle.Commands> submit "tx1.signed"

--------------------------------------------------------------
-- | Minting example.
--------------------------------------------------------------
minter = "addr_test1vpra4fdvstljtkayvw069tq98q8nnly777d72vghnaxdr2qv068g5"
minter_key = "/config/workspace/MyPrograms/swap-test/keys/seller.skey"

-- | *Myrtle.Commands> query minter

mr = minting 
    & #txin .~ "6a7fc863a31f5b2918921d86b757f22c3c0ee38b0cedfc4005d1463c9be8a7dc#0"
    & #tx_in_collateral .~ (mr ^. #txin)
    & #tx_out_addr .~ minter
    & #change_addr .~ minter
    & #amt .~ 3

-- | *Myrtle.Commands> mint margz

-- | *Myrtle.Commands> sign skfile "tx.unsigned" "tx.signed" 

-- | *Myrtle.Commands> submit "tx.signed"

-- | *Myrtle.Commands> query minter