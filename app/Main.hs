{-# LANGUAGE OverloadedStrings #-}  -- Allow string literals to be used as Text, etc.
{-# LANGUAGE TypeApplications #-}   -- Enables type applications like `sum @Int a b`

import qualified DataFrame as D               -- General DataFrame functionality
import qualified DataFrame.Functions as F     -- Column operations and expressions
import           DataFrame ((|>))             -- Chaining operator
import           Control.Exception (catch, IOException)

main :: IO ()
main = do
    -- Attempt to read TSV and process, with error handling
    result <- (D.readTsv "./data/chipotle.tsv" >>= processDataFrame) `catch` handleReadError
    print result

-- Processing logic for the DataFrame
processDataFrame :: D.DataFrame -> IO D.DataFrame
processDataFrame df = do
    let quantity = F.col "quantity" :: D.Expr Int
    return $ df
        |> D.select ["item_name", "quantity"]
        |> D.groupBy ["item_name"]
        |> D.aggregate
            [ F.sum quantity     `F.as` "sum_quantity"
            , F.mean quantity    `F.as` "mean_quantity"
            , F.maximum quantity `F.as` "maximum_quantity"
            ]
        |> D.sortBy D.Descending ["sum_quantity"]
        |> D.take 10

-- Handle file not found or read error
handleReadError :: IOException -> IO D.DataFrame
handleReadError e = do
    putStrLn $ "Error reading file: " ++ show e
    return D.empty
