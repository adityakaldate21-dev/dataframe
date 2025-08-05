<h1 align="center">
  <a href="https://dataframe.readthedocs.io/en/latest/">
    <img width="100" height="100" src="https://raw.githubusercontent.com/mchav/dataframe/master/docs/_static/haskell-logo.svg" alt="dataframe logo">
  </a>
</h1>

<div align="center">
  <a href="https://hackage.haskell.org/package/dataframe">
    <img src="https://img.shields.io/hackage/v/dataframe" alt="Hackage Latest Release"/>
  </a>
  <a href="https://github.com/mchav/dataframe/actions/workflows/haskell-ci.yml">
    <img src="https://github.com/mchav/dataframe/actions/workflows/haskell-ci.yml/badge.svg" alt="CI"/>
  </a>
</div>

<p align="center">
  <a href="https://dataframe.readthedocs.io/en/latest/">User Guide</a>
  |
  <a href="https://discord.gg/XJE5wKT2kb">Discord</a>
</p>

# DataFrame

A fast, safe, and intuitive DataFrame library.

## Why use this DataFrame library?

- Encourages concise, declarative, and composable data pipelines.
- Static typing makes code easier to reason about and catches many bugs at compile time—before your code ever runs.
- Delivers high performance thanks to Haskell’s optimizing compiler and efficient memory model.
- Designed for interactivity: expressive syntax, helpful error messages, and sensible defaults.
- Works seamlessly in both command-line and notebook environments—great for exploration and scripting alike.

## Example usage

### Interactive environment

![Screencast of usage in GHCI](./static/example.gif)

**Key features in example:**

- Intuitive, SQL-like API to get from data to insights.
- Create typed, completion-ready references to columns in a dataframe using `:exposeColumns`
- Type-safe column transformations for faster and safer exploration.
- Fluid, chaining API that makes code easy to reason about.

### Standalone script example

```haskell
-- Useful Haskell extensions.
{-# LANGUAGE OverloadedStrings #-}  -- Allow string literal to be interpreted as any other string type.
{-# LANGUAGE TypeApplications #-}  -- Convenience syntax for specifying the type `sum a b :: Int` vs `sum @Int a b`.

import qualified DataFrame as D  -- Import for general functionality.
import qualified DataFrame.Functions as F  -- Import for column expressions.

import DataFrame ((|>))  -- Import chaining operator with unqualified.

main :: IO ()
main = do
    df <- D.readTsv "./data/chipotle.tsv"
    let quantity = F.col "quantity" :: D.Expr Int  -- A typed reference to a column.
    print (df
      |> D.select ["item_name", "quantity"]
      |> D.groupBy ["item_name"]
      |> D.aggregate [ (F.sum quantity)     `F.as` "sum_quantity"
                     , (F.mean quantity)    `F.as` "mean_quantity"
                     , (F.maximum quantity) `F.as` "maximum_quantity"
                     ]
      |> D.sortBy D.Descending ["sum_quantity"]
      |> D.take 10)
