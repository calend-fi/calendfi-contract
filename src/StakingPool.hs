/* This code defines a StakingPool data type, which is simply a mapping from Account (represented as a String) to Stake (represented as an Int). 
The emptyPool function returns a new StakingPool with no stakes.
 The addStake and removeStake functions update the pool by adding or removing a specified stake from a given account, 
 respectively. The totalStake function calculates the total stake in the pool by summing up all the stakes in the mapping.*/

module StakingPool where

import Data.Map (Map)
import qualified Data.Map as Map

{-|
    Module      : StakingPool
    Description : Description of the StakingPool
    Copyright   : P2P Solutions Ltd.
    License     : GPL-3
    Maintainer  : jack
    Stability   : developer
 -}

type Stake = Int
type Account = String

data StakingPool = StakingPool { stakes :: Map Account Stake }

emptyPool :: StakingPool
emptyPool = StakingPool Map.empty

addStake :: Account -> Stake -> StakingPool -> StakingPool
addStake account stake pool =
  let currentStake = Map.findWithDefault 0 account (stakes pool)
      newStakes = Map.insert account (currentStake + stake) (stakes pool)
  in pool { stakes = newStakes }

removeStake :: Account -> Stake -> StakingPool -> StakingPool
removeStake account stake pool =
  let currentStake = Map.findWithDefault 0 account (stakes pool)
      newStakes = Map.insert account (currentStake - stake) (stakes pool)
  in pool { stakes = newStakes }

totalStake :: StakingPool -> Stake
totalStake = sum . Map.elems . stakes
