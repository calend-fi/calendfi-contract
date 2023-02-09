/*The stake balance of each staker used to store each NFT. Three functions are defined in the code:

stakeNFT: Used for staker to stake the specified NFT.
unstakeNFT: Used by the staker to cancel the stake operation on the specified NFT.
getStakeBalance: Used to query the stake balance of the specified NFT and staker.*/

{-|
    Module      : NFTStaking
    Description : Description of the NFTStaking
    Copyright   : P2P Solutions Ltd.
    License     : GPL-3
    Maintainer  : jack
    Stability   : developer
 -}

    
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Map.Strict as Map

type NFT = Text
type Staker = Text
type Balance = Int

data NFTStaking = NFTStaking
    { stakedNFTs :: Map.Map NFT (Map.Map Staker Balance)
    }

stakeNFT :: NFTStaking -> NFT -> Staker -> Balance -> NFTStaking
stakeNFT nftStaking nft staker balance =
    nftStaking { stakedNFTs = updatedStakedNFTs }
  where
    updatedStakedNFTs =
        Map.alter
            (Just . maybe (Map.singleton staker balance) (Map.insert staker balance))
            nft
            (stakedNFTs nftStaking)

unstakeNFT :: NFTStaking -> NFT -> Staker -> NFTStaking
unstakeNFT nftStaking nft staker =
    nftStaking { stakedNFTs = updatedStakedNFTs }
  where
    updatedStakedNFTs =
        Map.alter
            (fmap (Map.delete staker))
            nft
            (stakedNFTs nftStaking)

getStakeBalance :: NFTStaking -> NFT -> Staker -> Maybe Balance
getStakeBalance nftStaking nft staker = do
    stakers <- Map.lookup nft (stakedNFTs nftStaking)
    Map.lookup staker stakers

