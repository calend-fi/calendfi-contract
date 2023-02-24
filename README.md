# CalendFi  Staking Version 1.0


##Staking in CalendFi
For any participator ( such as Startup Team or Token holder ) who would like to propose a stake pool in CalendFi, we provide a one-stop DeFi solution for you. Read the information below. You will understand how the stake pool in CalendFi works and what you need to prepare. 

## How to create a stake pool in CalendFi?
Any creator (Usually a token issuer) would like to propose a stake pool in CalendFi. Three parameters need to fill in to initialise a new stake pool. 
1. APRValue 
2. RewardTokenAmount
3. MaximumAliveTime

Where:
- APRValue refers to the Annual percentage rate that the stake pool pays for the participator.
- RewardTokenAmount refers to the total amount of Token that the Creator deposits to the smart contract to issue rewards 
- MaximumAliveTime refers to the maximum alive time of the pool.

## How to participate?
Anyone wants to stake their Token into the pool to earn the rewards. Only the "StakeAmount" is required.

## When can I claim my rewards/principal?
You are able to claim your rewards/principal at any time you like.

## When will the pool be terminated?
There are two situations in which the pool is going to be terminated.
- When the MaximumAliveTime is reached, the remainder rewards Token will be sent back to Creator's wallet, and the rewards and principal of all participants will be returned to their wallet.
- When the stake pool runs out of RewardToken, the rewards and principal of all participants will be returned to their wallet.


## Ongoing development plan
- [ ] In Version 1.0:
-- CalendFi Staking only support creating the staking pool of ADA token.
-- Anyone is allowed to create a staking pool.

- [ ] Version 1.1: 
-- CalendFi Staking will support the staking pool of various tokens.
-- The Creator will be able to top up their reward token.

- [ ] Version 1.2:
-- The Creator can stake one Token and reward two reward tokens.
-- The Creator is able to set up a non-constant APRValue.
