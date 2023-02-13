/*Create an initial pool named pool.
Call the addStake function to mortgage 10 units of assets to the pool.
Call the removeStake function to withdraw 5 units of assets from the pool.
Returns the updated pool and final total stake.
Finally, we specify the desired outcome, i.e. the returned pool should be a pool containing the staker address and 5 units of assets, for a total stake of 5 units.
*/


testStakeAndUnstake : TestCase
testStakeAndUnstake = TestCase
  { description = "Stake and unstake"
  , program = do
      let pool = initialState
      pool' <- addStake 10 pool
      pool'' <- removeStake 5 pool'
      return (pool'', pool.totalStake + 10 - 5)
  , expect = Just (Pool (Address 0) 5 [(getAddress(), 5)])
