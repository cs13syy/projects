The Follow-The-Leader (FTL) algorithm is a simple algorithm for solving online prediction problems. Imagine that you have a committee of experts, each of which suggests a strategy. At each time point, you pick an expert and follow his or her advice, which yields some associated cost (or reward) at the next time step. Your goal is to minimize the total cost/maximize your total reward.

Follow-The-Leader uses a very simple approach: track the performance of all experts over all previous time steps, then select the expert/strategy/etc that has performed the best so far, and follow its advice on the next round. Update everything and choose again.

This approach is called follow the leader, in part because you're following (the advice) of the leading strategy (e.g., the player with the best score in a game). Follow the leader is also the name of a traditional children's game, where everyone imitates the movements of a "leader", which is close enough to the algorithm to be (mildly) funny.
