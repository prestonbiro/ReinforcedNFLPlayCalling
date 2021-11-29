# ReinforcedNFLPlayCalling
Code to accompany Biro and Walker's 2021 Paper - A Reinforcement Learning Based Approach to Play Calling in Football

For access to the paper, see: LINK TO PAPER HERE

This Github contains three R scripts that incorporate a tutorial to using the methods described in Biro and Walker's 2021 Paper - A Reinforcement Learning Based Approach to Play Calling in Football. Additionally, data has been provided to save the users' time in the optimization process. To follow the tutorial sequentially, download all scripts and follow them in the following order:

- TutorialHelperFunctions.R
- TutorialFindTransitionProbs.R
- TutorialFindUtilities.R

More might be added to this Github at some point. If you do have questions or feel something is incorrect, please email me at prestonbiro@utexas.edu.

## Terminology

The following terms are used in the tutorial functions but may not line up with the way you think about of them, so I define them here:
- DOWN, down: The down for the offense. I.e. DOWN = 1 => 1st down
- DIST, distance: The amount of yards needed for the offense to achieve to gain a first down. I.e. DIST = 10 => 10 yards to first down
- LOS, line of scrimmage: The amount of yards needed for the offense to score a touchdown. Always a positive number, and does not require a lable of the team's side of the field. I.e. LOS = 60 => 60 yards from scoring a touchdown, or on the offense's side of the field 40 yard line
