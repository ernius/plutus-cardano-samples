# Math Bounty Contract

## Description

This is a simple smart contract which locks deposited funds under a math bounty (specifically figuring out what number squared matches a given value). The user who locks their ada under this contract provides a `target` which is the number that must be the result of the squaring.

In other words:

```
x * x = target
```

This `target` is an integer and is placed within the script Datum of the locked output UTXO.

Once the funds are locked under the contract with the datum, anyone can solve the math bounty by providing the correct number (providing the correct `x`). 

They do this by submitting a transaction with their answer for `x` as the Redeemer. If their answer is correct, then they get to spend the locked bounty UTXO, and take the funds from within it.

## Code

NOTE: use plutus-app commit 4edc082309c882736e9dec0132a3c936fe63b4ea

