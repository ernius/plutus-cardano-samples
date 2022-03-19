# Plutus examples

## Math Bounty Contract

### Description

This is a simple smart contract which locks deposited funds under a math bounty (specifically figuring out what number squared matches a given value). The user who locks their ada under this contract provides a `target` which is the number that must be the result of the squaring.

In other words:

```
x * x = target
```

This `target` is an integer and is placed within the script Datum of the locked output UTXO.

Once the funds are locked under the contract with the datum, anyone can solve the math bounty by providing the correct number (providing the correct `x`). 

They do this by submitting a transaction with their answer for `x` as the Redeemer. If their answer is correct, then they get to spend the locked bounty UTXO, and take the funds from within it.

### Code

NOTE: use plutus-app commit 4edc082309c882736e9dec0132a3c936fe63b4ea

### Code 

- MathBounty: first version
- MathBountyFilter: fix consuming wrong datum contracts
- MathBount2: just consumes one utxo in another way

### Playground test cases

- Wallet 1 make a bounty, and Wallet 2 consumes it
- Wallet 1 make 2 bounties in the same slot, Wallet 2 consumes the existen one
- Wallet 1 make 2 bounties same datum in the different slots, Wallet 2 consumes them
- Wallet 1 and 2 makes 2 bounties with same datum in the same slot, Wallet 2 consumes all
- Wallet 1 and 2 makes 2 bounties with different datums in the same slot, Wallet tries to consume
- Introduce MathBountyFilter fixing previous problem, and test again previous case

## Run playground

### Run nix-shell in [plutus-app](https://github.com/input-output-hk/plutus-apps) repository

```
cd plutus-app
git checkout 4edc082309c882736e9dec0132a3c936fe63b4ea
nix-shell
```

### Run playground

Inside a two plutus-app's nix-shells run:

```
cd plutus-playground-client
plutus-playground-server
```

And:


```
cd plutus-playground-client
npm start
```

Playground should be accesible in: https://0.0.0.0:8009/


### Run documentation


Inside a plutus-app's nix-shell run:

```
build-and-serve-docs
```

Playground should be accesible in: http://localhost:8002/haddock

