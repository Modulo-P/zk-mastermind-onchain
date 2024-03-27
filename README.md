# zk-mastermind-plutus

This is a PlutusTx smart contract that implements the Mastermind game. The aim of this project is to illustrate the use of Zero-Knowledge proofs. It is part of our F10 catalyst proposal.

Mastermind is an ideal use case for zk-Snarks, a zero-knowledge proof cryptographic protocol. This game is played by two players: the codemaster and the codebreaker. The codemaster selects a sequence of colored pegs, while the codebreaker attempts to guess the sequence within a limited number of turns. ZK-Snark can be used to verify the correctness of the codebreaker's guesses without revealing the actual code, ensuring privacy and integrity during gameplay.

## Project structure

The project has the following structure:

```bash
├── app
├── nix
├── output
├── src
└── test
```

* **App:** Folder with the main application function.
* **nix:** Configurations files to Nix package manager.
* **output:** Folder where .uplc compilation is outputted.
* **src** Folder with application logic.
* **test** Folder with test to the validator.

## Use

### Compilation

To compile the validator use the following commnand.

```bash
cabal exec zk-mastermind-onchain
```

This will automatically build the project and output the .uplc code.

### Execute test case 

```bash
cabal test zk-mastermind-onchain-test
```




