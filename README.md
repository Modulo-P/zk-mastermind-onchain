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

### Appendix: Dapp repositories

The relevant repositories of the mastermind Dapp are as follows: 

1. [zk-mastermind-webdapp:](https://github.com/Modulo-P/zk-mastermind-webapp) Frontend application of the Mastermind Dapp.
2. [zk-mastermind-backend:](https://github.com/Modulo-P/zk-mastermind-backend) Backend application of the Mastermind Dapp.
3. [zk-mastermind-backend-onchain:](https://github.com/Modulo-P/zk-mastermind-backend-onchain) Hada mint contrat of the Mastermind Dapp.
4. [zk-mastermind-docker:](https://github.com/Modulo-P/zk-mastermind-docker) Docker container with the Kupo, Hydra and Cardano node components of the Dapp.
5. [zk-mastermind-plutus:](https://github.com/Modulo-P/zk-mastermind-plutus) PlutusTx validator that implements the logic of the game.
6. [zk-mastermind-aiken:](https://github.com/Modulo-P/zk-mastermind-aiken) Aiken validator that implements the logic of the game.


