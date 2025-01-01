[OCurrent](https://www.ocurrent.org/) deployment pipeline for [rocq-prover.org](https://github.com/coq/rocq-prover.org).

## Building & Installing

Use an existing opam switch or:
```bash
opam switch create 5.2.0 5.2.0
opam switch link 5.2.0
```

To compile the deployer use:

```bash
dune build
dune install
```

## Running

