[OCurrent](https://www.ocurrent.org/) deployment pipeline for [rocq-prover.org](https://github.com/coq/rocq-prover.org).

## Building & Installing

Use an existing opam switch or:
```bash
opam switch create 5.2.0 5.2.0
opam switch link 5.2.0
```

Install the dependencies with:
```bash
opam install . --deps-only
```

To compile the deployer use:

```bash
dune build
dune install
```

## Running

To run the GitHub application, it needs to be installed in your github project 
and configured. You can then run it, adapting the following command:

```bash
deploy-rocq-prover_org --github-app-id 1100037 
  --github-account-allowlist=coq
  --github-private-key-file private_key 
  --github-webhook-secret-file webhook_secret
```

By default, this opens a web server for monitoring the deployment on localhost:8080.

See ```deploy-rocq-prover_org --help``` for other configuration options.
