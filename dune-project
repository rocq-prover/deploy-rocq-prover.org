(lang dune 3.16)

(name rocq_prover_org_deploy)

(generate_opam_files true)

(source
 (github coq/rocq-prover.org-deploy))

(authors "Matthieu Sozeau")

(maintainers "Matthieu Sozeau")

(license MIT)

(documentation https://github.com/coq/rocq-prover.org-deploy)

(package
 (name rocq_prover_org_deploy)
 (synopsis "OCurrent deploy script for rocq-prover.org")
 (description "OCurrent deploy script for rocq-prover.org rebuilding and launching the docker image")
 (depends (prometheus-app (>= 1.0))
  (current (>= 0.6))
  current_git
  current_github
  fmt
  lwt
  (cmdliner (>= 1.2.0)))
 (tags
  (rocq infra)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
