# Coding guidance

- Follow the [semantic encapsulation contract](README.md#semantic-encapsulation) when changing conversion or diff behavior. Feature policy belongs in feature modules, not generic algorithms; preserve the architecture and behavioral regression tests.
- Prefer small, explicit module contracts and ordinary Emacs facilities over custom registries, discovery or declaration DSLs.
- Keep changes scoped and preserve this repository's space-indented Elisp style.
- Run `make compile` and `make test` with the project's declared dependencies. Retain configured commit signing; never bypass failed checks or signing.
