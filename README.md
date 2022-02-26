[![CI](https://github.com/MiSawa/xq/actions/workflows/ci.yml/badge.svg)](https://github.com/MiSawa/xq/actions/workflows/ci.yml) [![crates.io](https://img.shields.io/crates/v/xq.svg)](https://crates.io/crates/xq)

# XQ

[JQ](https://stedolan.github.io/jq/) reimplemented purely in Rust.


## Caution
This program is under development. You probably want to use the original implementation of [jq](https://stedolan.github.io/jq/), or pure Go implementation of it called [gojq](https://github.com/itchyny/gojq/) built by [itchyny](https://github.com/itchyny) instead.


## Current state
- Most of the jq components that require syntactical support are, hmm, implemented and tested against queries taken from the [jq user manual](https://stedolan.github.io/jq/manual/).
  Exceptions are imports and module headers.
- Many builtin functions are missing, include those require intrinsic implementation.
- Need more unit tests. Most of the CLI options are missing.
- JSON and YAML formats are supported.


## Goals
- Learn jq.


## Ideas
- Since we use [serde](https://github.com/serde-rs/serde) to deserialize input / serialize output,
  we should be able to support any format which has serde implementation (and has multi-document support) without too much effort, except those require scheme (protobuf, thrift, etc.).

- As a library, we should be able to make anything that implements `Serialize` as the input type, and anything implements `Deserialize` as the output type.
  So in theory, we should be able to write a library that can handle following;
  ```rust
  #[derive(Serialize)]
  struct GitHubRepo {
      user_name: String,
      repo_name: String,
      stars: usize,
  }
  #[derive(Deserialize)]
  struct User {
      name: String,
      total_stars: usize,
  }
  let repos: Vec<GitHubRepo> = fetch_repos();
  let users: Vec<User> = xq::somefunction_slurp<GitHubRepo, User>(repos, r#"
      group_by(.user_name)[]
      | { name: .[0].user_name, total_stars: ([.[].stars] | add) }
  "#).collect();
  ```
  I mean, it's not something that one should really use. It's just something possible and interesting... isn't it?


## Install and run
Are you sure you want to use this??? It's not a daily-usable stage. Though if you want to try, you can install it via
```shell
$ cargo install xq
$ cat whatever.json | xq 'query goes here'
```
. Or to get the latet version, please clone this repository to your local, and run
```shell
$ cargo install --path path-to-cloned-dir
$ cat whatever.json | xq 'query goes here'
```
.  Alternatively,
```shell
$ cat whatever.json | cargo run -- 'query goes here'
```
in the cloned directory to try out them without installing.


## Uninstall
```shell
$ cargo uninstall xq
```
if you've installed via `cargo install`. Also remove the cloned directory if you've cloned the repository.


## Pubilsh new version
Note for myself.
```shell
$ git swithc master                 # make sure you're on the master branch
$ cargo release patch               # to dry-run the release
$ cargo release patch --execute           # to actually execute the release
```


## Acknowledgements
- Although this isn't a direct translation at all, I referred to [jq](https://stedolan.github.io/jq/manual/) built by [ Stephen Dolan](https://github.com/stedolan) and [gojq](https://github.com/itchyny/gojq/) built by [itchyny](https://github.com/itchyny). Thank you for the interesting product!


## Credits
- Test cases in [tests/from_manual](./tests/from_manual) are taken from the [jq user manual](https://stedolan.github.io/jq/manual/), distributed under [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/), Copyright (C) 2012 Stephen Dolan. Please refer to [mod.rs](./tests/from_manual/mod.rs) there for more detail.


## Author
[Mi_Sawa](https://github.com/MiSawa)


## LICENSE
MIT. Please refer to [LICENSE](./LICENSE) file.
