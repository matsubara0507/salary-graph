workspace(name = "salary-graph")

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

http_archive(
    name = "rules_python",
    url = "https://github.com/bazelbuild/rules_python/releases/download/0.5.0/rules_python-0.5.0.tar.gz",
    sha256 = "cd6730ed53a002c56ce4e2f396ba3b3be262fd7cb68339f0377a45e8227fe332",
)

http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-0.14",
    url = "https://github.com/tweag/rules_haskell/archive/refs/tags/v0.14.tar.gz",
    sha256 = "851e16edc7c33b977649d66f2f587071dde178a6e5bcfeca5fe9ebbe81924334",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

stack_snapshot(
    name = "stackage",
    packages = [
        "aeson",
        "base",
        "bytestring",
        "containers",
        "directory",
        "elm-bridge",
        "file-embed",
        "filepath",
        "fsnotify",
        "http-api-data",
        "http-media",
        "servant-elm",
        "servant-server",
        "stm",
        "warp",
    ],
    local_snapshot = "//:stack-snapshot.yaml",
)

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

rules_haskell_toolchains(version = "9.2.1")

http_archive(
    name = "rules_elm",
    sha256 = "a9db7f55e3693ab94a60cbf602221095514aec6541253b21cc89f0ba1365d87c",
    urls = ["https://github.com/matsubara0507/rules_elm/releases/download/v1.0.0/rules_elm-v1.0.0.zip"],
)

load("@rules_elm//elm:repositories.bzl", rules_elm_repositories = "repositories")

rules_elm_repositories()

load("@rules_elm//elm:toolchain.bzl", rules_elm_toolchains = "toolchains")

rules_elm_toolchains(version = "0.19.1")
