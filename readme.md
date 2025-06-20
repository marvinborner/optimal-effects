# Optimal Effects

> POC/Experiments for extended abstract

Note: We have forked GraphRewriting locally since the upstream version
can not be built with current GHC. We have also adapted it to our needs
minimally.

## Usage

``` bash
cd impl/
cabal build
cabal run opteff-exe -- --help
cat samples/direct/fac.front | cabal run opteff-exe -- --visualize --target direct
```

## Notes

Most of this is a straight-forward implementation of the introduced
monadic/direct rules using the `graph-rewriting` library.

We implement the abstract algorithm without bookkeeping, although it
could be added trivially (see the original [lambdascope
source](https://github.com/jrochel/graph-rewriting/tree/master/lambdascope)).
Full beta-optimality can then be achieved by the LMO reduction order.

For this POC, effects are mocked and not connected to the real world,
see `Language.Generic.Effects`.

Implementationally interesting (mostly due to their conciseness via
paramorphisms and de Bruijn indices) could be the transformer from the
`Front` language to (Effectful-)`Lambda` in
`Language.Front.Transformer.Lambda`, and also the further transformers
from `Lambda` to graph `Node`s
(e.g. `Language.Lambda.Transformer.Direct`)
