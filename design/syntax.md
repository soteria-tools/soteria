# Logic and Syntax

## Introduction

Soteria is originally designed as a *semantic* framework. The syntactic representation of symbolic values, that is, interpreters are constructed independently of the existence of variables or the AST of symbolic values. In essence, this is what makes Soteria "natural" to use for writing interpreters, it hides away the "symbols" in "symbolic execution".

Unfortunately, this beautiful abstraction makes it impossible to reason about function summaries (used e.g. in bi-abduction), or type summaries (used in RUXt). Our goal is to extend Soteria with a way to reason about syntax, without adding too much complexity, and without exposing too much grungy details to the user.

Note that Soteria already has some embryonic support for syntax, used for bi-abduction in Soteria-C. However, this support is limited, does not generalise well, and is currently incompatible with over-approximation (which is necessary for some optimisations in RUXt).

## Syntactic reasoning

We re-use concepts from Compositional Symbolic Execution (CSE) to reason about syntax. All CSE tools use a notion of *producer* and *consumer* of assertion, where a producer is morally a "separation logic assume", and a consumer is a "separation logic assert". Producer "adds" an assertion to the current symbolic state, while a consumer removes an assertion from the current state.

Function specifications can then be *executed* by first consuming the precondition, and then producing the postcondition. Similarly, implications of the form `A ⊢ B` can be checked by producing `A` and then consuming `B` (in OX mode).

# TO EXPLAIN

- Identity producer
- Syntax needs of substitution, define it in a single place
- Why ability to consume needs to be checked pre-emptively


## Bikeshedding list

- All `subst` functions require a `Typed.cast`, can we get around this?
- fixes are now `syn list list`, this makes sense but can we add some type beauty to it?
- The horrible interface Value.Syn.Subst
- The specifications of the various functions in Value.Syn have too many invariants. How much could we simplify it?
- `State_monad`:
  - Exactly duplicates the code of Symex for Producer and Consumer. Needs to become a functor to avoid this
  - Has to expose `nondet_UNSAFE`, though possibly we could make `subst` produce something *within a monad*. Modular explicit would be extremely helpful, but in the meantime we can make a mini functor that we instantiate inline.


## TODOS
  - Go through Symex interface and remove all things that are useless. Specifically, old consume_pure!