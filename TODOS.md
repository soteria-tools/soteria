# TODOs for the Typed migration

- in Value_codec, expose something like `to_bitvec : sint | sfloat | sptr -> sint DecayMap.SM.t`, to avoid this being repeated in 2-3 places in rtree_block
- can we not store fullptrs in rtree_block and instead store thinpointers, now that they're standalone values? might need changes in value_codec but is probably nicer (we still need to, upon reading them, convert them to full pointers, so we can treat all pointers uniformly within the interpreter; this is only about the memory)
- i think i can get rid of all the T.sptr functions in Typed, and replace them with their T.sptr_t equivalent; the fact a thin pointer has an "inner" pointer is an implementation detail we can abstract away
- completely unrelated: i think reading unions doesn't trigger TB reads, so we completely miss any UB there (e.g. behind maybeuninit) ? oops
- fix the encoder to also tell the callback what the size of each value is; we should also enforce this size as nonzero :)
- (later) figure out the story with writing non-atomic values in the heap; it should be ok, granted we complexify `split` a bit to actually split into all atomic parts on demand...?
