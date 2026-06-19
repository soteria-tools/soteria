# TODOs for the Typed migration

- ⏭️ in Value_codec, expose something like `to_bitvec : sint | sfloat | sptr -> sint DecayMap.SM.t`, to avoid this being repeated in 2-3 places in rtree_block
  - The two sites that look duplicated (`split_rval` and `decode_lazy`) actually
    have different metadata semantics: `split_rval` concatenates a wide pointer's
    metadata into the bitvector, whereas `decode_lazy` drops it (`Sptr.decay
    (Ptr.ptr_of v)`) because its leaves are already split. A single shared helper
    can't be behaviour-preserving for both, so this isn't a safe readability-only
    change.
- ⏭️ can we not store fullptrs in rtree_block and instead store thinpointers, now that they're standalone values? might need changes in value_codec but is probably nicer (we still need to, upon reading them, convert them to full pointers, so we can treat all pointers uniformly within the interpreter; this is only about the memory)
  - Large change to the memory representation + value_codec; correctness-critical
    and out of scope for a typed-value readability pass.
- ⏭️ i think i can get rid of all the T.sptr functions in Typed, and replace them with their T.sptr_t equivalent; the fact a thin pointer has an "inner" pointer is an implementation detail we can abstract away
  - The raw `T.sptr` functions are inherited from the base `Typed_intf.S` (via
    `include module type of Ptr`) and are genuinely needed inside `sptr.ml`,
    which is the abstraction layer that builds thin pointers from inner ones.
    Removing them would mean restructuring the inherited interface and rewriting
    `sptr.ml`'s internals for little gain.
- ⏭️ completely unrelated: i think reading unions doesn't trigger TB reads, so we completely miss any UB there (e.g. behind maybeuninit) ? oops
  - A suspected soundness bug, unrelated to typed-value ergonomics; needs its own
    investigation rather than a refactor.
- ⏭️ fix the encoder to also tell the callback what the size of each value is; we should also enforce this size as nonzero :)
  - Changes encoder semantics (correctness-critical); not a readability change.
- ⏭️ (later) figure out the story with writing non-atomic values in the heap; it should be ok, granted we complexify `split` a bit to actually split into all atomic parts on demand...?
  - Explicitly marked "(later)".
- ⏭️ maybe we can define a ppx or macro to do the unwrapping and casting of arguments? could be cute...
  - Large, speculative ("could be cute"); a PPX is a big investment versus the
    existing cast helpers.
- ⏭️ clarify or improved the [Adt.Checked] operations, e.g. by recursively filling and checking the fields of nested ADTs.
  - Speculative: all current `Adt.Checked.mk_enum` call sites build shallow enums,
    so recursive field-filling has no motivating use yet.
- ⏭️ redefine [T.any] as something like [sint | sfloat | adt | sptr_f ], to avoid stuff like [sbool] being allowed
  - Touches the core value type (~280 use sites) and isn't obviously correct (need
    to confirm `sbool`/`sloc`/`sptr` never legitimately flow as values); too broad
    and risky for an incremental refactor — deserves its own focused change.
- ⏭️ Typed.Ptr.with_meta ?
  - No real call-site need: the metadata-rebuild sites actually modify the thin
    pointer (`with_tag`) and keep the metadata, which a `with_meta` wouldn't
    capture.
- ⏭️ Typed.Ptr.wrap (ptr -> ptr)? or just porting all the offset etc. functions? maybe [Sptr.Full.<fn> = lift <fn>]
  - There is no `Sptr.Full`, and the split -> modify -> rebuild pattern is already
    clear at call sites; lifting wasn't warranted.
- ✅ Possibly a [Typed.Adt.as_tuple{1,2,3}] that casts to an N-tuple, to avoid ugly list pattern matching?
  - Added `Typed.Adt.as_tuple1/2/3`; applied in alloc, optim and value_codec.
- ⏭️ We should probably have a special case that pre-creates and pre-hashes regularly used constants, like [unit], or the [cmp] variants. Not sure about the latter because that requires having the [TypeDecl] to type it properly, so it needs a crate :(
  - An optimization, not a readability change; the author themselves doubts the
    `cmp` variant (needs a crate to type), and `unit` is cheap to build.
- ✅ Add [Typed.Adt.nth_field idx v]/[Typed.Adt.with_field idx f v] to avoid the [as_tuples]
  - Already present, under the names `Typed.Adt.field_of` / `set_field` /
    `update_field`.
- ⏭️ Maybe make `nondet_valid` (and others?) receive a `'a ty` argument, to avoid needing to cast on each use
  - `nondet_valid` dispatches on the runtime Charon type and can yield any kind
    (structs, enums, ...), so the result kind isn't statically known; a typed
    wrapper would just be `cast (nondet_valid ty)`, no real gain.
- ✅ A utility to unwrap the pointer out of a box
  - Already present as `Value_codec.ptr_of_box`.
- ⏭️ A utility like `as_checked_ref` that returns a pointer option, with `Some` for references and boxes (i.e. a simpler version of ref_tys_in)
  - The ref/box-to-pointer logic lives essentially in one function (`ref_tys_in`),
    which also needs the box's allocator/marker to rebuild it; not enough
    repetition to justify a new helper.
- ⏭️ Maybe `type +'a ret =  ...` fixes all our problems?? do i just need to specify the invariance of all types....
  - Speculative; `'a t`/`'a ty` are already covariant (`+'a`) and no concrete
    variance problem was identified.
- ✅ Typed.Ptr.null for a full ptr (added as `Sptr.null_f`, not `Typed.Ptr`, since the null thin-pointer convention — size 0, align 1 — lives in `Sptr.null`, and `Typed` is below `Sptr` in the dependency order)
