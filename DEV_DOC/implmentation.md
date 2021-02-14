# implementation

## roadmap

### checkpoints

- define category with my preferred syntax

### list
> italic are done.
> bold ones are current important TODOs.

- core
  - *predicative & cumulative strict props, sets*
    - `Q` cmumulative between props and sets?
  - *pi type, lambda*
  - *(inductive) record & enum, (recursive) pattern lambda*
    - **ordered subtyping**
    - subset subtyping for enum
    - sublist subtyping for record
  - **setoid type theory**
  - *core syntax using dbi*
  - *normalization by evaluation*
    - *type directed conversion checking*
    - `Q` should I use glued evaluation by mutable variables?
    - `Q` should I use spine? (yes?)
  - soundness
    - *core checker*
    - termination checker
    - partial/total checker
    - coverage checker
- elaboration
  - `Q` subtyping constraint is solved as equality constraint for stuck term, I don't really know how to support that, it seems complicated
  - *universe lifting*
  - *implicit variables*
  - *extension methods (what we have is very simple, and slow)*
  - *namespaces (very simple even without import for now!)*
  - **typeclass, bundled and unbounded (check Arend)**
  - **generated instances**
  - inductive families like Agda
  - extensible syntax & macro & meta programming (check Lean4)
  - `Q` how to implement a elaborator without a mutable context & meta context? rewrite in monad? or use a dsl? my current approach is similar to [simple sub](https://github.com/LPTK/simple-sub) where the meta variable IS the context, and there are no separate meta context. Also it seems all major implementation use monad + single meta context, how do we implement with local meta contexts?
  - tactics
- bootstrapping & general programming language
  - builtin types
  - mutable records and array
  - union type & flow sensitive typing?
  - JS backend
  - Rust backend
- ui
  - presentation
  - interaction
- infra
  - incremental compiling
  - crates & package manager
  - more modular, abstract core
    - used as a library with interactive UI
    - enable visualized reductions
    - attach information transparently
    - abstract away some non-essential features: dbi

### unification

- type directed unification
