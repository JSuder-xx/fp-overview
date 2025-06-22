# fp-overview

A quick tour through the merits of pure statically typed "fancy" functional languages.

## Elm

Elm is not a fancy functional language. The Elm community makes a strong case for Elm as a language and an architecture.

- Almost zero run-time bugs. Stack overflows possible but easily avoided.
- Immutability makes code easier to reason about and debug.
- Great error messages.
- Fast compiler.
- Extraordinarily _simple_ and _stable_ language that is easy to learn along with _one and only one_ UX framework amplifies concepts over language feature noise.
- Incredible type inference.
- Well documented and semantically versioned libraries.
- Very strong tooling close to TypeScript level.
- A community interested in making functional ideas accessible.
- Single very simple Module system (no more worries about commonjs, esnext, amd, etc.) with a single module syntax and which detects circular dependencies.
- Structural and nominal typing provides tools for both re-use/flexibility and encapsulation.

## PureScript / Haskell

I haven't seen many good sales pitches for fancier functional languages like PureScript which support higher-kinded types and type classes.

- Some advocates talk about abstraction theory and mechanics while failing to show practical application. Functional abstractions simplify and normalize Api's
and provide a provably correct (algebraic law abiding) foundation for Api's and application types and behaviors. The `Abstractions` folder in this repository starts
with two examples of parlour tricks for a "Wow!" factor to pique interest and then the [E03Traversable](./src/Abstraction/E03Traversable.purs) module gives a glimpse
into the ability of abstractions to simplify Api's.
- Some pontificate on FP in a way that suggests all or most of the tools of software quality from the OO community no longer apply which introduces unnecessary wariness
that FP is unfit for the enterprise.
  - SOLID, encapsulation, and abstraction are every bit as applicable in FP as in OO. Furthermore, the notion that behavior
  can and often should be grouped with types exists in PureScript, OCaml, F#, and Haskell communities. In FP languages behavior is
  not _strictly_ required to be organized around its type such that discipline is required. Behavior _can_ be defined in modules other than the module which defines a type but .NET
  also appreciates the merits of _some_ extrinsic behavior (see [Extension members](https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/extension-methods)).
  By defining one major type per module and including core functions operating on that type in said module one can achieve all of the **COHERENCE** and discoverability one would expect of an OO language.
  The smallest unit of composition in a functional language is a function while in OO languages it is often an entire class (Decorator Pattern, Adapter Pattern). Since FP languages feature
  greater composition precision that enables improved transparency and decreased **COUPLING**.
    - ⚠️ Since the smallest unit in an FP language is a function it is easier for undisciplined engineers to create spaghetti in an FP language than in an OO language.
    OO languages create pressure on engineers to group functions with types (anemic classes still occur). OO is like data modeling 🚲 training wheels; helpful for novice and intermediate engineers.
  - My only guess is that those who suggest OO principles don't apply
    - Either only worked in academia.
    - Or have only built functional programs of a _relatively_ small size. FP removes the possibility for so many errors that engineers can afford to _defer_ enterprise
    engineering practices much longer than in imperative languages, but not indefinitely. 50K lines of a functional codebase without SOLID and encapsulation is likely 🍝 spaghetti.
    - Or have only worked on functional programs that are exceedingly logic heavy, with relatively few data models, and a UX with few interaction _methods_ such as videogames, simulations,
    or compilers.
    - Or moved to FP to _escape_ from the perceived faults of OO where those faults are often more about their ignorance of OO than OO itself. I have experience of
    several prominent / influential engineers highly critical of OO with _NEAR ZERO_ knowledge of Domain Driven Design, Hexagonal Architecture,
    the writings of Martin Fowler and/or Robert Martin, and Design Patterns. These highly intelligent and charismatic individuals have only novice to intermediate level knowledge
    of OO, or a view of OO from 1995, and yet speak very publicly about OO's faults as if experts on the subject.
