# fp-overview

A quick tour through the merits of pure statically typed "fancy" functional languages.

I haven't seen many good sales pitches.

- Some FP advocates talk about abstraction and then fail to show practical uses of abstraction. Instead they talk about the mechanics or the theory without showing the
benefits of practical application. Put succinctly, functional abstractions simplify and normalize Api's and provide a provably correct (law abiding) foundation
for Api's and application types and behaviors. The `Abstractions` folder in this repository starts with two examples of parlour tricks to show off a "Wow!" factor
and pique interest but then the [E03Traversable](./src/Abstraction/E03Traversable.purs) module gives a glimpse into the ability of abstractions to simplify Api's.
- Some pontificate on FP in a way that suggests all or most of the tools of software quality from the OO community no longer apply.
  - This is intensely misguided. My only guess is that those who suggest this have
    - Either only worked in academia.
    - Or have only built functional programs of a _relatively_ small size. FP removes the possibility for so many errors that engineers can afford to defer good engineering practices much longer than in imperative languages. BUT not indefinitely! 50K lines of a functional codebase without applying SOLID and encapsulating is spaghetti.
    - Or have only worked on functional programs that are exceedingly logic heavy and data modeling light.
    - Or moved to FP to _escape_ from the perceived faults of OO where those faults had more to do with their ignorance of OO than OO itself. That is harsh but I have experience of
    several prominent / influential engineers highly critical of OO with _ZERO_ knowledge of Domain Driven Design, Hexagonal Architecture,
    the writings of Martin Fowler, the writings of Robert Martin, and little or immature understanding of Design Patterns. In other words, they had novice to intermediate level
    knowledge of OO or a view of OO from 1995 and yet speak, very publicly, about OO's faults as if experts on the subject. In truth, the engineers I am describing are all
    extremely intelligent (and often charismatic) and many who listen to them are excited to be free of having to learn good ol'fashioned boring engineering and data modeling practices.
    Their followers are excited to be part of the FP club as if it were completely distinct from the rest of enterprise application development. The truth is that if
    you are building a large enterprise application with many data models and a large team you are going to be using a LOT of the same engineering principles in FP as in OO. It is
    just that the FP languages provide distinctly more elegant and precise tools for the realization of those concepts.
  - SOLID, encapsulation, and abstraction are every bit as applicable in FP as in OO. Furthermore, the notion that behavior
  should be grouped with types exists in Haskell, PureScript, OCaml, and F# communities (to name a few). The only difference is that in FP the behavior is
  not strictly bound to its type. Other modules can extend the behavior of a type, but the .NET clearly appreciates the merits of extrinsic behavior or they
  wouldn't have introduced Extension modules. Behavior can be grouped with types in Haskell, PureScript, Elm, OCaml, F#, or any other functional language and this can provide
  all of the **COHERENCE** and discoverability one would expect of an OO language. The difference is that the smallest unit of composition in a functional language
  is a function while in OO languages the smallest unit of composition tends to be an entire class (Decorator Pattern, Adapter Pattern). As such, FP languages
  feature greater composition precision and that means decreased **COUPLING**.
    - Unfortunately, since the smallest unit in an FP language is a function it is easier for undisciplined engineers to create spaghetti in an FP language than in an OO language
    because the OO language forces the engineer to group functions with types. OO is like data modeling training wheels. It is helpful for novice and intermediate
    engineers.
