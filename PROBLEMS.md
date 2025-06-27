# Problems

This document reviews what I perceive to be the problems with adopting functiona languages in 2025.

## Elm

Elm is an incredible little language with a lot to offer. However,

- There is no synchronous request/response JS Interop and this creates project risk if the behavior you need is implemented only in JS and that behavior needs to
be integrated _into_ your Elm code.
  - JS Interop is done by
    - Sending and listening for messages to and from the outside world.
    - Or by using Service Workers over JavaScript libraries to offer an HTTP GET style Api.
    - Or by using Web Components if the goal is to use components authored in a different application in your Elm application.
  - The Elm ecosystem has a lot of incredibly high quality libraries and there are enterprise companies that have been building
  software with Elm for eight years or more such that it is likely most vanilla needs can be met by an existing Elm library.
- The author of the language Evan has gone incredibly silent on Elm and there have been no real updates since 2019.
  - He has given some private talks on a new "database language" but few outside of an inner circle can confirm that has anything to do with Elm and it seems
  like it does not.
  - The community has been actively building new tools such as the language server, Lamdera, Elm Land, Elm Pages, etc.. There is sufficient talent in the
  community to take over should Evan ever officially resign from his role, but the community has too much respect for the creator to do so against
  his will so the language is held hostage by its creator.
- The official Elm model is client only. That said, Lamdera, Elm Pages, and other projects provide a backend Elm solution. The problem is that Lamdera is a small-ish company
  which creates risk and some of the other options are not quite as battle tested.
- Ironically, testing effects is hard in Elm. The standard update function returns a new `Model` and a `Cmd` where the command can be ANY command or effect. That means
that the standard model is far less testable than Redux Sagas.
  - Using the "Effect" pattern can mitigate this issue but it introduces even more boilerplate code (and Elm already has a bit much of that).
  - There are also Unit Test tools that support testing effects but they are a bit tedious and not terribly compiler enforced.

## F Sharp

There is a relatively modest business case for F#. It offers ergonomic improvements for functional style, proper sum types, and a couple of really neat features of its own
which is great BUT

- It can provide little in the way of added _safety_ over C# because F#
  - Still uses the **imperative** .NET framework which includes lots of null holes.
  - Offers mutability so that there are few guarantees about what code will mutate things.
- C# keeps adding more FP constructs all the time such that there are diminished returns.

Given a value proposition that is shrinking rather than growing, Microsofts history of shelving lab projects, and the shifting economics of this industry (hello AI!),
I wouldn't bet on F# being around for longer than 6 years. It might, but it might not.

## PureScript

PureScript is an amazing language but there are some problems which can be divided into a few different categories.

### Technical

These are issues that could, theoretically, be addressed with better technology (probably from increased funding).

- The compiler is really slow. I would guesstimate 3 - 5x slower than TypeScript and TypeScript is going to get 10x faster when the Go re-write is complete.
- The error messages are horrific because
  - You not only have structural typing (like TypeScript)
  - but you also have higher kindedness with infinite levels of type abstraction
  - and you have the Hindley-Milner inference which unifies bottom-up with top-down. The result of this is that if engineers do not provide type annotations
  then errors can be reported far from the actual source of the error (kinda like old school C++ compilers).
- The tooling is extremely sparse.
  - There is no RENAME symbol feature in the IDE so it is back to basic text search/replace.
  - I wrote the only Linter with any significant number of rules.
  - And on and on.

### Circumstantial Issues

- PureScript is literally 1000x less popular than TypeScript so
  - AI models are useless.
  - Good luck finding answers on Stack Overlfow or anything like that.
- Many libraries are terribly documented. When compared against the Elm community's documentation it is a pretty stark difference.

### Essential Issues

- PureScript features ad hoc polymorphism which is great when EITHER there is only one data type supporting an operation in a given local context OR engineers are careful to code for readability.
For example, the `map` function will work on an `Aff` (basically a promise), or an `Either`, or a `Maybe`, or `Parser`, or an `Array`, or a TON of things. Given an `Aff (Either String (Array Person))` one could write `map $ map $ map personToWidget` to convert that to `Aff (Either String (Array Widget))`. Notice the 3 maps there? Well there's tons of overload functions like that and if developers do not take care then code can only be understood by evaluating it in your mind and holding all relevant **context**. In other words, the code is highly contextual / not terribly readable / REGEX-like.
  - The language recently added a feature called Visible Type Applications allows engineers to explicitly specify types `map @Aff $ map @(Either String) $ map @Array personToWidget` which provides cues and allows a context-free reading.
  HOWEVER, 1. Not all functions in the standard library have been written to make use of this feature. 2. They do not work with symbolic operators such as `<$>` (which are preferred by Haskellers/PureScripters). 3. A lot of engineers don't want to bother with the verbosity.
  - Developers can also break up code into smaller named functions with type annotations but that can get extremely verbose.
  - Developers can also provide inline type annotations which are less verbose than entire functions but still not great.
- There are 16 symbolic operators that are very common (listed below) and then some libraries like JSON and Parser libraries love to add their own. These not only contribute to a code-y look but, as mentioned above, most
of the operators are abstract or heavily overloaded and do not support Visible Type Applications which means that it can be difficult to know what concrete thing is being operated upon.
  - With both a forward and backwards variation or left/right
    - `#` / `$` application of an argument to a function
    - `>>>` / `<<<` function composition
    - `<$>` / `<#>` functor mapping
    - `>>=` / `=<<` monadic binding
    - `>=>` / `<=<` Kleisi composition
    - Not even going to show the operators for Comonad!!! And yes, Comonad is a thing.
  - also
    - `>$<` contravariant mapping
    - `<>` append
    - `<*>` apply, `<*` apply first, and `*>` apply second
    - `<|>` for alt
- The prior two points can be allievated with good style. However, there is another, more fundamental problem: Almost any PureScript code can be made more abstract and the possibility of that
abstraction leads engineers to spend four times as long writing code in half of the circumstances leading to double the implementation time. If the abstraction is at the level of a function
signature it can lead to improved re-use but abstraction in implementations rarely even pays off with net positives because bug reductions through proper abstraction are nearly cancelled out
by reading costs later on. All told, I would estimate that it takes an intelligent and experienced PureScript engineer 50% longer to implement functionality over a 3 month period and perhaps
20% longer if considered over a two year period. It is close to as fast, but it is NOT as fast. The only way to move faster is to resist the urge to abstract function _implementations_ BUT
  - Often abstract implementations DO show the possibility of abstract signatures and so getting in a concrete mindset for implementation can backfire by leading to concrete signatures.
  - I have not seen any FP engineers capable of abstraction that could resist the urge to abstract. Worse, intelligent junior and mid engineers will spend many more times longer writing
  functionality than seniors with a much more dramatic decrease in output. Additionally, by focusing on low-level abstraction and math-y-ness, engineers often miss obvious high ROI opportunities to simplify
  their data modeling. I once saw a job candidate submit code using Comonads and tons of crazy type acrobatics but they made dozens of incredibly stupid modeling mistakes. Just horrific. It blew
  everyone's mind that someone could know how to use Comonads and still screw up the easy stuff that badly, but that is actually really common in these languages and is part of selection bias:
  Fancy FP languages tend to attract genius burnouts. Genius enough to write really incredible stuff, but burned out enough that they cannot be bothered to do any of the "simple" or "easy" things.
