# DAML-LF Research Interpreter Prototype

**Author:** [Martin Huschenbett](email:martin.huschenbett@digitalasset.com)
**Co-Author:** [Jussi Maki](email:jussi.maki@digitalasset.com)
**Audience:** Members of Language and Platform Engineering


## Why is this?

As it stands, the spider code base is starting to push DAML Engine to its
limits during development in DAML Studio in terms of scenario execution times.
Thus we started discussing and investigating ways to improve its performance.
Unfortunately, even super tricks like using de Bruijn indices did not show
any effect within the existing DAML-LF interpreter. That is why I decided
to spend a bit of my spare time and explore another way of writing a DAML-LF
interpreter from the ground up. This prototype is the result of my exploration.

Since I finished the first version of this protoype, we have rewritten it in
Scala and started to tweak the hell out of it. The result is a **6x speedup**
over the current DAML Engine.


## What is this exactly?

This a prototype implementation of an interpreter for the pure fragement of
DAML-LF. Like the current interpreter it is based on a CEK machine. Unlike the
current interpreter it is written in an imperative style using mutable data
structures in various places. Unlike the current interpreter it is using
de Bruijn indices for local names. Unlike the current interpreter it is
written in Rust.


## Why Rust?

Writing an efficient CEK machine is a tricky thing. You have to deal with
a lot of objects in memory and want to avoid copying them as much as possible
but at the same time make sure you copy enough of them. Otherwise, you start
overwriting memory locations which are shared in DAML-LF's memory model.
Rust's ownership system is a great formalism to reason about when you need to
copy and when you can avoid it. The borrow checker of the Rust compiler is an
incredibly powerful static analysis tool that mechanically enforces the rules
of ownership and supports the me as developer in following them.

These are exactly the reasons why I chose to write the
[RuCEKi](https://github.com/hurryabit/ruceki) interpreter for my personal toy
programming language [Pukeko](https://github.com/hurryabit/pukeko) in Rust.
as well. In fact, I heavily piggybacked on the RuCEKi implementation and the
knowledge I gained from it.


## What next?

We are now focusing on leveraging the lessons we have learnt during the
implementation of this prototype and its Scala rewrite for DAML Engine as much
as possible. The Rust prototype will end up on the shelf and probably be
consulted from time to time when we need to figure out another memory
management detail for the Scala implementation of the DAML-LF interpreter.
