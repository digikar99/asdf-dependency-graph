# asdf-dependency-graph

A minimal wrapper around `dot` available at least on Linux systems to generate dependency-graphs.

![asdf-dependency-graph.png](./asdf-dependency-graph.png?raw=true)

(Caveat to the above graph: external dependency of `dot`.)

## Other Solutions

- [asdf-viz](https://github.com/guicho271828/asdf-viz): It was faster/easier to start from scratch and do what I want to do, than dig into `asdf-viz` to figure out how to make it do what I want to do. Lisp Curse :/.
- [asd-graph](https://github.com/ioannad/asd-graph): (i) I couldn't load it. (ii) The approach of parsing an asd file instead of relying on the introspection facilities felt weird.


