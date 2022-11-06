# asdf-dependency-graph

A minimal wrapper around `dot` available at least on Linux systems to generate dependency-graphs.

```lisp
CL-USER> (asdf-dependency-graph:generate "asdf-dependency-graph.png" "asdf-dependency-graph")
NIL
NIL
0
```

![asdf-dependency-graph.png](./asdf-dependency-graph.png?raw=true)

(Caveat to the above graph: external dependency of `dot`.)

```lisp
CL-USER> (let ((asdf-dependency-graph:*interesting-systems*
                 '("extensible-compound-types"
                   "polymorphic-functions"
                   "cl-form-types"
                   "ctype"
                   "polymorph.maths")))
           (asdf-dependency-graph:generate "maths.png" "polymorph.maths"))
NIL
NIL
0
```

![maths.png](./maths.png?raw=true)

## Other Solutions

- [asdf-viz](https://github.com/guicho271828/asdf-viz): It was faster/easier to start from scratch and do what I want to do, than dig into `asdf-viz` to figure out how to make it do what I want to do. Lisp Curse :/.
- [asd-graph](https://github.com/ioannad/asd-graph): (i) I couldn't load it. (ii) The approach of parsing an asd file instead of relying on the introspection facilities felt weird.


