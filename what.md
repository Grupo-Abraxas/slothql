# What we have

- ADT to graph mapping
    - Represent case classes as graphs
        - classes marked as `DecomposableEntity` are **Nodes** - independent entities/sub-graphs
        - other classes are treated as sub-documents and _belong_ to parent node
        - depending on type, class fields may become
            - vertex properties (_primitives_)
            - separate vertices (_sub-documents_)
            - edges connecting to _related node_ vertices
        - `Seq` and `Map` of _storable_ values are represented as a vertex with indices/keys as properties names
        - `Seq` of _sub-documents_ or _related nodes_ is represented as
        ``` (owner vertex) -field-> ('Seq') -index-> (sub-document/related vertex) ```
    - Traits and classes can be annotated with `decomposable base`, thus adding their type names to the labels
        of any case class that inherits them
    - `field: Map[Entity, Label]` can be annotated with `labelled(set_name)`, that represents the map as
    ``` (owner vertex) -field-> (set_name) -Label-> (Entity vertex) ```  
- CRUD operations
    - Uses gremlin
    - Operations:
        - Create
        - Get
        - Find
        - Exists
        - Query (readonly)
        - Update
        - Delete
- Selectors for update
- (Selectors for queries)
- Readonly queries with gremlin


# What we want
SlothQL API

## Utils

- [ ] Query typeclass
    -[ ] Describes a computation to be executed in the database
    -[ ] Functor, Applicative, Monad (?)
    -[ ] Can be prepared for execution by building a `cats.effect.IO` 

## High level
      
- [ ] Sangria (GraphQL) integration

#### CRUD operations on ADT

- [ ] **CRUD DSL** - Static
    - [ ] _Create_ nodes
        - [ ] Returns new node's `id`
        - [ ] Creation descriptor
            - [ ] From object instances
            - [ ] **¿** How to use `id`s instead of _related_ instances **?**
    - [ ] _Read_ selected fields and relations
        - [ ] Returns selection
        - [ ] Syntax extension to chain read after other CRUD operations
        - [ ] Selectors: `shapeless` optics extended to preserve selection path
        - [ ] (_?_) graphql interpolation & interpretation
        - [ ] **Operations:**
            - [ ] _Get_ node by id
            - [ ] _Find_ nodes
                - [ ] Filter
                - [ ] Order
                - [ ] Paginate
            - [ ] _Query_
                - [ ] With SlothQL
    - [ ] _Update_ nodes
        - [ ] Returns unit
        - [ ] Partial updates based on selectors (just like in _Nous_)
        - [ ] (?) Does not affect related nodes
    - [ ] _Delete_
        - [ ] Returns unit
        - [ ] By `label` and `id`
        - [ ] Deletes sub-documents
        - [ ] Does not affect related nodes
        - [ ] (?) Preserves base consistency

- [ ] **CRUD DSL** - Dynamic
    - [ ] ?
    
- [ ] Validations
    - [ ] For each operation
    - [ ] Validate access to entire node
    - [ ] Validate access to specific node fields

- [ ] **¿** Transaction locks **?**

## Mapper level

- [ ] CRUD Interpreter

#### ADT to graph mapping

- [ ] Schema
    - [ ] **!** Define capabilities/restraints
    - [ ] Static
        - [ ] Derived (auto)
    - [ ] Dynamic
    - [ ] Conversions
        - [ ] Scala types/objects → sub-graph
            - [ ] Allow to substitute related class instances with `label + id`
        - [ ] Sub-graph → scala types/objects*
            - [ ] Generic partial case class representation_ (with `shapeless`)
        - [ ] Support
            - [ ] Case classes
            - [ ] (Sealed) Traits
        - [ ] **!** Setup
            - **?** Mark classes as nodes (as opposed to sub-documents)
            - **?** Mark classes with additional _labels_ to be used for vertices
            - **?** Class members can be marked to be included in the representation

## Low level

- [ ] Query Language
    - Highly modular features support and syntax
    - Syntax
        - Gremlin-like
        - Cypher-like

#### Low level - DB Support

- [ ] Neo4j support

### What else?
