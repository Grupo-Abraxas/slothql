package com.arkondata.slothql

/*
 ¿¡ Lens !?


  Scala
  =====
  Object = Type
         = Atomic(type)
         ∨ Product(name, fields)
         ∨ Coproduct(name, alternatives)
         ∨ Optional(Type)
  Arrow  = (Product: A .> Type: B) @@ field
         ∨ (Product: A .> Atomic: B) @@ field ## prop
         ∨ (A ^> B)
         ∨ (Optional A %> Optional B)
         // TODO: collect / unwind ???


  Graph
  =====
  Object = Node(:labels)
         ∨ Prop(::type)
         ∨ Rel(:type)
  Arrow  = ((Node:A) -[Rel:B]->)
         ∨ (-[Rel:A]-> (Node:B))
         ∨ ((Node:A) .> (Prop::B))
         ∨ ([Rel:A] .> (Prop::B))
         ∨ ((Node:A) #> (Node:A:B))

  +--------------------------------+-------------------------------+
  |              Scala             |             Graph             |
  +================================+===============================+
  |         Product(name)          |         Node(:labels)         |
  +--------------------------------+-------------------------------+
  |        Coproduct(name)         |         Node(:labels)         |
  +--------------------------------+-------------------------------+
  |         Atomic(type)           |         Prop(::type)          |
  +--------------------------------+-------------------------------+
  |       Optional: Product        |         OPTIONAL MATCH        |
  +--------------------------------+-------------------------------+
  |      Optional: Coproduct       |         OPTIONAL MATCH        |
  +--------------------------------+-------------------------------+
  |       Optional: Atomic         |         (IS) NOT NULL         |
  +--------------------------------+-------------------------------+
  |         (.>) @@ field          |     Rel(:type) / Prop.field   |
  +--------------------------------+-------------------------------+
  |               -                |      (Node:A) -[Rel:B]->      |
  +--------------------------------+-------------------------------+
  |               -                |      -[Rel:A]-> (Node:B)      |
  +--------------------------------+-------------------------------+
  |  (Product: A) .> (Atomic: B)   |     (Node:A) .> (Prop::B)     |
  |               @@ field         |                               |
  +--------------------------------+-------------------------------+
  |               -                |      [Rel:A] .> (Prop::B)     |
  +--------------------------------+-------------------------------+
  | (Coproduct: A) ^> (Product: B) |          (Node:A:B)           |
  +================================+===============================+
  |  (Product: A) .> (Product: B)  | (Node:A) -[:field]-> (Node:B) |
  |               @@ field         | = (-[Rel:field]-> (Node:B))   |
  |                                | ∘ ((Node:A) -[Rel:field]->)   |
  +--------------------------------+-------------------------------+
  |  (Product: A) .> (Atomic: B)   | (Node:A) -[:field{prop}]->    |
  |               @@ field         | = ([Rel:field] .> (Prop::B))  |
  |               ## prop          | ∘ ((Node:A) -[Rel:field]->)   |
  +================================+===============================+

  Coproduct: restricted to Product alternatives:
    + Implemented by matching labels
    - Select between Atomic and Product => between Prop & Rel
    - Atomic Coproduct is complicated
    - no need to handle nested Coproducts

 */
package object mapper {

}
