package com.abraxas.slothql.mapper

import shapeless._


trait Schema[A] {
  val manifest: Manifest[A]

  type Repr <: GraphRepr
  val repr: Repr
}

object Schema {
  type Aux[A, G <: GraphRepr] = Schema[A] { type Repr = G }
  def apply[A](implicit cached: Cached[Schema[A]]): Aux[A, cached.value.Repr] = cached.value

  def defineFor[A]: Impl.Builder[A] = Impl.Builder.asInstanceOf[Impl.Builder[A]]

  protected object Impl {
    class Builder[A] {
      def apply[G <: GraphRepr](g: G)(implicit m: Manifest[A]): Schema.Aux[A, G] =
        new Schema[A] {
          val manifest: Manifest[A] = m
          type Repr = G
          val repr: G = g
        }
    }
    private[Schema] object Builder extends Builder[Any]
  }
}



sealed trait GraphRepr
object GraphRepr {
  sealed trait Property extends GraphRepr {
    type Type
    val manifest: Manifest[_]

    // TODO: override toString
  }

  sealed trait Element extends GraphRepr

  trait Node extends Element {
    type Labels   <: HList
    type Fields   <: HList
    type Incoming <: HList
    type Outgoing <: HList

    val labels: List[String]
    val fields: Map[String, Property]
    val incoming: Map[String, Either[Ref[_], Relation]]
    val outgoing: Map[String, Either[Ref[_], Relation]]

    // TODO: override toString
  }

  trait Relation extends Element {
    type Type   <: String
    type Fields <: HList
    type From   <: Node
    type To     <: Node

    val tpe: String
    val fields: Map[String, Property]
    val from: Ref.To[Node]
    val to: Ref.To[Node]

    // TODO: override toString
  }

  trait Identifiable {
    repr: Element =>

    type IdField <: String
    val idField: String

    // TODO: override toString
  }

  sealed trait Ref[A] extends Element with Identifiable {
    type E <: Element with Identifiable
    val schema: Schema.Aux[A, E]
  }
  

  object Ref {
    type Aux[A, E0 <: Element with Identifiable] = Ref[A] { type E = E0 }
    type To[E0 <: Element] = Ref[_] { type E <: E0 with Identifiable }
    def apply[A]: Builder[A] = Builder.asInstanceOf[Builder[A]]

    protected class Builder[A] {
      def apply[E0 <: Element with Identifiable]()(implicit s: Cached[Schema.Aux[A, E0]]): Aux[A, E0] =
        new Ref[A] {
          type E = E0
          lazy val schema: Schema.Aux[A, E0] = s.value
          type IdField = schema.repr.IdField
          val idField: String = schema.repr.idField
        }
    }
    private object Builder extends Builder[Any]
  }


  object Property {
    type Aux[T] = Property{ type Type = T }
    def apply[T: Manifest]: Property.Aux[T] = new Property {
      type Type = T
      val manifest: Manifest[_] = implicitly[Manifest[T]]
    }
    def unapply(repr: GraphRepr): Option[Manifest[_]] = PartialFunction.condOpt(repr) {
      case prop: Property => prop.manifest
    }
  }
  
  object Node {
    type Aux[Labels0 <: HList, Fields0 <: HList, Incoming0 <: HList, Outgoing0 <: HList] =
      Node { type Labels = Labels0; type Fields = Fields0; type Incoming = Incoming0; type Outgoing = Outgoing0 }
    def unapply(repr: GraphRepr): Option[(List[String], Map[String, Property], Map[String, Either[Ref[_], Relation]], Map[String, Either[Ref[_], Relation]])] =
      PartialFunction.condOpt(repr) { case node: Node => (node.labels, node.fields, node.incoming, node.outgoing) }
  }

  object Relation {
    type Aux[Type0 <: String, Fields0 <: HList, From0 <: Node, To0 <: Node] =
      Node { type Type = Type0; type Fields = Fields0; type From = From0; type To = To0 }
    def unapply(repr: GraphRepr): Option[(String, Map[String, Property], Ref.To[Node], Ref.To[Node])] =
      PartialFunction.condOpt(repr) { case rel: Relation => (rel.tpe, rel.fields, rel.from, rel.to) }
  }
  
  object Identifiable {
    type Aux[Id <: String] = Identifiable { type IdField = Id }
    def unapply(repr: GraphRepr): Option[(String, GraphRepr)] = PartialFunction.condOpt(repr) {
      case ident: Identifiable => ident.idField -> repr
    }
  }
}

