package com.abraxas.slothql.mapper

import shapeless._

import com.abraxas.slothql.util.ShowManifest


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
  // GraphRepr tree leaf
  sealed trait Property extends GraphRepr {
    type Type
    val manifest: Manifest[_]

    override def toString: String = s"Property[${ShowManifest(manifest)}]"
  }

  sealed trait Element extends GraphRepr {
    protected def toStringName: String
    protected def toStringFields: List[String]
    override def toString: String = toStringName + (if (toStringFields.nonEmpty) toStringFields.mkString("(", "; ", ")") else "")
  }

  trait Node extends Element {
    type Labels   <: HList
    type Fields   <: HList
    // type Incoming <: HList
    type Outgoing <: HList

    val Labels: Labels
    val Fields: Fields
    val Outgoing: Outgoing


    val labels: List[String]
    val fields: Map[String, Property]
    // val incoming: Map[String, Relation]
    val outgoing: Map[String, Relation]


    protected def toStringName: String = "Node"
    protected def toStringFields: List[String] = List(
      s"labels = ${labels.mkString(", ")}",
      s"fields = ${printMap(fields)}",
      s"outgoing = ${printMap(outgoing)}"
    )
  }

  trait Relation extends Element {
    type Type   <: String
    type Fields <: HList
    type From   <: Node
    type To     <: Node

    val Type: Type
    val Fields: Fields
    val From: From
    val To: To

    val tpe: String
    val fields: Map[String, Property]
    val from: Node
    val to: Node

    protected def toStringName: String = "Relation"
    protected def toStringFields: List[String] = List(
      s"type = $tpe",
      s"fields = ${printMap(fields)}",
      s"from = $from",
      s"to = $to"
    )
  }

  private def printMap(m: Map[_, _]) = m.map{ case (k, v) => s"$k -> $v" }.mkString(", ")

  trait Identifiable {
    repr: Element =>

    type Id
    type IdField <: String
    val idField: String

    override protected def toStringFields: List[String] = s"idField = $idField" +: repr.toStringFields
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
    type Aux[Labels0 <: HList, Fields0 <: HList, Outgoing0 <: HList] =
      Node { type Labels = Labels0; type Fields = Fields0; type Outgoing = Outgoing0 }
    def unapply(repr: GraphRepr): Option[(List[String], Map[String, Property], Map[String, Relation])] =
      PartialFunction.condOpt(repr) { case node: Node => (node.labels, node.fields, node.outgoing) }
  }

  object Relation {
    type Aux[Type0 <: String, Fields0 <: HList, From0 <: Node, To0 <: Node] =
      Relation { type Type = Type0; type Fields = Fields0; type From = From0; type To = To0 }
    def unapply(repr: GraphRepr): Option[(String, Map[String, Property], Node, Node)] =
      PartialFunction.condOpt(repr) { case rel: Relation => (rel.tpe, rel.fields, rel.from, rel.to) }
  }
  
  object Identifiable {
    type Aux[Id0] = Identifiable { type Id = Id0 }
    def unapply(repr: GraphRepr): Option[(String, GraphRepr)] = PartialFunction.condOpt(repr) {
      case ident: Identifiable => ident.idField -> repr
    }
  }
}

