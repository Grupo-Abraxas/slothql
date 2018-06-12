package com.abraxas.slothql.test.models

import shapeless._

import com.abraxas.slothql.mapper.GraphRepr.Ref
import com.abraxas.slothql.mapper.{ GraphRepr, Schema }

case class Group(id: Option[String], members: Map[User, User.Role])

case class User(id: Option[String], name: String)

object User {
  sealed trait Role
  object Role {
    case object Admin extends Role
    case object User extends Role

    implicit lazy val schema = Schema.defineFor[Role](GraphRepr.Property[String])
  }

  object UserRepr extends GraphRepr.Node with GraphRepr.Identifiable {
    type Labels = Witness.`"User"`.T :: HNil
    type Fields = Witness.`"id"`.Field[GraphRepr.Property.Aux[Option[String]]] ::
      Witness.`"name"`.Field[GraphRepr.Property.Aux[String]] :: HNil
    type Incoming = HNil
    type Outgoing = HNil
    type IdField = Witness.`"id"`.T
    val labels: List[String] = List("User")
    val fields: Map[String, GraphRepr.Property] = Map(
      "id"   -> GraphRepr.Property[Option[String]],
      "name" -> GraphRepr.Property[String]
    )
    val incoming: Map[String, Either[GraphRepr.Ref[_], GraphRepr.Relation]] = Map()
    val outgoing: Map[String, Either[GraphRepr.Ref[_], GraphRepr.Relation]] = Map()
    val idField: String = "id"
  }

  implicit lazy val schema: Schema.Aux[User, UserRepr.type] = Schema.defineFor[User](UserRepr)

}

object FooBar {
  type MembersST = Witness.`"members"`.T
}

object Group {

  implicit object GroupMembersRepr extends GraphRepr.Relation {
    type Type = FooBar.MembersST
    type Fields = HNil
    type From = GroupRepr.type
    type To = User.UserRepr.type
    val tpe: String = "members"
    val fields: Map[String, GraphRepr.Property] = Map()
    lazy val from: Ref.To[GraphRepr.Node] = Ref[Group]()
    lazy val to: Ref.To[GraphRepr.Node] = Ref[User]()
  }


  object GroupRepr extends GraphRepr.Node with GraphRepr.Identifiable {
    type Labels = Witness.`"Group"`.T :: HNil
    type Fields = Witness.`"id"`.Field[GraphRepr.Property.Aux[Option[String]]] :: HNil
    type Incoming = HNil
    type Outgoing = Witness.`"members"`.Field[GraphRepr.Ref[Map[User, User.Role]]] :: HNil
    type IdField = Witness.`"id"`.T
    val labels: List[String] = List("Group")
    val fields: Map[String, GraphRepr.Property] = Map("id" -> GraphRepr.Property[Option[String]])
    lazy val incoming: Map[String, Either[GraphRepr.Ref[_], GraphRepr.Relation]] = Map()
    lazy val outgoing: Map[String, Either[GraphRepr.Ref[_], GraphRepr.Relation]] = Map("members" -> Right(GroupMembersRepr))
    val idField: String = "id"
  }


  implicit lazy val groupSchema: Schema.Aux[Group, GroupRepr.type] =
    Schema.defineFor[Group](GroupRepr)
}
