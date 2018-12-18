package com.abraxas.slothql.test.models

/*

import shapeless._

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
    type Fields = Witness.`"id"`.  Field[GraphRepr.Property.Aux[Option[String]]] ::
                  Witness.`"name"`.Field[GraphRepr.Property.Aux[String]] :: HNil
    type Outgoing = HNil
    type IdField = Witness.`"id"`.T
    val labels: List[String] = List("User")
    val fields: Map[String, GraphRepr.Property] = Map(
      "id"   -> GraphRepr.Property[Option[String]],
      "name" -> GraphRepr.Property[String]
    )
    val outgoing: Map[String, GraphRepr.Relation] = Map()
    val idField: String = "id"
  }

  implicit lazy val schema: Schema.Aux[User, UserRepr.type] = Schema.defineFor[User](UserRepr)

}

object Group {

  implicit object GroupMembersRepr extends GraphRepr.Relation {
    type Type = Witness.`"members"`.T
    type Fields = HNil
    type From = GroupRepr.type
    type To = User.UserRepr.type
    val tpe: String = "members"
    val fields: Map[String, GraphRepr.Property] = Map()
    val from: GraphRepr.Node = GroupRepr
    val to: GraphRepr.Node = User.UserRepr
  }


  object GroupRepr extends GraphRepr.Node with GraphRepr.Identifiable {
    type Labels = Witness.`"Group"`.T :: HNil
    type Fields = Witness.`"id"`.Field[GraphRepr.Property.Aux[Option[String]]] :: HNil
    type Incoming = HNil
    type Outgoing = Witness.`"members"`.Field[GroupMembersRepr.type] :: HNil
    type IdField = Witness.`"id"`.T
    val labels: List[String] = List("Group")
    val fields: Map[String, GraphRepr.Property] = Map("id" -> GraphRepr.Property[Option[String]])
    val outgoing: Map[String, GraphRepr.Relation] =  Map("members" -> GroupMembersRepr)
    val idField: String = "id"
  }


  implicit lazy val groupMembersSchema: Schema.Aux[Map[User, User.Role], GroupMembersRepr.type] =
    Schema.defineFor[Map[User, User.Role]](GroupMembersRepr)
  implicit lazy val groupSchema: Schema.Aux[Group, GroupRepr.type] =
    Schema.defineFor[Group](GroupRepr)
}
*/
