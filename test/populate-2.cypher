
/*        (g2)
    +-------------+   members   +-----------+    Edit
    |  Sub-Group  |-----------▶ |  Members  |------------┐
    +-------------+             +-----------+            ▼
           |                                        +--------+
           | parent                                 |  User  | (u1)
           ▼                                        +--------+
    +-------------+             +-----------+            ▲
    |  Root Group |-----------▶ |  Members  |------------┘
    +-------------+   members   +-----------+    Admin
          (g1)                        |
                                      | Read
                                      ▼
                                  +--------+
                                  |  User  | (u2)
                                  +--------+
*/
CREATE (:User {id: "u1", email: "john@example.com", name: "John", age: 28, confirmed: true}) <-[:Admin]- (:Members) <-[:members]- (_:Group{id: "g1", name: "Root Group"});
MATCH (g:Group { id: "g1"}) CREATE (:Group { id: "g2", name: "Sub Group" }) -[:parent]-> (g);
MATCH (u: User { id: "u1" }),(g:Group { id: "g2"}) CREATE (u) <-[:Edit]- (:Members) <-[:members]- (g);
MATCH (:Group { id: "g1"}) -[:members]-> (m:Members) CREATE (:User {id: "u2", email: "jane@example.com", name: "Jane", age: 22, confirmed: false}) <-[:Read]- (m);