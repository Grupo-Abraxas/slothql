CREATE  (b:Book{ title: "History of Rome" }),
        (a:Author{ name: "Theodor Mommsen" }),
        (p1:Page{ text: "The Mediterranean Sea with its various branches, penetrating far into the great Continent, forms the largest gulf of the ocean, and, alternately narrowed by islands or projections of the land and expanding to considerable breadth, at once separates and connects the three divisions of the Old World." }),
        (p2:Page{ text: "We have no information, not even a tradition, concerning the first migration of the human race into Italy. It was the universal belief of antiquity that in Italy, as well as elsewhere, the first population had sprung from the soil." }),
        (m:Meta{ isbn: "9786610240531" }),
        (b) -[:author]-> (a),
        (b) -[:meta]-> (m),
        (b) -[:pages{ index: 1 }] -> (p1),
        (b) -[:pages{ index: 2 }] -> (p2);

CREATE  (b:Book{ title: "Homotopy Type Theory" }),
        (p:Page{ text: "Homotopy type theory is a new branch of mathematics that combines aspects of several different fields in a surprising way." }),
        (m:Meta{ isbn: "" }),
        (b) -[:meta]-> (m),
        (b) -[:pages{ index: 1 }] -> (p);