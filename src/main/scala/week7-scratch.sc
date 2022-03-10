// Queries with for
// For is analogous to a query languages


val books: List[Book] = List(
    Book(title = "Title 1", authors = List("Last1, First1", "Last2, First2"))
)

for // Get titles of books written by author whose last name is Last1
    b <- books
    a <- b.authors
    if a.startsWith("Last1,")
yield b.title


// for translates using the methods map, flatMap, and withFilter
// as long as those methods are defined, can define for