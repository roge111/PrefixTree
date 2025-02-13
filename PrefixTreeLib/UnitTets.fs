open Xunit
//Желатедльно написать еще тесты
[<Fact>]
let ``Insert should add elements`` () =
    let trie = insert ['a'; 'b'; 'c'] empty
    let result = foldLeft (fun acc x -> acc @ [x]) [] trie
    Assert.Equal(['a'; 'b'; 'c'], result)

[<Fact>]
let ``Remove should delete elements`` () =
    let trie = insert ['a'; 'b'; 'c'] empty
    let trie' = remove ['a'; 'b'; 'c'] trie
    Assert.Equal(empty, trie')