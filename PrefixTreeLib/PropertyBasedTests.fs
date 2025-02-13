open FsCheck

[<Property>]
let ``Combine is associative`` (t1: Trie<int>, t2: Trie<int>, t3: Trie<int>) =
    combine (combine t1 t2) t3 = combine t1 (combine t2 t3)

[<Property>]
let ``Combine with empty is identity`` (t: Trie<int>) =
    combine t empty = t && combine empty t = t

[<Property>]
let ``Insert and remove are inverse`` (elements: int list) =
    let trie = insert elements empty
    let trie' = remove elements trie
    trie' = empty