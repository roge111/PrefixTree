module UnitTests

open NUnit.Framework
open FsUnit
open Trie

// Unit тесты для структуры Trie

[<TestFixture>]
type TrieTests () =

    // Тест 1: Проверка вставки слова в пустое дерево
    [<Test>]
    member this.``Insert word into empty Trie should update Trie``() =
        let trie: Trie = Trie.Empty  // Явно указываем тип Trie
        let word = "hello"
        let updatedTrie: Trie = trie.Insert word  // Явно указываем тип обновленного Trie
        
        // Проверяем, что дерево не пустое после вставки
        updatedTrie.Root.Children |> should not' (be Empty)
        updatedTrie.Root.Children |> should contain ('h')

    // Тест 2: Проверка удаления слова из дерева
    [<Test>]
    member this.``Remove word from Trie should remove the word``() =
        let trie: Trie = Trie.Empty
        let word = "hello"
        let updatedTrie: Trie = trie.Insert word
        let removedTrie: Trie = updatedTrie.Remove word
        
        // Проверяем, что дерево пустое после удаления слова
        removedTrie.Root.Children |> should be Empty

    // Тест 3: Проверка фильтрации по условию
    [<Test>]
    member this.``Filter Trie by predicate should return filtered Trie``() =
        let trie: Trie = Trie.Empty
        let words = ["apple"; "banana"; "cherry"]
        
        let updatedTrie: Trie = words |> List.fold (fun t word -> t.Insert word) trie
        let filteredTrie: Trie = updatedTrie.Filter (fun word -> word.StartsWith "b")
        
        // Проверяем, что только слова, начинающиеся с "b", остались в дереве
        filteredTrie.Root.Children |> should contain ('b')
        filteredTrie.Root.Children |> should not' (contain 'a')

    // Тест 4: Проверка слияния двух деревьев
    [<Test>]
    member this.``Merge two tries should combine their nodes``() =
        let trie1: Trie = Trie.Empty.Insert "apple"
        let trie2: Trie = Trie.Empty.Insert "banana"
        
        let mergedTrie: Trie = Trie.Merge trie1 trie2
        
        // Проверяем, что оба слова присутствуют в объединённом дереве
        mergedTrie.Root.Children |> should contain ('a')
        mergedTrie.Root.Children |> should contain ('b')

    // Тест 5: Проверка левой свертки (foldLeft)
    [<Test>]
    member this.``FoldLeft should accumulate correctly``() =
        let trie: Trie = Trie.Empty
        let words = ["a"; "b"; "c"]
        
        let updatedTrie: Trie = words |> List.fold (fun (t: Trie) word -> t.Insert word) trie
        let result = updatedTrie.FoldLeft (fun acc word -> acc + word) ""
        
        // Проверяем, что свёртка даёт правильный результат
        result |> should equal "abc"

    // Тест 6: Проверка правой свертки (foldRight)
    [<Test>]
    member this.``FoldRight should accumulate correctly``() =
        let trie: Trie = Trie.Empty
        let words = ["a"; "b"; "c"]
        
        let updatedTrie: Trie = words |> List.fold (fun (t: Trie) word -> t.Insert word) trie
        let result = updatedTrie.FoldRight (fun word acc -> word + acc) ""
        
        // Проверяем, что свёртка даёт правильный результат
        result |> should equal "cba"
