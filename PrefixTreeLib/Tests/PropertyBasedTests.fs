module PropertyBasedTests

open FsCheck
open NUnit.Framework
open Trie
open System

[<TestFixture>]
type property () = 

    
    let generateRandomWord () =
        let rand = Random()
        let length = rand.Next(3, 10)
        let chars = [| for _ in 1..length -> char (rand.Next(int 'a', int 'z' + 1)) |]
        new String(chars)

    // Свойство 1: Проверка нейтрального элемента
    
    member this.``Neutral element property`` (word: string) =
        let trie = Trie.Empty.Insert word
        let mergedWithEmpty1 = Trie.Merge trie Trie.Empty
        let mergedWithEmpty2 = Trie.Merge Trie.Empty trie
        mergedWithEmpty1.Root.Children = trie.Root.Children && mergedWithEmpty2.Root.Children = trie.Root.Children


    // Свойство 2: Ассоциативность
   
    member this.``Associative merge property`` (word1: string) (word2: string) (word3: string) =
        let trie1 = Trie.Empty.Insert word1
        let trie2 = Trie.Empty.Insert word2
        let trie3 = Trie.Empty.Insert word3
        let merged1 = Trie.Merge (Trie.Merge trie1 trie2) trie3
        let merged2 = Trie.Merge trie1 (Trie.Merge trie2 trie3)
        merged1.Root.Children = merged2.Root.Children

    // Свойство 3: Идентичность операции
    
    member this.``Identity operation property`` (word: string) =
        let trie = Trie.Empty
        let insertedTrie = trie.Insert word
        let removedTrie = insertedTrie.Remove word
        let reInsertedTrie = removedTrie.Insert word
        reInsertedTrie.Root.Children = insertedTrie.Root.Children

    // Запуск тестов
    [<Test>]
    member this.``Run Property Tests``() =
        let testCases = [("hello", "world", "test")]
        testCases |> List.iter (fun (word1, word2, word3) ->
            printfn "Running tests for words: %s, %s, %s" word1 word2 word3
            Check.Quick (fun word -> this.``Neutral element property`` word)
            Check.Quick (fun word1 word2 word3 -> this.``Associative merge property`` word1 word2 word3)
            Check.Quick (fun word -> this.``Identity operation property`` word)
        )
