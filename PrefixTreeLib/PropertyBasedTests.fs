module PropertyBasedTests

open FsCheck
open NUnit.Framework
open Trie
open System

let generateRandomWord () =
    let rand = Random()
    let length = rand.Next(3, 10) // Длина слова от 3 до 10 букв
    let chars = [| for _ in 1..length -> char (rand.Next(int 'a', int 'z' + 1)) |]
    new String(chars)

// Свойство 1: Проверка нейтрального элемента
// Если мы сливаем дерево с пустым деревом, оно должно остаться без изменений
[<Test>]
let prop_neutralElement (word: string) =
    let trie = Trie.Empty.Insert word
    let mergedWithEmpty1 = Trie.Merge trie Trie.Empty
    let mergedWithEmpty2 = Trie.Merge Trie.Empty trie
    mergedWithEmpty1.Root.Children = trie.Root.Children && mergedWithEmpty2.Root.Children = trie.Root.Children  // Проверяем, что слияние с пустым деревом не изменяет исходное дерево

// Свойство 2: Ассоциативность
// Слияние двух деревьев слиянием третьего дерева должно быть ассоциативным
[<Test>]
let prop_associativeMerge (word1: string) (word2: string) (word3: string) =
    let trie1 = Trie.Empty.Insert word1
    let trie2 = Trie.Empty.Insert word2
    let trie3 = Trie.Empty.Insert word3
    
    let merged1 = Trie.Merge (Trie.Merge trie1 trie2) trie3
    let merged2 = Trie.Merge trie1 (Trie.Merge trie2 trie3)
    
    merged1.Root.Children = merged2.Root.Children  // Проверка ассоциативности слияния

// Свойство 3: Идентичность операции
// Если мы вставим слово в дерево, потом удалим его, а затем снова вставим, дерево должно вернуться в исходное состояние
[<Test>]
let prop_identityOperation (word: string) =
    let trie = Trie.Empty
    let insertedTrie = trie.Insert word
    let removedTrie = insertedTrie.Remove word
    let reInsertedTrie = removedTrie.Insert word
    reInsertedTrie.Root.Children = insertedTrie.Root.Children  // Проверка, что после удаления и вставки дерево вернется к исходному состоянию

let generateTestCases count =
    Array.init count (fun _ ->
        let word1 = generateRandomWord()
        let word2 = generateRandomWord()
        let word3 = generateRandomWord()
        (word1, word2, word3) // Возвращаем кортеж из 3 слов
    )

// Пример использования
let testCases = generateTestCases 6

[<Test>]
let ``Run Property Tests``() =
    testCases |> Array.iter (fun (word1, word2, word3) ->
        printfn "Running tests for words: %s, %s, %s" word1 word2 word3
        Check.Quick (prop_neutralElement word1)  // Тест нейтрального элемента
        Check.Quick (prop_associativeMerge word1 word2 word3)  // Тест ассоциативности
        Check.Quick (prop_identityOperation word1)  // Тест идентичности операции
    )
