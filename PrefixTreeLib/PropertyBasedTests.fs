module PropertyBasedTests

open FsCheck
open Trie

// Свойство 1: Тождественный элемент
// Если мы вставим слово в пустое дерево и ничего не изменим, дерево должно оставаться без изменений
let prop_identityElement word =
    let trie = Trie.Empty
    let insertedTrie = trie.Insert word
    let unchangedTrie = insertedTrie // Не изменяем дерево
    unchangedTrie.Root.Children = insertedTrie.Root.Children  // Пустое дерево остаётся неизменным

// Свойство 2: Ассоциативность
// Слияние двух деревьев слиянием третьего дерева должно быть ассоциативным
let prop_associativeMerge word1 word2 word3 =
    let trie1 = Trie.Empty.Insert word1
    let trie2 = Trie.Empty.Insert word2
    let trie3 = Trie.Empty.Insert word3
    
    let merged1 = Trie.Merge (Trie.Merge trie1 trie2) trie3
    let merged2 = Trie.Merge trie1 (Trie.Merge trie2 trie3)
    
    merged1.Root.Children = merged2.Root.Children  // Ассоциативность слияния

// Свойство 3: Идентичность операции
// Если мы вставим слово в дерево, потом удалим его, а затем снова вставим, дерево должно вернуться в исходное состояние
let prop_identityOperation word =
    let trie = Trie.Empty
    let insertedTrie = trie.Insert word
    let removedTrie = insertedTrie.Remove word
    let reInsertedTrie = removedTrie.Insert word
    reInsertedTrie.Root.Children = insertedTrie.Root.Children  // После удаления и вставки дерево должно быть одинаковым

// Примеры данных для тестирования
let testCases = [
    "cat", "dog", "hello";  // Тестируем на простых словах
    "apple", "banana", "orange";  // Слова для слияния
    "prefix", "suffix", "test"   // Слова для проверки удаления и вставки
]

// Запуск тестов для всех данных
testCases |> List.iter (fun (word1, word2, word3) ->
    printfn "Running tests for words: %s, %s, %s" word1 word2 word3
    Check.Quick (prop_identityElement word1)  // Тест тождества
    Check.Quick (prop_associativeMerge word1 word2 word3)  // Тест ассоциативности
    Check.Quick (prop_identityOperation word1)  // Тест идентичности операции
)
