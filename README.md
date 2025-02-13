# Лабораторная работа 2
Выаолнил: Батаргин Егор Александрович

Группа: P3332

ITMO.ID: 335189

#
**Перфиксное дерево**

Задание заключалось в том, чтобы создать структуру "Префиксное дерево" или "Trie". И осуществить следующие операции:

 - добавление и удаление элементов;
 - фильтрация;
 - отображение (map);
 - свертки (левая и правая);
 - структура должна быть моноидом.

Теперь пройдем по каждой функции.

**Добавление**

За добавление отвечаеn член класса Insert:

     member this.Insert (word: string) =
            let rec insertNode (node: TrieNode) (word: char list) =
                match word with
                | [] -> { node with Value = Some "" }
                | c :: rest ->
                    let updatedChildren = 
                        if Set.contains c node.Children then 
                            node.Children
                        else 
                            Set.add c node.Children
                    let updatedChild = insertNode { node with Children = updatedChildren } rest
                    updatedChild
            { this with Root = insertNode this.Root (List.ofSeq word) }
Здесь рекурсивно вызвается функция добавления для слова. Так же идет проверка на то, что текущий узел уже существует, чтобы не было дублирования

**Удаление**

За удаление отвечает "Remove":

        member this.Remove (word: string) =
        let rec removeNode (node: TrieNode) (word: char list) =
            match word with
            | [] -> { node with Value = None }
            | c :: rest ->
                if Set.contains c node.Children then
                    let updatedChildren = Set.remove c node.Children
                    let updatedChild = removeNode { node with Children = updatedChildren } rest
                    updatedChild
                else
                    node
        { this with Root = removeNode this.Root (List.ofSeq word) }
Рекурсивно идет обновление узлов.

**Фильтрация**

      member this.Filter (predicate: string -> bool) =
        let rec filterNode (node: TrieNode) (prefix: string) =
            let filteredChildren =
                node.Children
                |> Set.filter (fun c -> predicate (prefix + string c))
                |> Set.fold (fun acc c -> acc + string c) ""
            { node with Children = Set.ofList (filteredChildren |> Seq.toList) }
        { this with Root = filterNode this.Root "" }
**Отбражение**

Отоброжение - это применение некторой функции изменения данных к каждому узлу. Например, изменить везде тип данных.

    member this.Map (f: string -> string) =
          let rec mapNode (node: TrieNode) (prefix: string) =
              let mappedValue = node.Value |> Option.map f
              let mappedChildren = 
                  node.Children
                  |> Set.fold (fun acc c -> acc + string c) ""
              { node with Value = mappedValue; Children = Set.ofList (mappedChildren |> Seq.toList) }
          { this with Root = mapNode this.Root "" }

**Лева и права сверстки**

    member this.FoldLeft (folder: 'a -> string -> 'a) (state: 'a) =
        let rec foldLeftNode (node: TrieNode) (prefix: string) (acc: 'a) =
            let acc' = 
                match node.Value with
                | Some value -> folder acc (prefix + value)
                | None -> acc
            node.Children
            |> Set.fold (fun acc c -> foldLeftNode { node with Children = Set.add c node.Children } (prefix + string c) acc) acc'
        foldLeftNode this.Root "" state

    member this.FoldRight (folder: string -> 'a -> 'a) (state: 'a) =
        let rec foldRightNode (node: TrieNode) (prefix: string) (acc: 'a) =
            let acc' = 
                match node.Value with
                | Some value -> folder (prefix + value) acc
                | None -> acc
            node.Children
            |> Set.fold (fun acc c -> foldRightNode { node with Children = Set.add c node.Children } (prefix + string c) acc) acc'
        foldRightNode this.Root "" state
**Слияние**

Используется для проверки моноидности

     static member Merge (t1: Trie) (t2: Trie) =
            let rec mergeNodes (node1: TrieNode) (node2: TrieNode) =
                let mergedValue =
                    match node1.Value, node2.Value with
                    | Some v1, Some v2 -> Some (v1 + v2)
                    | Some v, None | None, Some v -> Some v
                    | None, None -> None
                let mergedChildren =
                    node1.Children
                    |> Set.fold (fun acc c1 -> 
                        if Set.contains c1 node2.Children then
                            Set.add c1 acc
                        else acc) node2.Children
                { Value = mergedValue; Children = mergedChildren }
            { Root = mergeNodes t1.Root t2.Root }
