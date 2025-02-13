
//Пример вида дерева: Node(true, Set[('a', Node(false, Set[('p', Node(false, Set[('p', Node(true, Set[('l', Node(false, Set[('e', Empty)]))]))]))]))]))]))


let rec insert (elements: 'T list) (trie: Trie<'T>) : Trie<'T> =
    match elements, trie with
    | [], Node (_, children) -> Node (true, children) // Если узел уже сществует (Node (_, children)), мы помечаем его как конечный узел
    | [], Empty -> Node (true, Set.empty) // Если текущий узел пуст (то есть Empty), создаем новый конечной узел
    | x :: xs, Node (isEnd, children) -> // Если список элементов не пустой (x :: xs),мы обрабатываем текущий символ x и рекурсивно добаляем оставшиеся из xs
        //Тут мы делаем проверку на то, что уже такой узел может сущетсвоать. Например, буде слово apple и app.
        //Когда пройдем по apple, мы получим дерево () - a - p - p -l - e,
        //Когда пройдем по слову app, то дерево не должно визульно измениться. Лишь пометить, что второе p конечный символ
        //Если не делать провреку на наличе, то после app дерево будет: a - p - p
        let child = // Ищем дочерний узел, соответствующий x
            match Set.tryFind (fun (k, _) -> k = x) children with
            | Some (_, t) -> t //Если такой узел найден, то используем его
            | None -> Empty //Если нет, создаем пустой узел (Empty)
        let newChild = insert xs child // Рекурсивно добавляем оставшиеся элементы из xs
        Node (isEnd, Set.add (x, newChild) (Set.remove (x, child) children)) // Удаляем старый дочерний узел, добавляем новый дочерний (обновляем узел)
    | x :: xs, Empty ->
        let newChild = insert xs Empty //Добавляем рекурсивно символы xs в пустой узел
        Node (false, Set.singleton (x, newChild)) //Создаем новый узел с текущим символом x и дочерним узлом newChild


//Реализация удаления
let rec remove (elements: 'T list) (trie: Trie<'T>) : Trie<'T> =
    match elements, trie with
    //Если список элементов пустой, то мы дошли до конца
    //Если при этом текущий узел существует, то  мы снимаем пометку, что это конец (флаг isEnd)
    //Еслти Trie пуст, то ничего не делаем
    | [], Node (_, children) -> Node (false, children)
    | [], Empty -> Empty
    | x :: xs, Node (isEnd, children) -> //Мы обрабатываем текущий символ x и рекурсивно удаляем оставшиеся символы из xs
       //Ищем дочерний узел, который соответствует символу x
        match Set.tryFind (fun (k, _) -> k = x) children with
        | Some (_, t) -> //Если узел найден, то продолжаем удаление
            let newChild = remove xs t  // Рекурсивно делаем удаление для остальных xs и дочернего узла t
            if newChild = Empty then //Если дочерний узел пустой, то он нам больше не нужен
                Node (isEnd, Set.remove (x, t) children)
            else //Если новый дочерний узел не пуст, обновляем его в наборе дочерних узлоав (удаляем ветку, если данный узел используется в других словах)
                Node (isEnd, Set.add (x, newChild) (Set.remove (x, t) children))
        | None -> trie //Если узел не найден  (None), строка отсутвует в Trie, то ничего не делаем
    | _ -> trie //Если Trie пуст или список элементов пуст, ничего не делаем

//Реализация фильтрации
let rec filter (predicate: 'T list -> bool) (trie: Trie<'T>) : Trie<'T> =
    //Функция helper рукурсивно Trie и проверяет, удовлетворяет ли текущий префикс (path) предикату
    //path - текущий префикс (список символов от корня до текущего узла)
    //trie - текущий узел Trie
    //в качетсве return идет новый узел Trie, который содержит те строки, который удовлетворяют предикату
    let rec helper path (trie: Trie<'T>) : Trie<'T> =
        match trie with
        | Node (isEnd, children) -> //Если текущий узел существует, обрабатываем дочерние узлы
        // |> - означает, что мы функцию применяем для каждого элемента набора children
            let newChildren =
                children
                //Применяю Set.map к каждому элементу набора children. Для каждого дочрнего узла (k, t):
                // k -  символ, t - дочерний Trie
                // Идет вызыв helper как вспомогательной для дочернего узла t, передавая обновленный префикс
                |> Set.map (fun (k, t) -> (k, helper (path @ [k]) t)) // Знак @ позволяет добавлять элемент в список, при этом делая сам path неизменяемым. 
                //Мы филтруем дочерние узлы, отсавляя те, которые не являються пустыми, чтобы удалить те узлы, которые больше неиспользуються.
                |> Set.filter (fun (_, t) -> t <> Empty)
                //Если текущий префикс path удовлетворяет предикату или у узла есть узлы, которые нее были удалены, то узел сохраняется
            if predicate path || not (Set.isEmpty newChildren) then
                Node (isEnd, newChildren)
            else
                Empty //иначе возвращается пустым
        | Empty -> Empty
    helper [] trie

//Отображение map
//Данная функция используется для изменения типа данных в дереве Trie, не меняя его структуру. 
// В данном случае, изменение идет с типа 'T в тип 'U

// На вход приходит дерево trie  с типом данных  'T
//mapping отвечат за изменение типа 'T в тип 'U
// С помощью паттерна match для trie применяем условия проверки узла на Node или Empty
// Если узел  - Node, то мы создаем новый узел newChildren, и применяем для него фцнкцию mapping
let rec map (mapping: 'T -> 'U) (trie: Trie<'T>) : Trie<'U> =
    match trie with
    | Node (isEnd, children) ->
        let newChildren =
            children // Намбор дочерних узлов текщуего узла 
            |> Set.map (fun (k, t) -> (mapping k, map mapping t)) //Преобразовываем k, и потом рекурсивно преобразовываем t
        Node (isEnd, newChildren) //  Сохраняем новый узел
    | Empty -> Empty //Если узел пустой, то ничего и не делаем




//Свертки (лева и правая)


// Делает агрегаци (объединение) информации в одно значение. 
// Функция floder - функция, принимающая текущее состояние и объект и возвращает новое состояние

//В данном случае floder посчитает количество узлов
let folder (acc: int) (ch: 'T) : int =
    acc + 1

//  state - это текущее состоние. В начале оно может быть пустым списком или 0
// trie - текущее дерево
// Если узел сущетсвует, то мы применям операцию свертки
// Set.flod - агрегация для набора Set. Они принмает на вход три аргумента: функция обработки, начальное состояние и набор, который обрабатывается

//Про функцию внутри Ste.flot^
//acc - текущее состоние агрегированное состоние
// foldLeft folder (folder acc k) t - рекурсивынй вызов foldLeft для дочернего узла 

//Текущая верстка делается слева направо 
let rec foldLeft (folder: 'State -> 'T -> 'State) (state: 'State) (trie: Trie<'T>) : 'State =
    match trie with
    | Node (_, children) ->
        Set.fold (fun acc (k, t) -> foldLeft folder (folder acc k) t) state children
    | Empty -> state

//Текущая свертка делается справа налево
let rec foldRight (folder: 'T -> 'State -> 'State) (state: 'State) (trie: Trie<'T>) : 'State =
    match trie with
    | Node (_, children) ->
        Set.foldBack (fun (k, t) acc -> folder k (foldRight folder acc t)) children state
    | Empty -> state

//Моноид для двух деревьев

//Моноид соблюдается за счет:
// 1) Есть обработка случаев, когда одно дерево пустое. В таком случа результат слияния равен другому дереву
// 2) Независимость от порядка объядинения: trie1 combine trie2 = trie2 combine trie1
// 3) Соблюдается корректность объединения деревьев:
//  Если ключа k найдены поддеревья в обоих случаях , мы рекурсивно объединяем.
//  Или оставляем поддерево того дерева, в котором найден ключ
//  В Node (isEnd1 || isEnd2, newChildren) мы добавляем узел
let empty : Trie<'T> = Empty

let rec combine (trie1: Trie<'T>) (trie2: Trie<'T>) : Trie<'T> =
    match trie1, trie2 with
    | Node (isEnd1, children1), Node (isEnd2, children2) ->
        let newChildren =
            Set.union children1 children2
            |> Set.map (fun (k, t) ->
                let t1 = Set.tryFind (fun (k', _) -> k' = k) children1 |> Option.map snd
                let t2 = Set.tryFind (fun (k', _) -> k' = k) children2 |> Option.map snd
                match t1, t2 with
                | Some t1', Some t2' -> (k, combine t1' t2')
                | Some t1', None -> (k, t1')
                | None, Some t2' -> (k, t2')
                | None, None -> (k, Empty)
            )
        Node (isEnd1 || isEnd2, newChildren)
    | Node (isEnd, children), Empty -> Node (isEnd, children)
    | Empty, Node (isEnd, children) -> Node (isEnd, children)
    | Empty, Empty -> Empty

// Моноид для трех деревьев

// То же самое все.
let empty : Trie<'T> = Empty

let rec combineThree (trie1: Trie<'T>) (trie2: Trie<'T>) (trie3: Trie<'T>) : Trie<'T> =
    match trie1, trie2, trie3 with
    | Node (isEnd1, children1), Node (isEnd2, children2), Node (isEnd3, children3) ->
        let newChildren =
            // Объединяем все три набора детей
            Set.union (Set.union children1 children2) children3
            |> Set.map (fun (k, t) ->
                let t1 = Set.tryFind (fun (k', _) -> k' = k) children1 |> Option.map snd
                let t2 = Set.tryFind (fun (k', _) -> k' = k) children2 |> Option.map snd
                let t3 = Set.tryFind (fun (k', _) -> k' = k) children3 |> Option.map snd
                match t1, t2, t3 with
                | Some t1', Some t2', Some t3' -> (k, combineThree t1' t2' t3')
                | Some t1', Some t2', None -> (k, combine t1' t2')
                | Some t1', None, Some t3' -> (k, combine t1' t3')
                | None, Some t2', Some t3' -> (k, combine t2' t3')
                | Some t1', None, None -> (k, t1')
                | None, Some t2', None -> (k, t2')
                | None, None, Some t3' -> (k, t3')
                | None, None, None -> (k, Empty)
            )
        Node (isEnd1 || isEnd2 || isEnd3, newChildren)
    | Node (isEnd, children), Empty, Empty
    | Empty, Node (isEnd, children), Empty
    | Empty, Empty, Node (isEnd, children) -> Node (isEnd, children)
    | Empty, Empty, Empty -> Empty
