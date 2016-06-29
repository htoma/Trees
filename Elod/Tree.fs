module Tree

type Node<'a,'b> =
    { Key: 'a
      Value: 'b
      Nodes: Node<'a,'b> option array }

type Tree<'a,'b> when 'a : equality (childNodes:int, childSelector: ('a*'a) ->int) =
    let mutable Root: Node<'a,'b> option = None
   
    member __.AddEntry(key:'a,value:'b) =
        let newNode = {Key=key; Value=value; Nodes= Array.replicate childNodes None} |> Some
        match Root with
        | None ->
            Root <- newNode
        | Some root ->
            let rec findLeaf(node: Node<'a,'b>) =
                let pos = childSelector(root.Key, key)
                if pos >= childNodes
                    then failwith (sprintf "Selector returned index %i larger than limit %i" pos childNodes)

                match node.Nodes.[pos] with
                | None ->
                    node.Nodes.[pos]<-{Key=key; Value=value; Nodes= Array.replicate childNodes None} |> Some
                | Some child ->
                            findLeaf child
                
            findLeaf root

    member __.GetValue(key:'a) =
        let rec findLeaf(node: Node<'a,'b> option) =
            match node with
            | None -> None
            | Some node ->
                    if node.Key=key then Some node.Value
                    else
                    findLeaf node.Nodes.[childSelector(node.Key, key)]
        findLeaf Root
