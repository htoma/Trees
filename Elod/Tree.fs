module Tree

type Node<'a,'b> =
    { Key: 'a
      mutable Value: 'b //value can be updated for an existing key
      Nodes: Node<'a,'b> option array }

type Tree<'a,'b> when 'a : equality (childNodes:int, childSelector: ('a*'a) ->int) =
    let mutable Root: Node<'a,'b> option = None
   
    member __.AddEntry(key:'a,value:'b,?upsert:bool) =
        let newNode = {Key=key; Value=value; Nodes= Array.replicate childNodes None} |> Some //precreate the node to be added
        match Root with
        | None ->
            Root <- newNode
        | Some root ->
            let rec findLeaf(node: Node<'a,'b>) =
                match node.Key,upsert with //check on key node and the flag for insert/update
                | sameKey,Some flag when sameKey=key && flag=true -> 
                    printfn "Updating an existing key: %A with %A" key value
                    node.Value<-value
                | _ ->    
                    //it's all insert
                    let pos = childSelector(root.Key, key)
                    if pos >= childNodes
                        then failwith (sprintf "Selector returned child index %i larger than limit %i" pos childNodes)

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
