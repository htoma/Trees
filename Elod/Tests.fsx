#load "Tree.fs"
open System
open Tree

let binarySelector (nodeValue:int,value:int) =
    if nodeValue<value then 0 else 1 

let tree = Tree(2, binarySelector)
tree.AddEntry(2, "Two")
tree.AddEntry(3, "Three")
tree.AddEntry(1, "One")
tree.AddEntry(5, "Five")

tree.GetValue 5 // Some "Five"
tree.GetValue 7 // None

let threeWaySelector (nodeValue:int,value:int) =
    if value<=nodeValue/2 then 0
    else if value<=nodeValue then 1
    else 2
        
let ttree = Tree(3, threeWaySelector)
ttree.AddEntry(5, "Five")
ttree.AddEntry(7, "Seven")
ttree.AddEntry(4, "Four")
ttree.AddEntry(9, "Nine")
ttree.AddEntry(12, "Twelve")
ttree.AddEntry(1, "One")
ttree.AddEntry(5, "Five more", true)

ttree.GetValue 1    
ttree.GetValue 2
ttree.GetValue 9
ttree.GetValue 10
ttree.GetValue 5
