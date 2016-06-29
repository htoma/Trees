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

tree.GetValue 7

