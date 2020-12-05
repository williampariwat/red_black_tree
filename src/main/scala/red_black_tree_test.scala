object red_black_tree_test extends App{
    val brt = new red_black_tree[Int]
    brt.insert(3)
    brt.insert(2)
    brt.insert(4)
    brt.insert(6)
    brt.insert(5)
//    brt.insert(7)
//    brt.insert(9)
//    brt.insert(8)
//    brt.insert(10)
//    brt.insert(11)
//    brt.insert(12)

    println(brt.searchTree(5))
    println(brt.searchTree(16))
    println(brt.searchTree(3))

    println(brt.searchTree(11))
    println(brt.searchTree(14))

    println("Height: "+ brt.heightCPS(brt.root))
//    assert(brt.heightCPS(brt.root) == 8)
    //Testing the height function for String type BST

    (brt.printTree())
    println("Height of black: "+brt.blackHeight(brt.root))
    println("Size: " + brt.size(brt.root))
    println(brt.sortedArray())


}
