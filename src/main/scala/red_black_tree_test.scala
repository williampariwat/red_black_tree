object red_black_tree_test extends App{

    //Test case 1 Integer
    val brt = new red_black_tree[Int]
    brt.insert(3)
    brt.insert(2)
    brt.insert(4)
    brt.insert(6)
    brt.insert(5)
    brt.insert(7)
    brt.insert(9)
    brt.insert(8)
    brt.insert(10)
    brt.insert(11)
    brt.insert(12)

    println(brt.searchTree(5))
    assert(brt.searchTree(16)==false)
    assert(brt.searchTree(3)==true)

    assert(brt.searchTree(11)==true)
    assert(brt.searchTree(14)==false)

    assert(brt.heightCPS(brt.root) == 8)
    //Testing the height function for String type BST
    (brt.printTree())
    assert(brt.size(brt.root) == 11)
    assert(brt.heightCPS(brt.root) == 8)
    assert(brt.sortedArray() == List(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

    println("------------------------------------------------------------------------")
    //Test case 2 Integer
    val brt_1 = new red_black_tree[Int]
    brt_1.root = brt_1.listToTree((List(10,7,3,3,2,4,5,4,3,41,23,21,38,59,24,76,50,36,17)))
    assert(brt_1.Balanced(brt_1.root) == true)
    assert(brt_1.blackHeight(brt_1.root) == 1);
    brt_1.printTree()
    assert(brt_1.size(brt_1.root) == 16)
    assert(brt_1.heightCPS(brt_1.root) == 6)
    assert(brt_1.sortedArray() == List(2, 3, 4, 5, 7, 10, 17, 21, 23, 24, 36, 38, 41, 50, 59, 76))

    println("------------------------------------------------------------------------")
    //Test case 3 String

    val brt_2 = new red_black_tree[String]
    brt_2.root = brt_2.listToTree(List("f","b","a","d","e","t"))
    assert( brt_2.blackHeight(brt_2.root) == 1);

    assert(brt_2.blackBalanced(brt_2.root) == true)
    assert(brt_2.Balanced(brt_2.root) == true)
    assert(brt_2.size(brt_2.root) == 6)
    assert(brt_2.heightCPS(brt_2.root) == 4)
    assert(brt_2.sortedArray() == List("a", "b", "d", "e", "f", "t"))
    brt_2.printTree()
    println("------------------------------------------------------------------------")


    //Test case 4 Double

    val brt_3 = new red_black_tree[Double]
    brt_3.root = brt_3.listToTree(List(3.2,3.2,4.3,4.2,8.9,7.6,23.0,43.1))
    assert( brt_3.blackHeight(brt_3.root) == 1);

    assert(brt_3.blackBalanced(brt_3.root) == true)
    assert(brt_3.Balanced(brt_3.root) == true)
    assert(brt_3.size(brt_3.root) == 7)
    assert(brt_3.heightCPS(brt_3.root) == 5)
    assert(brt_3.sortedArray() == List(3.2, 4.2, 4.3, 7.6, 8.9, 23.0, 43.1))
    brt_3.printTree()
    println("------------------------------------------------------------------------")

    //Test case 5 String

    val brt_4 = new red_black_tree[String]

    brt_4.root = brt_4.listToTree(List("hi","my","name","is","William","6180067"))
    println("Height: " + brt_4.blackHeight(brt_4.root));

    assert(brt_4.blackBalanced(brt_4.root) == true)

    assert(brt_4.Balanced(brt_4.root) == true)
    assert(brt_4.size(brt_4.root)==6)
    assert(brt_4.heightCPS(brt_4.root)==3)
    assert(brt_4.sortedArray() == List("6180067", "William", "hi", "is", "my", "name"))
    brt_4.printTree()

    println("------------------------------------------------------------------------")
    //Test case 6 Char

    val brt_5 = new red_black_tree[Char]

    brt_5.root = brt_5.listToTree(List('9', ':','!', '"','#', '$'))
    assert( brt_5.blackHeight(brt_5.root) == 1);

    assert(brt_5.blackBalanced(brt_5.root) == true)

    assert(brt_5.Balanced(brt_5.root) == true)
    assert(brt_5.size(brt_5.root)==6)
    assert(brt_5.heightCPS(brt_5.root) == 5)
    assert(brt_5.sortedArray()==List('!', '"', '#', '$', '9', ':'))
    brt_5.printTree()
    println("------------------------------------------------------------------------")
}
