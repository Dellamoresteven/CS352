




// def f() : Int = 5;
// f()

// def f(): Int = g(); 
// def g(): Int = 5; 
// f()

// def f() : Int = {
//   var idx = 8;
//   5
// };
// f()

// def f(x: Int): Int = x + 1;
// f(0)


// def nl() = putchar(10);
// putchar((if (0.isInt) 'O' else 'K').toInt);
// putchar((if ('K'.isInt) 'O' else 'K').toInt);
// nl();
// 5

// val u = getchar(); 
// putchar(0 + u);
// 2

// def f(x: Int): Int = g(x); 
// def g(x: Int): Int = f(x); 
// if (getchar() == -1) 
//     g(3) 
// else 0

// putchar((((getchar()/3)/3)/3))

// def f(x: Int): Int = if (x == 0) 0 else g(x - 1); 
// def g(y: Int): Int = f(y); 
// def h(z: Int) = z; 
// f(0)

// def f(x: Int): Int = {
//     if(x < 1) {
//         0
//     } else {
//         f(0)
//     }
// };
// f(1)

val intPrintAsChar = functionCompose[Int,Char,Unit](printChar, (x: Int) => x.toChar);
val o = listMake1[Int](79);
val k = listMake3[Int](3, 5, 5);
val nl = listMake2[Int](2, 5);
def prod(l: List[Int]) = listFoldLeft[Int,Int]((x: Int, y: Int) => x * y, 1, l);
intPrintAsChar(prod(o));
intPrintAsChar(prod(k));
intPrintAsChar(prod(nl));
5