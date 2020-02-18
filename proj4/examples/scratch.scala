// def f(x: Int): Int = x;
// def g(x: Int): Int = x;
// var x = new Array[Int](3);
// x(0) = 4;
// val x = f(2) + x(0);
// x


// val x = (true != true);
// 2

// if(5==5){
//     var x = 5;
//     var x = 3;
//     x
// } else {
//     2
// }


// def g(x: Int):Int = {
//     def f(x:Int):Int = 3; 
//     4
// };
// g(3)

// def g(x: Int):Int = {x};
// def f(x:Int):Int = g(x);
// f(2)


// def g(x: Int):Int = {x};
// def f(x:Int):Int = g(x);
// if(5== 4) {
//     var x = 5;
//     x = 4;
//     x
// } else {
//     f(2)
// }

// var x = 5;
// if(x==5){
//     while(x == 5) {
//         x = 2
//     };
//     x
// } else { 
//     9
// }

// if(5== 5) {
//     var x = 2;
//     while(x < 4){
//         x = x + 1
//     };
//     x
// } else {
//     99
// }

// var x = 2;
// if(5== 5) {
//     x = x + 4
// } else {
//     99
// }

// var x = true;
// if(x==true){
//     2<3
// } else { 
//     false
// };
// 3

// def g(x: Int):Int = {
//     def f(x:Int):Int = 3; 
//     f(5)
// };
// g(3)

// def g(x: Int):Boolean = {
//     true
// };
// if( g(3) == g(4) ) {
//     false
// } else {
//     false
// };
// 5


def g(x: Int):Boolean = {
    false
};
def f(x: Int):Boolean = {
    false
};

if( if( false ) g(3) else if(true) if(f(2)) f(1) else false else true ){
    5
} else {
    4
}