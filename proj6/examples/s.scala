




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

def f(x: Int): Int = if (x == 0) 1 else g(x - 1); 
def g(y: Int): Int = f(y); 
def h(z: Int) = z; 
f(0) + f(0) + f(0)