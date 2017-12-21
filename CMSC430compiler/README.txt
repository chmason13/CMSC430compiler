I, Colin Mason, pledge on my honor that I have not given or received any unauthorized assistance on this examination (or assignment).

eq? : Obj Obj -> Boolean
Takes 2 arguments and checks if they both refer to the same object. If both arguments refer to the same object, then the boolean true is returned. Otherwise, the boolean false is returned. Can raise "Too many arguments"(string) and "Not enough arguments" exceptions.

eqv? : Obj Obj -> Boolean
Takes 2 arguments and checks if they both refer to the same object. If both arguments refer to the same object, then the boolean true is returned. Otherwise, the boolean false is returned. Can raise "Too many arguments"(string) and "Not enough arguments" exceptions. (See eq?)

= : Int Int...+ -> Boolean
Takes 2 or more integers and compares the first integer to each subsequent integer. If any of the subsequent integers are not equal to the first integer, then the boolean false is returned. Otherwise the boolean true is returned. Can raise "Illegal argument"(string) and "Not enough arguments"(string) exceptions.

> : Int Int...+ -> Boolean
Takes 2 or more integers and compares the first integer to each subsequent integer. If any of the subsequent integers are not less than the first integer, then the boolean false is returned. Otherwise the boolean true is returned. Can raise "Illegal argument"(string) and "Not enough arguments"(string) exceptions.

< : Int Int...+ -> Boolean
Takes 2 or more integers and compares the first integer to each subsequent integer. If any of the subsequent integers are not greater than the first integer, then the boolean false is returned. Otherwise the boolean true is returned. Can raise "Illegal argument"(string) and "Not enough arguments"(string) exceptions.

<= : Int Int...+ -> Boolean
Takes 2 or more integers and compares the first integer to each subsequent integer. If any of the subsequent integers are not greater than or equal to the first integer, then the boolean false is returned. Otherwise the boolean true is returned. Can raise "Illegal argument"(string) and "Not enough arguments"(string) exceptions.

>= Int Int...+ -> Boolean
Takes 2 or more integers and compares the first integer to each subsequent integer. If any of the subsequent integers are not less than or equals to the first integer, then the boolean false is returned. Otherwise the boolean true is returned. Can raise "Illegal argument"(string) and "Not enough arguments"(string) exceptions.

+ : Int... -> Int
Takes 0 or more integers and adds the first integer to each subsequent integer. Returns 0 if no argument is provided. If one argument is provided, then it returns that integer. Can raise an "Illegal argument"(string) exception.

- : Int...+ -> Int
Takes 1 or more integers and subtracts the first integer from each subsequent integer. If one argument is provided, 1 minus the given argument is returned. Can raise "Illegal argument"(string) and "Not enough arguments"(string) exceptions.

* : Int... -> Int
Takes 0 or more integers and multiplies the first integer from each subsequent integer. If 0 arguments are provided, then 1 is returned. If 1 argument is provided, then the result of multiplying that integer by 1 is returned. Can raise an "Illegal argument"(string) exception.

/ : Int...+ -> Int
Takes 1 or more integers and divides the first integer by each subsequent integer. If 1 argument is provided, then the result of dividing 1 by that integer is returned. Can raise "Illegal argument"(string) and "Not enough arguments"(string) exceptions.

cons? : Obj -> Boolean
Takes 1 argument and checks if this arguments is a pair. If the argument is a pair the boolean true is returned. Otherwise, the boolean false is returned. Can raise "Too many arguments"(string) and"Not enough arguments"(string) exceptions. (See pair?)

null? : Obj -> Boolean
Takes 1 argument and checks if this argument is the empty list. If the argument is the empty list the boolean truen is returned. Otherwise, the boolean false is returned. Can raise "Too many arguments"(string) and "Not enough arguments"(string) exceptions.

cons : Obj Obj -> Pair
Takes 2 arguments and returns a new pair whose car and cdr are the first and second arguments, respectively. Can raise "Too many arguments"(string) and "Not enough arguments"(string) exceptions.

car : Pair -> Obj
Takes 1 pair and returns the car of that pair. Can raise "Illegal argument"(string), "Too many arguments"(string), and "Not enough arguments" exceptions.

cdr : Pair -> Obj
Takes 1 pair and returns the cdr of that pair. Can raise "Illegal argument"(string), "Too many arguments"(string), and "Not enough arguments" exceptions.

procedure? : Obj -> Boolean
Takes 1 argument and checks to see if it is a procedure. If the object is a procedure, then the boolean true is returned. Otherwise, false is returned. Can raise "Too many arguments"(string) and "Not enough arguments" exceptions.

void? : Obj -> Boolean
Takes 1 argument and checks to see if it is the constant #<void>. If the object is the constant #<void>, then the boolean true is returned. Otherwise, false is returned. Can raise "Too many arguments"(string) and "Not enough arguments" exceptions.

integer? : Obj -> Boolean
Takes 1 argument and checks to see if it is an integer or not. If the object is an integer, then the boolean true is returned. Otherwise, the boolean false is returned. Can raise "Too many arguments"(string) and "Not enough arguments" exceptions.

number? : Obj -> Boolean
Takes 1 argument and checks to see if it is a number or not. If the object is a number, then the boolean true is returned. Otherwise, the boolean false is returned. Can raise "Too many arguments"(string) and "Not enough arguments" exceptions.

hash : List List -> Immutable-Hash
Takes a list of keys as the first argument and a list of values as the second argument. Returns immutable hash object that containing key value pairs. Each key in the first list is mapped to the value with the corresponding index in the second list. Can raise "The amount of keys and values provided for hash, must be equal"(string), "Too many arguments for hash"(string), and "Not enough arguments for hash"(string) exceptions.

hash-ref : Immutable-Hash Obj -> Obj
Takes an immutable hash object as the first argument and an object as the second object. If a key value pair exists in the hash provided then the value mapped to the provided key is return. Can raise "Too many arguments for hash-ref"(string), "Not enough arguments for hash-ref"(string), "First argument must be a hash"(string), and "Key value pair does not exist for the provided key"(string) exceptions.

hash-set : Immutable-Hash Obj Obj -> Immutable-Hash
Takes an immutable hash object as the first argument, a key object as the second argument, and a value object as the third argument. It returns a new immutable hash with all the key value pairs in the original hash, plus a new key value pair consisting of the second argument as the key and the third argument as the value. Can raise "Too many arguments for hash-set"(string), "Not enough arguments for hash-set"(string) and "First argument must be a hash"(string) exceptions.

hash-remove : Immutable-Hash Obj -> Immutable-Hash
Takes an immutable hash object as the first argument and a key object as the second argument. It returns a new immutable hash with all the key value pairs in the original hash, minus the key value pair that consisted of the second argument as the key and the third argument as the value. Can raise "Too many arguments for hash-remove"(string), "Not enough arguments for hash-remove"(string) and "First argument must be a hash"(string) exceptions.

EXCEPTIONS:

I handled too many and too few arguments exceptions in desugar. When ever a prim statement is transformed I match on the operation it is called with and check the arguments being passed into this prim relative to the operation. For example, in my implementation, + can be called with 0 arguments but - cannot. So if - is called with 0 arguments then I raise an error. I do these types of checks with all of the prims we support in utils, as well as the prims I created for implementing a hash. For some of the prims I also adjust the argumetn to work with my implementation. For example in the case of +, if no arguments are provided then I create an argument list of two 0s so that my primitive plus operation in header.cpp may still operate on two arguments.

I also attempted to handle application of a non-function value. In desugar, when matching on a list of expressions (untagged application) I check the head of the list to see if the first expression is a key in our environment. This eliminates the possibility of the symbol in function position being a predefined function from a let form. If it isn't in the environment then I check if the head of the list satisfies pair?. If it does not then I throw a "Non function value is applied" exception becuase this would mean that it is some expression that satisfies datum? in function position. If it is a pair then I check to see if the value is the car of the pair is a 'lambda symbol. At this point I figured that I would be able to check if the lambda is recieving the correct amount of arguments considering I still have the rest of the list of expressions at my disposal. So given list L, if the head of the list is an expression that starts with 'lambda then I match on it to obtain its list of arguments. I then compare the length of its list of arguments to the length of L-1. If L-1 is greater than the length of the arguments then I throw a "Function is provided too many arguments error" exception. If its the opposite, then I throw a "Function is provided too few arguments error". I did not get around to attempting to find if a non-initialized let value is used, however, now that I am writing this before its due I do have a theoretical solution if it counts for anything. When matching on a let expression without a * I would iterate through both xs and es, keeping track of every x value I iterate over in a set. If an expression in es, e, was a symbol representing a variable, like 'a', then I would check to see if that variable exists in my hash. If it was not there then I would throw a "Use of uninitialized variable in let binding" exception. 

In my desugar file all exception throws are done with either error or raise. However, I did notice after finally getting project to run semi successfully (I will speak more on later) that the errors I emitted were not stopping the program from executing all the way through in linux at the command line. I assumed the way I was thrwoing errors was correct at least according to racket, so I am unsure why these exceptions weren't raise all the way through. I could never get eval-llvm to run in the Dr. Racket repl due to Windows issues so I always used ubuntu bash to run my prooject. When running tests through my compile.rkt file errors would always halt execution so this was one thing I was not able to solve by the due date.

HASH IMPLEMENTED WITH HAMPT:
I implemented a hash using hampt with a tagging scheme very similar to the qay vectors were tagged. I created a class called hKey that acts as a wrapper class for both the key and value of the hash. So every hash I create would look something like "hamt<hKey, hKey>". The hKey in the key position of the hash stores the key provided by the user. THe hKey in the value position stores the value provided by the user. I created hash, hash-set, hash-ref, and hash-remove prims that rely in the hamt.h constructor, insert, get, and remove methods.  

GC:

I added a garbage collection toggle, but decided to not implement this aspect of the project and to focus on other aspects.

FINAL REMARKS:
I struggled alot to get clang to actually work with my project. I still don't understand why to be honest because I was using all reference solutions but recieved random errors that I even struggled to find solutions to using google. It was actually very frustrating because the aspect of the project that I was told would only take an hour took a large amount of time just to get to work. So much so that I actually had to just implement alot of the other aspects to the project without being able to actually test them thoroughly from the command line. I did not procrastinate on this project. I have actually been working on it for probably the last 72 out 96 last hours and still had a very hard time getting the clang aspect of this to work. I don't know what is considered as thorough for the README extra credit, but I am hoping this suffices for that. This is the last class I need to pass to graduate and I know I am on the borderline so pleeeeeeeaaaaase curve this class. Lastly, thank you for all your instruction this was a very interesting class.