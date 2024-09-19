In Scheme, a dialect of Lisp, the language design emphasizes the use of lists and expressions in various constructs. Scheme allows the inclusion of lists of expressions in several constructs, not just `begin`. Below is a list of some key constructs where lists of expressions are admitted, along with brief explanations and examples for each:

1. **`begin`**: Used to sequence expressions. When executed, each expression in the list is evaluated in order, and the value of the last expression is returned.

   ```scheme
   (begin
     (display "Hello, ")
     (display "world!\n"))
   ```

2. **`let`**: Allows binding of variables to values for a sequence of expressions. The variables are local to the `let` expression.

   ```scheme
   (let ((x 5)
         (y 10))
     (+ x y))
   ```

3. **`let*`**: Similar to `let`, but the bindings are performed sequentially, allowing later bindings to refer to earlier ones.

   ```scheme
   (let* ((x 5)
          (y (+ x 5)))
     (+ x y))
   ```

4. **`letrec`**: Allows for recursive definitions. Variables can be bound to values that themselves can refer to the bound variables.

   ```scheme
   (letrec ((fact (lambda (n)
                    (if (= n 0)
                        1
                        (* n (fact (- n 1)))))))
     (fact 5))
   ```

5. **`cond`**: Provides a way to choose from multiple branches of code. Each branch is a list of a predicate expression and one or more consequent expressions.

   ```scheme
   (cond ((> 3 2) 'greater)
         ((< 3 2) 'less))
   ```

6. **`case`**: Similar to `cond`, but chooses the branch based on the value of an expression matching a list of possible values.

   ```scheme
   (case (+ 2 3)
     ((5) 'five)
     ((6) 'six))
   ```

7. **`do`**: A looping construct that iterates based on a termination condition, allowing for sequences of expressions to be executed each iteration.

   ```scheme
   (do ((i 0 (+ i 1)))
       ((= i 5) i)
     (display i)
     (newline))
   ```

8. **`lambda`**: Used to create anonymous functions. The body of a `lambda` expression can contain multiple expressions, wrapped in a `begin` implicitly if more than one expression is present.

   ```scheme
   ((lambda (x)
      (display "Square of ")
      (display x)
      (display " is ")
      (* x x))
    5)
   ```

9. **`define`**: Used for defining variables and functions. When defining functions, the body can contain multiple expressions.

   ```scheme
   (define (square x)
     (display "Calculating square of ")
     (display x)
     (* x x))
   ```

10. **`and`, `or`**: Logical constructs that can evaluate multiple expressions, short-circuiting if the outcome is determined before all expressions are evaluated.

    ```scheme
    (and (> 5 0) (< 5 10))
    ```

    ```scheme
    (or (null? '()) (= 5 10))
    ```