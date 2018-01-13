# Simple Integral for Math.js
This library is an extension for the popular [Math.js](http://mathjs.org) library. It adds an `integral` function that is capable of finding the integrals of simple mathematical functions. It is also flexible in the integration rules used, allowing custom rules to be be used instead of or in addition to the standards rules.

Note that this software is still considered beta, so if you come across any bug or rough edges, please submit an issue!


## Install
To install the package, use the following:
```
npm install mathjs-simple-integral
```

To register the extension with Math.js, use the following:
```javascript
math.import(require('mathjs-simple-integral'));
```
This will add the function `math.integral` to Math.js.


## Usage
The basic usage of this extension is very easy: just provide the integrand (either as a `Node` or a string) and the variable of integration (either as a `SymbolNode` or a string):

```javascript
math.integral('x^2', 'x'); // 'x ^ 3 / 3'
math.integral('1/x', 'x'); // 'log(abs(x))'
math.integral('e^x', 'x'); // 'e^x'
math.integral('cos(2*x+pi/6)', 'x'); // 'sin(2 * x + pi / 6) / 2'
```

If `integral` is unable to find the integral of the given expression, it will throw an error:
```javascript
math.integral('e^(x^2)'); // Error: Unable to find integral of "e ^ (x ^ 2)" with respect to "x"
```


### Simplification
By default, `integral` runs `math.simplify` on the output, as the integration process can produce some unwieldy expressions. However, it is possible to get raw, unsimplified output from `integral` by passing in an `options` object with `simplify` set to `false` to the optional third parameter:
```javascript
math.integral('x^2', 'x', {simplify: false}); // '1 / (2 + 1) * x ^ (2 + 1)'
math.integral('1/(2*x)', 'x', {simplify: false}); // '2 ^ -1 * log(abs(x))'
```


### Custom Rules
In this implementation, integration rules are defined as a function that takes as parameters (1) `expr`, the expression to be integrated; (2) `context`, the context of integration; and (3) `subIntegral`, a function that tries to integrate a subexpression or rewritten form of the integral. The integration rule then returns the computed integral, or `null` if it was unable to find one. In addition to many standard integration already implemented (located at `math.integral.rules`), a custom set of integration rules can be specified.

For example, suppose we added a custom function `myUpperGamma` representing the [Upper Incomplete Gamma Function](https://en.wikipedia.org/wiki/Incomplete_gamma_function), and we now want to add support for integrating it, particularly we want to implement this rule: `integral("myUpperGamma(s,x)", "x") = x*myUpperGamma(s,x) - myUpperGamma(s+1, x)` ([verify here]()). First, let us write this rule as a function:

```javascript
var myUpperGammaRule = function(expr, context, subIntegral) {
  // Ensure we are trying to integrate a FunctionNode
  if(expr.type === "FunctionNode") {
    // Ensure we are trying to integrate 'myUpperGamma' and that it has 2 arguments
    if(expr.name === "myUpperGamma" && expr.args.length === 2) {
      // Ensure that the first argument is constant and the second one is
      // the variable of integration
      if(context.isConstant(expr.args[0]) && expr.args[1].equals(context.variable)) {
        // Yay, we matched 'myUpperGamma(s,x)', so we know the integral!
        return new OperatorNode('-', 'subtract', [
          new OperatorNode('*', 'multiply', [
            context.variable,
            new FunctionNode('myUpperGamma', [
              expr.args[0],
              context.variable
            ])
          ]),
          new FunctionNode('myUpperGamma', [
            new OperatorNode('+', 'add', [
              expr.args[0],
              new ConstantNode(1)
            ]),
            context.variable
          ])
        ]);
      }
    }
  }
  // Our rule, didn't apply :(
  // return undefined
}
```

Now that we have our custom integration rule, we can add it to the list of standard rules and use this combined list to find integrals involving `myUpperGamma`!

```javascript
// Define our integration rules to include our custom rule
var options = { rules: math.integral.rules.concat([myUpperGammaRule]) };

// Compute integrals of our function!
math.integral('myUpperGamma(a,x)', 'x', options);
// 'x*myUpperGamma(a,x) - myUpperGamma(a+1,x)'

math.integral('myUpperGamma(a^2,x) + 13', 'x', options);
// 'x*myUpperGamma(a^2,x) - myUpperGamma(a^2+1,x) + 13*x'

math.integral('myUpperGamma(a,5*x+99)', 'x', options);
// '((5*x+99) * myUpperGamma(a, 5*x+99) - myUpperGamma(a+1, 5*x+99)) / 5'

math.integral('sqrt(myUpperGamma(a,x)) * sqrt(myUpperGamma(a,x))', 'x', options);
// 'x*myUpperGamma(a,x) - myUpperGamma(a+1,x)'
```

Now suppose in addition to `myUpperGamma` we have another custom function `mySum`: just like `add`, it will accept a variable number of arguments and add the arguments together, but it also performs some other non-mathematical function (such as logging each argument before evaluation, or checking to ensure that none of its arguments are `NaN`). Now, we can add rules to the integrator that represent the linearity of the integral over `mySum`:

```javascript
// integral(mySum(f(x), g(x), ...), x) = mySum(integral(f(x), x), integral(g(x), x), ...)
function mySumRule(expr, context, subIntegral) {
  // Ensure we are trying to integrate a FunctionNode
  if(expr.type === "FunctionNode") {
    // Ensure we are trying to integrate 'mySum'
    if(expr.name === "mySum") {
      // Try to find integrals of all the terms in the sum
      var termIntegrals = expr.args.map(function(term) {
        return subIntegral(term, context, 'sum rule (mySum)');
      });

      // Only if all terms had integrals did we actually find an integral
      if(termIntegrals.every(function(termInt) { return !!termInt; })) {
        // Yay, we found the integral!
        return new FunctionNode('mySum', termIntegrals);
      }
    }
  }
  // return undefined
}
```

Note how we use the `subIntegral` callback to find the integral of all the terms in the sum, and then returns an integral for the entire `mySum` expression only if all the individual terms could be integrated. Now, if we use both of our custom rules, we can integrate expressions with both `mySum` and `myUpperGamma`:

```javascript
var options = { rules: math.integral.rules.concat([myUpperGammaRule, mySumRule]) };

math.integral("mySum(3*x^2, x, 1/x, 1)", "x", options);
// 'mySum(x^3, x^2/2, log(abs(x)), x)'

math.integral("mySum(2*x, myUpperGamma(a,x))", "x", options);
// 'mySum(x^2, x*myUpperGamma(a,x) - myUpperGamma(a+1,x))'
```

### Debug Output
The options object can have a property `debugPrint` that, if set to `true`, will instruct the integrator to "show its work": that is, it will print to the console all steps taken and all rules applied in a particular integration. For example, `integral("x^2 + sin(pi*x)", "x", {debugPrint: true})` produces the following output on the console:

```
find integral of (x ^ 2) + (sin(pi * x))  dx
  sum rule: find integral of x ^ 2  dx
    Computed: (1 / (2 + 1)) * (x ^ (2 + 1))
  sum rule: find integral of sin(pi * x)  dx
    linear substitution: find integral of sin(x)  dx
      Computed: -(cos(x))
    Computed: (-(cos(pi * x))) / (pi * 1)
  Computed: ((1 / (2 + 1)) * (x ^ (2 + 1))) + ((-(cos(pi * x))) / (pi * 1))
```

Using `debugPrint` is a good way to learn how the integration rules interact and combine to find integrals, and is extremely helpful when developing custom integration rules.


## Algorithm
The integration algorithm used by this implementation is based on a depth-first search memoized pattern matching. Each pattern is specified as a function that attempts to either compute the integral directly, or to rewrite or split the integrand into an expression that is easier to integrate, and computing the integral of that. The core of the integrator is rule agnostic: by default it uses a set of standard integration rules stored at `integral.rules`, although using custom rules is supported (see the [Custom Rules](#custom-rules) section).

The memoization of integrands offers not only a speedup for common integrals of sub-expressions, but also prevents infinite loops if two rules undo each other (such as if one multiplies out exponents, and the other combines these common factors). However, this algorithm is still susceptible to a rule (or to several interacting rules) that can apply an infinite number of times producing a new, unique integrand every application; such a rule would cause the integrator to recurse indefinitely (or, until all stack space or memory is consumed).


### Limitations
Due to the simplicity of this approach (and the relative early stage of development of this package), there are many limitations in this current implementation. To give an incomplete list, there are currently no rules to support the following:
 - Integration-by-parts or u-substitution (except for u-substitution of a linear function when the variable of integration is unique in the integrand). This means we cannot integrate the following expressions:
   - `2 * x * cos(x^2)`
   - `3 * x^2 * e^(x^3)`
   - `x * ln(x)`
   - `x^3 * e^x`
 - Partial fractions decomposition to integrate rational functions
 - Inverse trigonometric functions
 - Trigonometric simplification, as is needed for expressions like `cos(x) * tan(x)`
 - Integrating `e ^ -(x^2)` and the like using [`erf`](https://en.wikipedia.org/wiki/Error_function)

If implement a rule to handle one of these cases (or any other currenly unsupported case), please submit a pull request!


## License
Copyright (c) 2018 Joel Hoover (joelahoover@gmail.com)

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
