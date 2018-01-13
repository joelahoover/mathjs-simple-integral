var mathRaw = require('mathjs');
var expect = require('chai').expect;
require('mocha');

var integral = require('../lib/integral');

describe('Integral Function', function() {

  function getMathWithIntegral() {
    var math = mathRaw.create();
    math.import(integral);
    return math;
  }

  function compareSimplifiedStrings(left, right) {
    expect(mathRaw.simplify(left).toString()).to.equal(mathRaw.simplify(right).toString());
  }

  it('should exist in mathjs after being imported', function() {
    var math = getMathWithIntegral();
    expect(math).to.respondTo('integral');
  });

  it('should be callable with any combinations of parameter types', function() {
    var math = getMathWithIntegral();

    var options = { simplify: false };
    var x = math.parse("x");
    expect(math.integral.bind(null, "x", "x", options)).to.not.throw();
    expect(math.integral.bind(null, "x", "x")).to.not.throw();
    expect(math.integral.bind(null, "x", x, options)).to.not.throw();
    expect(math.integral.bind(null, "x", x)).to.not.throw();
    expect(math.integral.bind(null, x, "x", options)).to.not.throw();
    expect(math.integral.bind(null, x, "x")).to.not.throw();
    expect(math.integral.bind(null, x, x, options)).to.not.throw();
    expect(math.integral.bind(null, x, x)).to.not.throw();
  });

  it('should find basic integrals', function() {
    var math = getMathWithIntegral();

    compareSimplifiedStrings(math.integral("1", "x"), "x");
    compareSimplifiedStrings(math.integral("x", "x"), "1/2*x^2");
    compareSimplifiedStrings(math.integral("+x", "x"), "1/2*x^2");
    compareSimplifiedStrings(math.integral("-x", "x"), "-1/2*x^2");
    compareSimplifiedStrings(math.integral("(((x)))", "x"), "1/2*x^2");
    compareSimplifiedStrings(math.integral("x + 1", "x"), "1/2*x^2 + x");
    compareSimplifiedStrings(math.integral("2*x", "x"), "x^2");
    compareSimplifiedStrings(math.integral("x/2", "x"), "1/4*x^2");
    compareSimplifiedStrings(math.integral("x/x", "x"), "x");
  });

  it('should find integrals of polynomials', function() {
    var math = getMathWithIntegral();

    compareSimplifiedStrings(math.integral("x^2", "x"), "1/3*x^3");
    compareSimplifiedStrings(math.integral("x^3", "x"), "1/4*x^4");
    compareSimplifiedStrings(math.integral("8*x^3 + 3x^2 + 1", "x"), "2*x^4+x^3+x");
    compareSimplifiedStrings(math.integral("11*x^10", "x"), "x^11");
  });

  it('should find integrals of polynomial expression not written in standard form', function() {
    var math = getMathWithIntegral();

    compareSimplifiedStrings(math.integral("12 * (2 * x + 1)", "x"), "12*(x^2 + x)");
    compareSimplifiedStrings(math.integral("10 * x * 4 * y", "x"), "20*y*x^2");
    compareSimplifiedStrings(math.integral("12 * (x * x)", "x"), "4*x^3");
    compareSimplifiedStrings(math.integral("12 * x * x", "x"), "4*x^3");
    compareSimplifiedStrings(math.integral("(12 * x) * x", "x"), "4*x^3");
    compareSimplifiedStrings(math.integral("3 * +x * -x", "x"), "-x^3");
    compareSimplifiedStrings(math.integral("x * (4 * x^2 + (1+1)*y)", "x"), "x^4 + y*x^2");
    compareSimplifiedStrings(math.integral("(x + 1) * (x - 1)", "x"), "1/3*x^3 - x");
    compareSimplifiedStrings(math.integral("(-x)^3", "x"), "-1/4*(-x)^4");
    compareSimplifiedStrings(math.integral("(x + 1)^3", "x"), "1/4*(x+1)^4");
    compareSimplifiedStrings(math.integral("((1/8)*x + 1)^3", "x"), "2*((1/8)*x+1)^4");
    compareSimplifiedStrings(math.integral("x * (x+1)^2 * x", "x"), "1/2 * x^4 + 1/5 * x^5 + x^3/3");

    compareSimplifiedStrings(math.integral("add(2, x, 1)", "x"), "3*x + x^2/2");
    compareSimplifiedStrings(math.integral("subtract(3*x^2, 1)", "x"), "x^3 - x");
    compareSimplifiedStrings(math.integral("multiply(x, 5, x, x^2)", "x"), "x^5");
    compareSimplifiedStrings(math.integral("divide(x, 10)", "x"), "x^2/20");
  });

  it('should find integrals with x to non-integer, negative, and variable powers', function() {
    var math = getMathWithIntegral();

    compareSimplifiedStrings(math.integral("x^3.5", "x"), "1/4.5*x^4.5");
    compareSimplifiedStrings(math.integral("x^-3", "x"), "-1/2*x^-2");
    compareSimplifiedStrings(math.integral("x^-3.6", "x"), "-1/2.6*x^-2.6");
    compareSimplifiedStrings(math.integral("1/x", "x"), "log(abs(x))");
    compareSimplifiedStrings(math.integral("x^-1", "x"), "log(abs(x))");
    compareSimplifiedStrings(math.integral("x^(6-6.5 + 8-8.5)", "x"), "log(abs(x))");
    compareSimplifiedStrings(math.integral("1/(x^-4)", "x"), "1/5*x^5");
    compareSimplifiedStrings(math.integral("x^(b-1)", "x"), "1/b * x^b");
    compareSimplifiedStrings(math.integral("sqrt(x)", "x"), "2/3*x^(3/2)");
    compareSimplifiedStrings(math.integral("nthRoot(x,6)", "x"), "x^(7/6) * 6/7");
    compareSimplifiedStrings(math.integral("nthRoot(e,1/x)", "x"), "e^x");
  });

  it('should find integrals of exponential functions', function() {
    var math = getMathWithIntegral();

    compareSimplifiedStrings(math.integral("e^x", "x"), "e^x");
    compareSimplifiedStrings(math.integral("2^x", "x"), "2^x / log(2)");
    compareSimplifiedStrings(math.integral("a^x", "x"), "a^x / log(a)");
    compareSimplifiedStrings(math.integral("e^(x+1)", "x"), "e^(x+1)");
    compareSimplifiedStrings(math.integral("e^(2*x+1)", "x"), "1/2*e^(2*x+1)");
    compareSimplifiedStrings(math.integral("e^-x", "x"), "-e^-x");
    compareSimplifiedStrings(math.integral("(e^x)^b", "x"), "e^(x*b)/b");
    compareSimplifiedStrings(math.integral("pow(a, -x)", "x"), "-(1/log(a) * a^(-x))");
    compareSimplifiedStrings(math.integral("exp(x)", "x"), "e^x");
  });

  it('should find integrals of logrithmic functions', function() {
    var math = getMathWithIntegral();

    compareSimplifiedStrings(math.integral("log(x)", "x"), "x*log(x) - x");
    compareSimplifiedStrings(math.integral("log(x, 10)", "x"), "1/log(10)*(x*log(x) - x)");
    compareSimplifiedStrings(math.integral("1/log(10, x)", "x"), "1/log(10)*(x*log(x) - x)");
  });

  it('should find integrals of basic trigonometric functions', function() {
    var math = getMathWithIntegral();

    compareSimplifiedStrings(math.integral("sin(x)", "x"), "-cos(x)");
    compareSimplifiedStrings(math.integral("cos(x)", "x"), "sin(x)");
    compareSimplifiedStrings(math.integral("tan(x)", "x"), "log(abs(sec(x)))");
    compareSimplifiedStrings(math.integral("sec(x)", "x"), "log(abs(sec(x) + tan(x)))");
    compareSimplifiedStrings(math.integral("csc(x)", "x"), "log(abs(csc(x) - cot(x)))");
    compareSimplifiedStrings(math.integral("cot(x)", "x"), "log(abs(sin(x)))");

    compareSimplifiedStrings(math.integral("cos(2*x+1)", "x"), "sin(2*x+1)/2");
    compareSimplifiedStrings(math.integral("sec(pi*(x+1/2))", "x"), "log(abs(sec(pi*(x+1/2)) + tan(pi*(x+1/2))))/pi");
  });

  it('should use custom rules while integrating', function() {
    var math = getMathWithIntegral();
    var OperatorNode = math.expression.node.OperatorNode;
    var FunctionNode = math.expression.node.FunctionNode;
    var ConstantNode = math.expression.node.ConstantNode;

    var rules = math.integral.rules.concat([
      // myfunc is actually just the incomplete gamma function
      // http://www.wolframalpha.com/input/?i=integral+gamma(a,x)+dx
      // integral(myfunc(c, x), x) = x*myfunc(c, x) - myfunc(c+1, x)
      function(expr, context, subIntegral) {
        // Ensure we are trying to integrate a FunctionNode
        if(expr.type === "FunctionNode") {
          // Ensure we are trying to integrate 'myfunc' and that it has 2 arguments
          if(expr.name === "myfunc" && expr.args.length === 2) {
            // Ensure that the first argument is constant and the second one is
            // the variable of integration (as we can only find an integral under
            // those constraints)
            if(context.isConstant(expr.args[0]) && expr.args[1].equals(context.variable)) {
              // Yay, we found the integral!
              return new OperatorNode('-', 'subtract', [
                new OperatorNode('*', 'multiply', [
                  context.variable,
                  new FunctionNode('myfunc', [
                    expr.args[0],
                    context.variable
                  ])
                ]),
                new FunctionNode('myfunc', [
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
      },
      // mysum is just a regular addition, but maybe it does some logging or such,
      // so we want to keep it. However, we want to integrate using the linearity
      // property of mysum.
      // integral(mysum(f(x), g(x), ...), x) = mysum(integral(f(x), x), integral(g(x), x), ...)
      function(expr, context, subIntegral) {
        // Ensure we are trying to integrate a FunctionNode
        if(expr.type === "FunctionNode") {
          // Ensure we are trying to integrate 'mysum'
          if(expr.name === "mysum") {
            // Try to find integrals of all the terms in the sum
            var termIntegrals = expr.args.map(function(term) {
              return subIntegral(term, context);
            });

            // Only if all terms had integrals did we actually find an integral
            if(termIntegrals.every(function(termInt) { return !!termInt; })) {
              // Yay, we found the integral!
              return new FunctionNode('mysum', termIntegrals);
            }
          }
        }
      }
    ]);
    var options = { rules: rules };

    compareSimplifiedStrings(math.integral("myfunc(a,x)", "x", options), "x*myfunc(a,x) - myfunc(a+1,x)");
    compareSimplifiedStrings(math.integral("myfunc(a,x) + 13", "x", options), "x*myfunc(a,x) - myfunc(a+1,x) + 13*x");
    compareSimplifiedStrings(math.integral("myfunc(a^2,5*x+99)", "x", options), "((5*x+99)*myfunc(a^2,5*x+99) - myfunc(a^2+1,5*x+99))/5");
    compareSimplifiedStrings(math.integral("sqrt(myfunc(a,x)) * sqrt(myfunc(a,x))", "x", options), "x*myfunc(a,x) - myfunc(a+1,x)");

    compareSimplifiedStrings(math.integral("mysum(3*x^2, x, 1/x, 1)", "x", options), "mysum(x^3, x^2/2, log(abs(x)), x)");
    compareSimplifiedStrings(math.integral("mysum(2*x, myfunc(a,x))", "x", options), "mysum(x^2, x*myfunc(a,x) - myfunc(a+1,x))");
  })

  it.skip('should not be too slow', function() {
    var math = getMathWithIntegral();

    // This test is 3-4 times slower with simplify set to true
    var options = { simplify: false };

    for(var i = 0; i < 250; i+=6) {
      math.integral("3 * +x * -x", "x", options);
      math.integral("(x + 1) * (x - 1)", "x", options);
      math.integral("8*x^3 + 3x^2 + 1", "x", options);
      math.integral("((1/8)*x + 1)^10", "x", options);
      math.integral("e^(2*x+1)", "x", options);
      math.integral("cos(2*x+1)", "x", options);
    }
  });

  it('should fail to find an integral when no integral exists', function() {
    var math = getMathWithIntegral();

    expect(math.integral.bind(null, "e^(x^2)", "x")).to.throw();
    expect(math.integral.bind(null, "log(log(x))", "x")).to.throw();
    expect(math.integral.bind(null, "1/log(x)", "x")).to.throw();
    expect(math.integral.bind(null, "e^x / x", "x")).to.throw();
    expect(math.integral.bind(null, "e^(e^x)", "x")).to.throw();
    expect(math.integral.bind(null, "sin(x^2)", "x")).to.throw();
    expect(math.integral.bind(null, "sin(x)/x", "x")).to.throw();
  });

});
