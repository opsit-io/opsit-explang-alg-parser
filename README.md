Explang Algebraic Syntax Parser
===============================

*Algebraic syntax parser for [Explang](https://github.com/opsit-io/opsit-explang-core) -
a simple and customizable dynamic language for the Java platform*.

This project strives to provide a simple Julia-like syntax for Explang
with a number of nice features for casual programming, usage as 
a query language for information retrieval, etc:

* Support for JSON compatible syntax to define data objects (Maps and Lists),
* Support for shell/splunk like pipe syntax to build data processing and filtering pipelines.
* Support for string interpolation using variables and any function calls.

This plug-in is still very much work in progress.

Dependencies
------------

This plugin depends on [Explang Core](https://github.com/opsit-io/opsit-explang-core)
and the [ANTLR](https://www.antlr.org) framework

Language Documentation
----------------------

See [Explang Language Documentation](https://github.com/opsit-io/opsit-explang-docs):

- [Language Overview](TBD)
- [Language Functions Reference](TBD)
- [javadoc](TBD) for the *explang-core* module
- [javadoc](TBD) for the *explang-alg-parser* module

Code Examples
-------------

- See [Sample code](examples/)

Installation
------------

Download executable JAR jars from Github 
[releases](https://github.com/opsit-io/opsit-explang-alg-parser/releases)


Or use maven CLI to fetch the artifact from maven central:

```
mvn org.apache.maven.plugins:maven-dependency-plugin:2.8:get -Dartifact=io.opsit:opsit-explang-alg-parser:0.0.3:jar:runnable   -Dtransitive=false -Ddest=opsit-explang-alg-parser-0.0.3-runnable.jar
```

Using REPL
----------

Explang-core contains built-in REPL. 

```
$ java -jar opsit-explang-alg-parser-0.0.3-runnable.jar
Welcome to the EXPLANG REPL!
Active parser is AlgParser
Loaded packages are: [base.math, base.text, io, base.bindings, ffi, base.funcs, loops, threads, base.version, base.coercion, base.logic, base.lang, base.arithmetics, base.seq, base.control, base.regex, dwim, base.docs, base.beans, base.types]
Please type an EXPLANG expression terminated by an extra NEWLINE
[0]>  print("Hello, world!\n");
Hello, world!

=> Hello, world!

[1]>
```

This built-in REPL implementation does not support editing of and command history, so one is 
may want to it with some kind of wrapper such as [rlwrap](https://github.com/hanslub42/rlwrap),
VS Code [repeater REPL extension](https://github.com/RegisMelgaco/repeater--repl-tool), 
[Emacs inferior lisp mode](https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#External-Lisp).

There is a REPL implementation with editing support in the separate project 
[Explang JLine REPL](https://github.com/opsit-io/opsit-explang-jline-repl).


Executing Explang Scripts
-------------------------

```shell
$ java -jar opsit-explang-alg-parser-0.0.3-runnable.jar ./examples/hello.jl
Hello world
```

Quick Start Guide to Using Explang with Alg Parser from Java Code
-----------------------------------------------------------------

### Add to the dependencies listin *pom.xml*


```xml
<dependencies>
  <dependency>
    <groupId>io.opsit</groupId>
    <artifactId>opsit-explang-alg-parser</artifactId>
    <version>0.0.3</version>
  </dependency>
...
</dependencies>

```

### Parse, compile and evaluate Explang expressions from Java code


```java

import io.opsit.explang.ASTNList;
import io.opsit.explang.Compiler;
import io.opsit.explang.ICompiled;
import io.opsit.explang.IParser;
import io.opsit.explang.ParseCtx;
import io.opsit.explang.parser.alg.AlgParser;
import java.util.HashMap;
import java.util.Map;

...

// code: add two variables
String code = "(+ a b)";
// map with variable values
Map<String,Object> vars = new HashMap<String,Object>();
vars.put("a", 1);
vars.put("b", 2.5);

// Create compiler
Compiler compiler = new Compiler();
// Create a parser and associate it with the compiler
IParser parser = new AlgParser();
ParseCtx pctx = new ParseCtx("mycode");
compiler.setParser(parser);
    
// parse code into Abstract Syntax Tree
ASTNList exprs = parser.parse(pctx, code);

// Check that an expression was parsed without errors.
if (exprs.hasProblems()) {
  System.err.println("Parse errors: " + exprs.getProblem());
} else if (exprs.isEmpty()) {
  System.out.println("No expressions were parsed");
} else {
  // Compile an expression into reusable form that can be executed
  ICompiled exp = compiler.compile(exprs.get(0));
  // Create context with user data
  Compiler.ICtx ctx = compiler.newCtx(vars);
  // execute compiled code in given context;
  Object expVal = exp.evaluate(compiler.newBacktrace(), ctx);
  System.out.println("Result: " + expVal);
}

```

Licenses
--------

Explang is licensed under the [GNU AFFERO GENERAL PUBLIC LICENSE](LICENSE).
