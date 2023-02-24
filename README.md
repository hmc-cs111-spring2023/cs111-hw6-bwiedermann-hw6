[![Open in Codespaces](https://classroom.github.com/assets/launch-codespace-f4981d0f882b2a3f0472912d15f9806d57e124e0fc890972558857b51b24a6f9.svg)](https://classroom.github.com/open-in-codespaces?assignment_repo_id=10264151)
# Internal Implementation Techniques

_You can work on the assignment on your own or with a partner. I recommend working with a
partner!_

In this assignment, we will use the techniques we have learned in class to add some
fluency to our "machine" library from the previous assignment. This additional syntactic
sugar will allow the user to more easily construct regular expressions.

For example, imagine a user wants to check whether a string `s` takes the form of a
letter, followed by a number (e.g., `c3`). Using the code from last week, the user would
need to write

```scala
matches(s, Concat(Character('c'), Character('3'))
```

After our work in this assignment, the user can instead write:

```scala
'c' ~ '3' matches s
```

or even:

```scala
"c3" matches s
```

We will also explore ways to more easily convert a regular expression to a DSL.

A key aspect of our work is that we can add all this fluency _without modifying any of the
code we wrote last week_! For that reason, this week's assignment depends on a solution to
last week's assignment, but it does not provide a way to modify that solution. Instead, we will
_extend_ it.

These are the parts of the assignment:

0. Set up your development environment.
1. Add an implicit conversion from a character to a regular expression.
2. Add an implicit conversion from a string to a regular expression.
3. Extend regular expressions, so that they define several operators.
4. Explore a technique for converting from a regular expression to a DFA.
5. Write a brief reflection of your work.
6. Commit your code, push it to Github, and submit it on Gradescope.
7. Provide feedback on another assignment.

## Overview

**Be sure to skim the whole document, before you start coding!**

You won't need to write a lot of code for this assignment, but you may need to think
carefully and experiment a bit. If you get stuck for too long, please ask for help!

## Resources

- Our class materials from the past couple of classes will be helpful. Check them out on
  the course website!
- The starter code depends on an implementation of last week's assignment. You will
  probably want to refer to the
  [documentation](https://hmc-cs111-spring2023.github.io/docs/scala-machines/) for that
  code. (You can also look at the
  [implementation](https://github.com/hmc-cs111-spring2023/machine-library), if that is
  helpful.)

Please also ask questions in class, on Discord, and in office hours!

## Part 0: Set up your development environment

There are (at least) three ways to work on the assignment:

1. In Codespaces
2. On your own machine, using Docker
3. On your own machine, using your own installation of Scala

Our first Scala assignment contains instructions for each of these options.

### Starter code

Once you have your repository, examine the starter code. There are only two files:

- `src/main/scala/machines/Internal.scala`: This is the file in which you will write all
  your implementation code.
- `src/main/scala/Program.scala`: This file contains a series of tests for regular expressions. Currently, all
  the regular expressions are defined using the case classes. As you work on the
  assignment, you will modify this file so that they take advantage of your nicer syntax.

## Part 1: `Char` ↦ `RegularLanguage`

### A. Define the implicit conversion

In `Internal.scala`, define an implicit conversion from `Char` to `RegularLanguage`.

### B. Test the implicit conversion

After defining the implicit conversion, you can test it out in the Scala REPL. Our
implicit conversion _should_ allow us to write something like:

```scala
'1' matches "1"
```

which should result in `true`.

Let's try it!

1. Run `sbt console` (or, if you already have `sbt` started, you can run the command
   `console`).
1. Type `'1' matches "1"`.
1. You should get an error, which claims that `matches` is not a member of `Char`, even
   though we have defined an implicit conversion!

The issue is that Scala requires us to _explicitly_ ask to use the implicit conversions we
want. Here's how we can do that:

4. Still in the Scala REPL, type `import machines.given`. This line will import into the
   console the implicit conversions that we defined.
5. Now, try again to use our implicit conversion: `'1' matches "1"`.

If we have properly defined the implicit conversion, we should get the result `true`. Try
a few more examples, just to be sure.

### C. Modify `Program.scala` to use the implicit conversion

Now that we are sure that our implicit conversion works, modify
`src/main/scala/Program.scala` to take advantage of it. Here's how:

1. Run the command `sbt run` (or just `run` if `sbt` is already up and running). This
   command runs the `main` function in `Program.scala`. You should see the message `All
the tests passed!`. (You may also see some warnings, which you can ignore.)
1. Now, modify `Program.scala`, under `Part 1`, so that it uses your implicit conversion.
   Remember to also `import machines.given`!
1. `sbt run` again, and you should get the same message. We haven't changed anything
   significant about _what_ the program does, but we have rewritten part of it so that the
   patterns are a little bit clearer.

## Part 2: `String` ↦ `RegularLanguage`

1. Modify `Internal.scala` to implement the implicit conversion.
2. Modify `Program.scala`, under `Part 2`, to use the new conversion.

Some hints:

- There are multiple ways to turn a string into a regular expression, but ultimately, all
  ways need to construct the regular expression by concatenating the individual characters
  of the string, in the right order.
- Options include recursion, `map` + [`reduce`](https://allaboutscala.com/tutorials/chapter-8-beginner-tutorial-using-scala-collection-functions/scala-reduce-example/), or
  [`foldRight`](https://allaboutscala.com/tutorials/chapter-8-beginner-tutorial-using-scala-collection-functions/scala-foldright-example/),
  among others.

## Part 3: Adding operators to `RegularLanguage`

In `Internal.scala`, implement the following operators:

A. implement the binary operator `||`, which corresponds to the union operation

B. implement the binary operator `~`, which corresponds to the concatenation
operation

C. implement the postfix operator `<*>`, which corresponds to the Kleene star operation

D. implement the postfix operator `<+>`, which means "one or more repetitions of the
preceding pattern"

E. implement the repetition operator `{n}` which means "`n` repetitions of the
preceding pattern"

Some hints:

- Remember that each operator should result in a new `RegularLanguage`.
- When using the extensions, we need to import them. Use `import machines._`.
- $r^+$ is the same as $r · (r^*)$.
- The repetition operator is probably the trickiest. Use the name `apply` for your
  operator. This operator adds "function call" capability. This capability, together with
  the option to use curly braces when calling single-argument functions, should give us
  what we want.

## Part 4: `RegularLanguage` ↦ `DFA`

You may remember from the previous assignment that there is a function named
[`regexToDFA`](https://hmc-cs111-spring2023.github.io/docs/scala-machines/machines.html#regexToDFA-fffff1a7)
that takes as input a `RegularLanguage` and an alphabet (i.e., a set of `Char`s) and
results in a DFA that is equivalent to the regular language.

It would be nice if we could add an implicit conversion from `RegularLanguage` to `DFA`,
which makes use of this function. The problem is that `regexToDFA` also needs an alphabet,
to do its work. The `RegularLanguage` type does not explicitly store any information about
the language's alphabet. If we want to implicitly convert from a regular language to a
DFA, we will need to make the alphabet explicit in some way. We have at least two options:

1. Given a regular language, extract the set of characters that appear in the language, and
   use that set as the alphabet.
1. Use some Scala features that support "contextual abstractions" -- essentially, ways to
   work with implicit values.

Let's do both, but start with the second approach, mainly because it offers us a chance to
learn something new :).

### Contextual parameters

First, read this [brief
article](https://docs.scala-lang.org/scala3/book/ca-given-using-clauses.html) about the
Scala feature we will take advantage of, named _contextual parameters_.

Now, extend `RegularLanguage` with a method named `toDFA`, which employs a
contextual parameter for the alphabet. After this extension, we should be able to use it
in the scala REPL like so:

```scala
scala> import machines.{_, given}        // import all our extensions

scala> import dfa.writeFile              // import a function for making pictures from DFAs

scala> given Set[Char] = Set('0', '1')   // set our alphabet to be bitvalues
lazy val given_Set_Char: Set[Char]

scala> writeFile( ("01" <*>) toDFA )     // a picture will be in output.dot.png
```

It might be a little unsatisfying that the user must explicitly call `toDFA`, and we
haven't implemented a true implicit conversion. Unfortunately, I haven't come across any
way that Scala allows us to use a context parameter in an implicit conversion. If you find
a way, please let everyone know!

There is a way that we can create alphabet-specific implicit conversions (i.e., one
conversion for each kind of alphabet). If you are interested, ask me about it!

But let us now finish our extensions by implementing a true implicit conversion

### Implicit conversion

In the same file, implement an implicit conversion from `RegularLanguage` to `DFA`.

I recommend writing a helper function in `Internal.scala` that extracts characters from a regular language, in
the following way:

$$
\begin{align*}
\text{chars}(\emptyset) &= \emptyset \\
\text{chars}(\epsilon) &= \emptyset \\
\text{chars}(c) &= \{c\} \\
\text{chars}(r_1 \cup r_2) &= \text{chars}(r_1) \cup \text{chars}(r_2)\\
\text{chars}(r_1 \cdot r_2) &= \text{chars}(r_1) \cup \text{chars}(r_2)\\
\text{chars}(r^*) &= \text{chars}(r)
\end{align*}
$$

From there, you can do one of the following:

- pass the appropriate arguments to `regextoDFA` _or_
- call the extension you just wrote, with `using`.

After it is implemented, you can test it in the scala REPL like so:

```scala
scala> import machines.{_, given}        // import all our extensions

scala> import dfa.writeFile              // import a function for making pictures from DFAs

scala> writeFile(('a' <*>) ~ 'b')        // a picture will be in output.dot.png
```

Wow! You have just added a lot of fluency to this library!

## Part 5: Reflection

In the file `reflection.md`, write about:

- Your process of implementing the internal DSL for regular languages
- Your thoughts about the design of this DSL

I highly recommend that you **work on this part as you go.** In other words,
treat it as a running diary. Take a moment now to look at the questions in that
file, then update the file as you work on the assignment. Although you might
treat the file as a running diary, the version you submit should be clear and concise.

## Commit, push, and submit to Gradescope

Be sure to remember to commit and push your work to GitHub, and submit on Gradescope! No
matter where you are working (including Codespaces), you will need to push to GitHub, just
as you have for previous assignments.

## Peer-review other people's work

After the submission deadline, I will post critique partners. Each person will add
feedback to someone else's work; though that might mean that some people receive multiple
critiques. You should look through their solutions to each of the parts, and comment on
the clarity of their code. You don't need to comment on everything, but find some
interesting parts of their work to evaluate. Some questions you might consider / answer
when providing feedback:

- What do you like about their implementation?
- Is there anything that could be done to improve their implementation?
- Did they implement things in the same or in different ways as you?
- With which aspects of their reflection do you agree? With which aspects do
  you disagree or just have a different perspective?
